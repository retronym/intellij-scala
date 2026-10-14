package org.jetbrains.plugins.scala.tcHarness

import com.google.gson.JsonObject
import com.intellij.testFramework.EdtTestUtil
import com.intellij.util.ThrowableRunnable
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import junit.framework.TestCase
import org.jetbrains.plugins.scala.tcHarness.TcProtocol.*

import java.io.{PrintWriter, StringWriter}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.{CompletableFuture, Executors, LinkedBlockingQueue}
import scala.util.Try

/**
 * The type-checking harness: a long-lived "test" that keeps an IntelliJ light project warm and
 * answers type-checking requests over HTTP, with scalac as the oracle. See README.md here.
 *
 * Env: `TC_PORT` (default 8723), `TC_LABEL` (echoed in responses, e.g. `tip` / `base`).
 * Runs until `POST /shutdown`.
 */
class TcServerTest extends TestCase {

  private sealed trait Job
  private final case class Check(body: String, result: CompletableFuture[String]) extends Job
  private case object Shutdown extends Job

  private val jobs = new LinkedBlockingQueue[Job]
  private val pool = Executors.newCachedThreadPool()
  // the compilers are single-threaded (Scala 3 asserts that a ContextBase stays on one thread)
  private val oracleThread = Executors.newSingleThreadExecutor()
  private var worker: TcWorker = null
  private var generation = 0

  private val label = sys.env.getOrElse("TC_LABEL", "")
  /** The commit under test: the newest one that is not part of the harness. */
  private lazy val commit: String =
    Try(scala.sys.process.Process(Seq("git", "log", "-1", "--format=%h", "--invert-grep", "--grep=type-checking harness")).!!.trim).getOrElse("?")

  def testServe(): Unit = {
    // production behaviour where the tree has these knobs (unit-test mode makes some checks fatal)
    System.setProperty("scala.types.substitutorInvariants", sys.env.getOrElse("TC_INVARIANTS", "off"))
    System.setProperty("scala.types.recursionGuard.failHard", "false")

    val port = sys.env.getOrElse("TC_PORT", "8723").toInt
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", port), 0)
    server.setExecutor(pool)
    server.createContext("/health", ex => reply(ex, 200, render(obj("ok" -> bool(true), "label" -> str(label), "commit" -> str(commit),
      "env" -> Option(worker).map(w => envJson(w.env)).getOrElse(com.google.gson.JsonNull.INSTANCE)))))
    server.createContext("/shutdown", ex => { reply(ex, 200, "{\"ok\":true}"); jobs.put(Shutdown) })
    server.createContext("/check", ex => {
      val body = new String(ex.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
      val result = new CompletableFuture[String]
      jobs.put(Check(body, result))
      Try(result.get()).fold(e => reply(ex, 500, render(obj("error" -> str(stackTrace(e))))), reply(ex, 200, _))
    })
    server.start()
    println(s"TC-HARNESS listening on 127.0.0.1:$port label=$label commit=$commit")
    try {
      var running = true
      while (running) jobs.take() match {
        case Shutdown => running = false
        case Check(body, result) =>
          try result.complete(render(handle(parseRequest(body))))
          catch { case e: Throwable =>
            println(s"TC-HARNESS request failed: ${stackTrace(e).linesIterator.take(8).mkString("\n  ")}")
            result.completeExceptionally(e)
          }
      }
    } finally {
      server.stop(0)
      if (worker != null) Try(EdtTestUtil.runInEdtAndWait((() => worker.stop()): ThrowableRunnable[Throwable]))
      pool.shutdownNow()
      oracleThread.shutdownNow()
    }
  }

  private def handle(req0: Request): JsonObject = {
    val (aliasOf, synthetic) = aliased(req0.queries.flatMap(operands))
    val queries = req0.queries.map {
      case Query.Conforms(a, b) => Query.Conforms(aliasOf(a), aliasOf(b))
      case Query.Equiv(a, b)    => Query.Equiv(aliasOf(a), aliasOf(b))
      case Query.BaseType(a, c) => Query.BaseType(aliasOf(a), aliasOf(c))
      case Query.TypeOf(a)      => Query.TypeOf(aliasOf(a))
    }
    val req = req0.copy(files = req0.files ++ synthetic, queries = queries)
    def user(ds: Seq[Diagnostic]) = ds.filterNot(_.file.startsWith(TcWorker.SyntheticPrefix))
    val version = TcWorker.parseVersion(req.env.scalaVersion)
    val scalacOptions = req.env.scalacOptions ++ (if (req.env.jdk == "mock") Nil else Seq("-release", req.env.jdk))
    val oracle: Option[CompletableFuture[(TcOracle#Result, Long)]] = Option.when(req.oracle)(CompletableFuture.supplyAsync(() => {
      val o = TcOracle.get(version, req.env.classpath, scalacOptions)
      val t0 = System.nanoTime()
      val (files, javaOut) = TcOracle.withJavac(req.files, req.env.classpath, req.env.jdk)
      val r = o.run(files, req.queries.map(_.encode), javaOut.fold("")(_.toString))
      (r, (System.nanoTime() - t0) / 1000000)
    }, oracleThread))

    var setupMs = 0L
    if (worker == null || worker.env != req.env || req.fresh) {
      val t0 = System.nanoTime()
      if (worker != null) {
        val old = worker
        worker = null
        Try(EdtTestUtil.runInEdtAndWait((() => old.stop()): ThrowableRunnable[Throwable])).failed.foreach(e => println(s"TC-HARNESS tearDown: $e"))
      }
      if (req.fresh) generation += 1
      val w = new TcWorker(req.env, generation)
      EdtTestUtil.runInEdtAndWait((() => w.start()): ThrowableRunnable[Throwable])
      worker = w
      setupMs = (System.nanoTime() - t0) / 1000000
    }

    val t0 = System.nanoTime()
    var ij: TcWorker.Result = null
    EdtTestUtil.runInEdtAndWait((() => ij = worker.check(req)): ThrowableRunnable[Throwable])
    val ijMs = (System.nanoTime() - t0) / 1000000
    val TcWorker.Result(ijDiags, ijAnswers, ijCanonical, logged) = ij

    val response = obj(
      "server" -> obj("label" -> str(label), "commit" -> str(commit)),
      "env" -> envJson(req.env),
      "setupMs" -> num(setupMs),
      "intellij" -> obj("ms" -> num(ijMs), "diagnostics" -> arr(ijDiags.map(_.toJson)), "loggedErrors" -> arr(logged.map(str)))
    )
    val scalac = oracle.map(_.get())
    scalac.foreach { case (r, ms) =>
      response.add("scalac", obj("version" -> str(version.minor), "ms" -> num(ms), "diagnostics" -> arr(user(r.diagnostics).map(_.toJson)), "crash" -> str(r.crash.orNull)))
      // errors are matched by file and line
      def key(d: Diagnostic) = (d.file, d.line)
      val ijErr = ijDiags.filter(_.severity == "error")
      val scErr = user(r.diagnostics).filter(_.severity == "error")
      val (ijKeys, scKeys) = (ijErr.map(key).toSet, scErr.map(key).toSet)
      val ijOnly = ijErr.filterNot(d => scKeys(key(d)))
      val scOnly = scErr.filterNot(d => ijKeys(key(d)))
      response.add("diff", obj("agree" -> bool(ijOnly.isEmpty && scOnly.isEmpty && r.crash.isEmpty),
        "intellijOnly" -> arr(ijOnly.map(_.toJson)), "scalacOnly" -> arr(scOnly.map(_.toJson))))
    }
    if (req.queries.nonEmpty) {
      val scAnswers = scalac.map(_._1.answers).filter(_.size == req.queries.size)
      val sameType = if (scAnswers.isEmpty) Map.empty[Int, String] else typeChecks(req0, ijCanonical, version, scalacOptions)
      response.add("queries", arr(req0.queries.zipWithIndex.map { case (q, i) =>
        val o = q.toJson
        o.add("intellij", str(ijAnswers(i)))
        scAnswers.foreach { sc =>
          o.add("scalac", str(sc(i)))
          sameType.get(i).foreach(e => o.add("scalacSameType", str(e)))
          val agree = sameType.get(i) match {
            case Some("true")  => true
            case Some("false") => false
            case _             => normalize(ijAnswers(i)) == normalize(sc(i))
          }
          o.add("agree", bool(agree))
        }
        o
      }))
    }
    response
  }

  private def operands(q: Query): Seq[String] = q match {
    case Query.Conforms(a, b) => Seq(a, b)
    case Query.Equiv(a, b)    => Seq(a, b)
    case Query.BaseType(a, c) => Seq(a, c)
    case Query.TypeOf(a)      => Seq(a)
  }

  /**
   * Each operand also gets an alias in a synthetic file, so that an operand may be a type
   * expression (`List[Int]`, `Main.o.T`) as well as the name of a definition in the sources.
   */
  private def aliased(ops: Seq[String]): (Map[String, String], Option[SourceFile]) = {
    val distinct = ops.distinct
    val aliasOf = distinct.zipWithIndex.map { case (op, i) => op -> s"$op\u001f__tc$i" }.toMap
    val synthetic = Option.when(distinct.nonEmpty)(SourceFile(s"${TcWorker.SyntheticPrefix}queries.scala",
      distinct.zipWithIndex.map { case (op, i) => s"  type __q___tc$i = $op" }.mkString("object __tc {\n", "\n", "\n}\n")))
    (aliasOf, synthetic)
  }

  /**
   * Printed types differ between the engines, so for `type` and `baseType` answers scalac also
   * checks its own type against IntelliJ's (canonical text, as a type expression): `true`, `false`,
   * or `exc ...` when that text doesn't type there (a this-type, a local).
   */
  private def typeChecks(req: Request, canonical: Seq[Option[String]], version: org.jetbrains.plugins.scala.ScalaVersion,
                         scalacOptions: Seq[String]): Map[Int, String] = {
    val checks = req.queries.zip(canonical).zipWithIndex.collect {
      case ((q: Query.TypeOf, Some(c)), i)   => (i, q, c)
      case ((q: Query.BaseType, Some(c)), i) => (i, q, c)
    }
    def run(cs: Seq[(Int, Query, String)]): Seq[String] = {
      val (aliasOf, synthetic) = aliased(cs.flatMap { case (_, q, c) => operands(q) :+ c })
      val encoded = "N" +: cs.map {
        case (_, Query.TypeOf(a), c)      => s"TE\t${aliasOf(a)}\t${aliasOf(c)}"
        case (_, Query.BaseType(a, k), c) => s"BE\t${aliasOf(a)}\t${aliasOf(k)}\t${aliasOf(c)}"
        case other                        => throw new IllegalStateException(other.toString)
      }
      CompletableFuture.supplyAsync(() =>
      {
        val (files, javaOut) = TcOracle.withJavac(req.files, req.env.classpath, req.env.jdk)
        TcOracle.get(version, req.env.classpath, scalacOptions).run(files ++ synthetic, encoded, javaOut.fold("")(_.toString)).answers
      }, oracleThread).get()
    }
    if (checks.isEmpty) return Map.empty
    // a text that doesn't parse (e.g. Scala 3 syntax in a Scala 2 program) stops the whole run: then one run per check
    val together = run(checks)
    val answers =
      if (together.size == checks.size && !together.exists(_.startsWith("exc not typed"))) together
      else checks.flatMap(c => run(Seq(c)).headOption.orElse(Some("exc")))
    checks.map(_._1).zip(answers).toMap
  }

  /** Printed types differ between the engines in package prefixes and spacing. */
  private def normalize(answer: String): String =
    answer.replace("_root_.", "").replaceAll("\\b(?:scala|java)(?:\\.[a-z]\\w*)*\\.(?=[A-Z])", "").replace(" ", "")

  private def envJson(e: Env): JsonObject = obj("scalaVersion" -> str(e.scalaVersion), "jdk" -> str(e.jdk),
    "classpath" -> arr(e.classpath.map(str)), "scalacOptions" -> arr(e.scalacOptions.map(str)))

  private def reply(ex: HttpExchange, code: Int, body: String): Unit = {
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    ex.getResponseHeaders.add("Content-Type", "application/json; charset=utf-8")
    ex.sendResponseHeaders(code, bytes.length)
    ex.getResponseBody.write(bytes)
    ex.close()
  }

  private def stackTrace(e: Throwable): String = {
    val sw = new StringWriter
    Option(e.getCause).filter(_ => e.isInstanceOf[java.util.concurrent.ExecutionException]).getOrElse(e).printStackTrace(new PrintWriter(sw))
    sw.toString
  }
}
