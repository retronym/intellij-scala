package org.jetbrains.plugins.scala.lang.typePbt

import org.jetbrains.plugins.scala.{ScalaFileType, ScalaVersion}
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.types.{BaseTypes, ScType, ScTypeExt}
import org.jetbrains.plugins.scala.lang.typePbt.Ast.Program
import org.junit.Assert

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Property-based differential test of the plugin's type operations against scalac 2.13.
 * See PLAN.md in this package.
 *
 * Generates cake-shaped programs (`Gen`), asks scalac (`ScalacOracle`) and the plugin the
 * same questions about the types in them, shrinks each disagreement (`Shrink`), and
 * classifies it as a known issue or not (`Classify`).
 *
 * Knobs (system properties): `scala.pbt.seed` (default 1), `scala.pbt.count` (programs,
 * default 30), `scala.pbt.report` (write the report there too), `scala.pbt.failOnUnknown`.
 */
class TypePbtTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_2_13)
  override protected def supportedIn(version: ScalaVersion): Boolean = version == ScalaVersion.Latest.Scala_2_13

  private def prop(name: String, default: String): String = Option(System.getProperty(s"scala.pbt.$name")).orElse(Option(System.getenv(s"SCALA_PBT_${name.toUpperCase}"))).getOrElse(default)

  private lazy val oracle = ScalacOracle.get(ScalaVersion.Latest.Scala_2_13)
  private var programCounter = 0

  /** Answers to `questions`, or `Left(errors)` if scalac rejects the program. */
  private final case class Answers(scalac: Seq[String], plugin: Seq[String], findings: Seq[Finding])

  private def runCase(c: Case): Either[Seq[String], Answers] = {
    programCounter += 1
    val source = Ast.show(c.program, s"__pbt$programCounter")
    oracle.ask(source, c.questions.map(_.encode)) match {
      case Left(errors) => Left(errors)
      case Right(scalacAnswers) =>
        val (pluginAnswers, extra) = askPlugin(source, c.questions)
        val findings = ArrayBuffer.empty[Finding]
        c.questions.zip(scalacAnswers).zip(pluginAnswers).foreach { case ((q, s), p) =>
          if (!s.startsWith("exc")) {
            if (p.startsWith("exc")) findings += Finding("pluginException", q, s, p, c.program)
            else if (p.startsWith("unresolved")) findings += Finding("pluginUnresolved", q, s, p, c.program)
            else q match {
              case _: Question.Conforms if s != p => findings += Finding("conforms", q, s, p, c.program)
              case _: Question.Equiv if s != p    => findings += Finding("equiv", q, s, p, c.program)
              case _: Question.BaseType if s.takeWhile(_ != '\t') != p.takeWhile(_ != '\t') =>
                findings += Finding("baseType", q, s, p, c.program)
              case _ =>
            }
          }
        }
        findings ++= extra.map(_.copy(program = c.program))
        Right(Answers(scalacAnswers, pluginAnswers, findings.toSeq))
    }
  }

  /** The plugin's answers, plus findings of properties checked within the plugin alone. */
  private def askPlugin(source: String, questions: Seq[Question]): (Seq[String], Seq[Finding]) = {
    configureFromFileText(ScalaFileType.INSTANCE, source)
    val file = getFile.asInstanceOf[ScalaFile]
    val aliases: Map[String, ScTypeAliasDefinition] = file.depthFirst().collect {
      case ta: ScTypeAliasDefinition if ta.name.startsWith("__q_") => ta.name.stripPrefix("__q_") -> ta
    }.toMap
    val types = mutable.Map.empty[String, Either[String, ScType]]
    def tpe(id: String): Either[String, ScType] = types.getOrElseUpdate(id,
      aliases.get(id) match {
        case None     => Left("unresolved\tno alias")
        case Some(ta) => ta.aliasedType.left.map(f => "unresolved\t" + f.toString)
      })
    val extra = ArrayBuffer.empty[Finding]
    val answers = questions.map { q =>
      try {
        val ts = q.ids.map(tpe)
        ts.collectFirst { case Left(err) => err }.getOrElse {
          val List(a, b) = ts.map(_.toOption.get)
          q match {
            case _: Question.Conforms => a.conforms(b).toString
            case _: Question.Equiv    => a.equiv(b).toString
            case _: Question.BaseType =>
              b.extractClass match {
                case None => "unresolved\tno class"
                case Some(cls) =>
                  BaseTypes.baseType(a, cls) match {
                    case None => "none"
                    case Some(bt) =>
                      if (!a.conforms(bt))
                        extra += Finding("baseTypeIsSuper", q, "true", s"false\t${bt.canonicalText}", null)
                      "some\t" + bt.canonicalText
                  }
              }
          }
        }
      } catch {
        case e: Throwable if !e.isInstanceOf[com.intellij.openapi.progress.ProcessCanceledException] =>
          "exc\t" + String.valueOf(e).replace('\n', ' ')
      }
    }
    (answers, extra.toSeq)
  }

  /** Shrinks `f` while scalac accepts the program and the same disagreement reproduces. */
  private def shrink(f: Finding, budget: Int): Finding = {
    def same(g: Finding): Boolean = g.check == f.check && g.question == f.question && g.direction == f.direction
    var current = f.program
    var steps = 0
    var progress = true
    val keep = f.question.ids.toSet
    // only the question at hand
    current = dropOtherQueries(current, keep)
    if (reproduces(current, f.question).forall(!same(_))) current = f.program
    while (progress && steps < budget) {
      progress = false
      val it = Shrink.program(current, keep).iterator
      while (!progress && it.hasNext && steps < budget) {
        val candidate = it.next()
        steps += 1
        if (reproduces(candidate, f.question).exists(same)) { current = candidate; progress = true }
      }
    }
    reproduces(current, f.question).find(same).getOrElse(f.copy(program = current))
  }

  private def reproduces(p: Program, q: Question): Seq[Finding] =
    runCase(Case(p, List(q))).toOption.map(_.findings).getOrElse(Nil)

  private def dropOtherQueries(p: Program, keep: Set[String]): Program = {
    def ms(xs: List[Ast.Member]): List[Ast.Member] = xs.flatMap {
      case Ast.Query(id, _) if !keep(id) => Nil
      case Ast.ClassMem(c)               => List(Ast.ClassMem(c.copy(members = ms(c.members))))
      case m                             => List(m)
    }
    Program(ms(p.members))
  }

  def testDifferential(): Unit = {
    val seed = prop("seed", "1").toLong
    val count = prop("count", "30").toInt
    val shrinkBudget = prop("shrinkBudget", "400").toInt
    val report = ArrayBuffer.empty[String]
    var discarded = 0
    var asked = 0
    val discardReasons = mutable.Map.empty[String, Int].withDefaultValue(0)
    val discardSamples = mutable.LinkedHashMap.empty[String, (Seq[String], Program)]
    val answerStats = mutable.Map.empty[String, Int].withDefaultValue(0)
    val raw = ArrayBuffer.empty[(Long, Finding)]

    for (i <- 0 until count) {
      val programSeed = seed * 1000003L + i
      val c = new Gen(new Random(programSeed)).generate()
      runCase(c) match {
        case Left(errors) =>
          discarded += 1
          val reason = errors.headOption.map(_.replaceAll("^\\d+: ", "").replaceAll("\\b(T|K|I|M|v|a|q|c)\\d+\\w*", "_").take(60)).getOrElse("?")
          discardReasons(reason) += 1
          if (!discardSamples.contains(reason)) discardSamples(reason) = (errors, c.program)
        case Right(a) =>
          asked += c.questions.size
          c.questions.zip(a.scalac).foreach { case (q, s) => answerStats(q.getClass.getSimpleName + ":" + s.takeWhile(_ != '\t')) += 1 }
          a.findings.foreach(f => raw += programSeed -> f)
      }
    }

    // shrink one finding per (check, question) per program, then group by signature
    val shrunk = raw.distinctBy { case (s, f) => (s, f.check, f.question) }.map { case (s, f) => s -> shrink(f, shrinkBudget) }
    val bySignature = shrunk.groupBy(_._2.signature).toSeq.sortBy(-_._2.size)
    var unknown = 0
    report += s"# TypePbtTest seed=$seed count=$count"
    report += s"programs: ${count - discarded} typed by scalac, $discarded discarded; questions asked: $asked"
    report += s"scalac answers: ${answerStats.toSeq.sorted.map { case (k, v) => s"$k=$v" }.mkString(", ")}"
    report += s"discard reasons: ${discardReasons.toSeq.sortBy(-_._2).take(12).map { case (k, v) => s"$v× $k" }.mkString("; ")}"
    report += s"findings: ${raw.size} raw, ${shrunk.size} shrunk, ${bySignature.size} signatures"
    val byIssue = shrunk.groupBy(x => Classify.classify(x._2).fold("UNKNOWN")(_.id)).view.mapValues(_.size).toSeq.sortBy(-_._2)
    report += s"classified: ${byIssue.map { case (k, v) => s"$k=$v" }.mkString(", ")}"
    for ((sig, fs) <- bySignature) {
      val (s, f) = fs.minBy(x => Ast.show(x._2.program, "p").length)
      val issue = Classify.classify(f)
      if (issue.isEmpty) unknown += fs.size
      report += ""
      report += s"## ${issue.fold("UNKNOWN")(i => s"known: ${i.id}")} — $sig (${fs.size}×)"
      report += s"seed $s: ${f.question.show}: scalac=${f.scalac.replace('\t', ' ')} plugin=${f.plugin.replace('\t', ' ')}"
      report += "```scala"
      report += Ast.show(f.program, "p")
      report += "```"
    }
    if (prop("showDiscards", "false").toBoolean) for ((reason, (errors, p)) <- discardSamples) {
      report += ""
      report += s"## discarded: $reason"
      report ++= errors
      report += "```scala"
      report += Ast.show(p, "p")
      report += "```"
    }
    val text = report.mkString("\n")
    println(text)
    Option(prop("report", null)).foreach(path => Files.writeString(Paths.get(path), text))
    if (prop("failOnUnknown", "false").toBoolean && unknown > 0) Assert.fail(s"$unknown unknown finding(s)")
  }
}
