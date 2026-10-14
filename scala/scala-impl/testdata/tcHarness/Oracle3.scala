package tcoracle

import dotty.tools.dotc
import dotc.ast.tpd
import dotc.ast.tpd.TreeOps
import dotc.core.Contexts.*
import dotc.core.Symbols.*
import dotc.core.Types.*
import dotc.core.Flags
import dotc.reporting.StoreReporter
import dotc.util.SourceFile

import scala.collection.mutable

/**
 * The Scala 3 side of the type-checking harness (`TcOracle`); the protocol is `Oracle2`'s.
 * Each compile gets a fresh compiler and `ContextBase` (see `Driver`).
 */
class Oracle3(classpath: String, options: Array[String]) extends java.util.function.Function[Array[String], Array[String]] {

  // A fresh ContextBase per request: runs of one compiler share the symbol table, so top-level
  // definitions of earlier requests would stay visible (as in the REPL). The classpath index is
  // kept across ContextBases by dotty's own caches.
  private final class Driver extends dotc.Driver {
    override protected def sourcesRequired: Boolean = false
    val rootContext: Context = {
      val args = Array("-classpath", requestClasspath, "-Ystop-after:refchecks", "-color:never") ++ options
      setup(args, initCtx.fresh) match {
        case Some((_, ctx)) => ctx
        case None           => throw new IllegalArgumentException("bad scalacOptions: " + options.mkString(" "))
      }
    }
    val compiler: dotc.Compiler = newCompiler(using rootContext)
  }

  private def clean(s: String) = String.valueOf(s).replace('\t', ' ').replace('\n', ' ')

  // the request's own extra classpath (e.g. its Java sources compiled by javac); one request at a time
  private var requestClasspath = classpath

  def apply(in: Array[String]): Array[String] = {
    requestClasspath = if (in(0).isEmpty) classpath else classpath + java.io.File.pathSeparator + in(0)
    val n = in(1).toInt
    val sources = (0 until n).map(i => (in(2 + 2 * i), in(3 + 2 * i)))
    val queries = in.drop(2 + 2 * n)
    try answer(sources, queries)
    catch { case t: Throwable => Array("X\t" + clean(t.toString)) }
  }

  // As in Oracle2: diagnostics from a run without the query aliases (`__tc_*` files).
  private def answer(sources: Seq[(String, String)], queries: Array[String]): Array[String] = {
    val user = sources.filterNot(_._1.startsWith("__tc_"))
    if (queries.isEmpty) compile(user, Array.empty)
    else if (queries.head == "N") compile(sources, queries.tail).filter(_.startsWith("A\t"))
    else compile(user, Array.empty) ++ compile(sources, queries).filter(_.startsWith("A\t"))
  }

  private def compile(sources: Seq[(String, String)], queries: Array[String]): Array[String] = {
    val driver = new Driver
    val reporter = new StoreReporter(null)
    given ctx0: Context = driver.rootContext.fresh.setReporter(reporter)
    val run = driver.compiler.newRun
    run.compileSources(sources.map { case (p, t) => SourceFile.virtual(p, t) }.toList)
    val all = reporter.removeBufferedMessages
    val diags = all.filter(d => d.level >= dotc.interfaces.Diagnostic.WARNING).map { d =>
      val sev = if (d.level == dotc.interfaces.Diagnostic.ERROR) "error" else "warning"
      val (path, line, col) = if (d.pos.exists) (d.pos.source.path, d.pos.line + 1, d.pos.column + 1) else ("", 0, 0)
      s"D\t$sev\t$path\t$line\t$col\t${clean(d.message)}"
    }
    (diags ++ answerQueries(run, queries)(using run.runContext.withPhase(run.runContext.base.typerPhase))).toArray
  }

  private def answerQueries(run: dotc.Run, queries: Array[String])(using Context): Seq[String] = {
    val aliases = mutable.LinkedHashMap[String, Symbol]()
    val terms = mutable.LinkedHashMap[String, Symbol]()
    val classes = mutable.LinkedHashMap[String, Symbol]()
    for (unit <- run.units) unit.tpdTree.foreachSubTree {
      case td: tpd.TypeDef if td.symbol.isAliasType => aliases.getOrElseUpdate(td.name.toString.stripPrefix("__q_"), td.symbol)
      case td: tpd.TypeDef if td.symbol.isClass && !td.symbol.is(Flags.Module) => classes.getOrElseUpdate(td.name.toString, td.symbol)
      case vd: tpd.ValOrDefDef if !vd.symbol.is(Flags.Synthetic) && !vd.symbol.is(Flags.Param) && !vd.symbol.isConstructor =>
        terms.getOrElseUpdate(vd.name.toString, vd.symbol)
      case _ =>
    }

    def same(x: Type, y: Type): Boolean = x <:< y && y <:< x
    def tpe(id: String): Type = {
      val alts = id.split('\u001f')
      alts.iterator.map(tpe1).collectFirst { case Some(t) => t }.getOrElse(throw new NoSuchElementException(s"unresolved ${alts.head}"))
    }
    def tpe1(id: String): Option[Type] =
      // one step, as in Oracle2: the right-hand side as written
      aliases.get(id).map(sym => sym.info match {
        case TypeAlias(alias) => alias
        case _                => sym.typeRef
      }).filterNot(_.isError)
        .orElse(terms.get(id).map(_.info.finalResultType))
    def cls(id: String): Symbol = id.split('\u001f').iterator
      .map(a => classes.get(a).orElse(aliases.get(a).map(_.info.hiBound match {
        case l: HKTypeLambda => l.resType.classSymbol
        case t               => t.classSymbol
      })).orElse(tpe1(a).map(_.classSymbol)))
      .collectFirst { case Some(s) => s }
      .getOrElse(throw new NoSuchElementException(s"unresolved class ${id.split('\u001f').head}"))

    val answers = queries.map { q =>
      val a = try q.split('\t') match {
        case Array("C", a, b) => (tpe(a) <:< tpe(b)).toString
        case Array("E", a, b) => (tpe(a) =:= tpe(b)).toString
        case Array("B", a, c) =>
          val bt = tpe(a).baseType(cls(c))
          if (!bt.exists) "none" else bt.show
        // is our answer the same type as the given one (another engine's, as an operand)? Mutual
        // conformance, not =:=, which tells `M with Object { type T = Int }` from `M { type T = Int }`
        case Array("TE", a, c) => same(tpe(a), tpe(c)).toString
        case Array("BE", a, k, c) =>
          val bt = tpe(a).baseType(cls(k))
          if (!bt.exists) "none" else same(bt, tpe(c)).toString
        case Array("T", a) => tpe(a).show
        case other => "exc bad query " + other.mkString(" ")
      } catch { case t: Throwable => "exc " + t }
      "A\t" + clean(a)
    }
    answers.toSeq
  }
}
