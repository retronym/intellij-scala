package pbtoracle

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

/**
 * The scalac side of `TypePbtTest`. Compiled at test time by the scalac 2.13 it
 * runs on (see `ScalacOracle`), and called through `BiFunction` so that no
 * scalac type crosses the classloader boundary.
 *
 * `apply(source, queries)` types `source` up to typer and answers each query
 * over the `type __q_<id> = ...` aliases in it:
 *
 *  - `C <a> <b>`: `a <:< b`, answer `true` / `false`
 *  - `E <a> <b>`: `a =:= b`
 *  - `B <a> <c>`: `a.baseType(c.typeSymbol)`, answer `none` or `some <type>`
 *  - `S <a> <c>`: `a <:< a.baseType(c.typeSymbol)`, or `none`
 *
 * The result starts with `OK`, or `ERR` followed by the compiler errors. Fields
 * are tab-separated. A query that throws answers `exc <message>`.
 */
class OracleImpl(classpath: String) extends java.util.function.BiFunction[String, Array[String], Array[String]] {
  private var global: Global = _
  private var runs = 0

  private def freshGlobal(): Global = {
    val settings = new Settings
    settings.classpath.value = classpath
    settings.stopAfter.value = List("typer")
    settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
    new Global(settings, new StoreReporter(settings))
  }

  def apply(source: String, queries: Array[String]): Array[String] = {
    // Each program lives in its own package, so one Global can type many of
    // them; renew it now and then to bound the symbol table.
    if (global == null || runs >= 100) { global = freshGlobal(); runs = 0 }
    runs += 1
    val g = global
    import g._
    val reporter = g.reporter.asInstanceOf[StoreReporter]
    reporter.reset()
    val run = new Run
    run.compileSources(List(new BatchSourceFile("Pbt.scala", source)))
    val errors = reporter.infos.toList.filter(_.severity == reporter.ERROR)
    if (errors.nonEmpty)
      return ("ERR" :: errors.map(i => s"${i.pos.line}: ${i.msg}".replace('\n', ' '))).toArray

    val aliases = scala.collection.mutable.Map[String, Symbol]()
    def collect(t: Tree): Unit = {
      t match {
        case td: TypeDef if td.name.startsWith("__q_") => aliases(td.name.toString.stripPrefix("__q_")) = td.symbol
        case _ =>
      }
      t.children.foreach(collect)
    }
    run.units.foreach(u => collect(u.body))
    def tpe(id: String): Type = {
      val sym = aliases.getOrElse(id, throw new NoSuchElementException(s"no alias __q_$id"))
      typeRef(sym.owner.thisType, sym, Nil).dealias
    }

    val answers = queries.map { q =>
      try q.split('\t') match {
        case Array("C", a, b) => (tpe(a) <:< tpe(b)).toString
        case Array("E", a, b) => (tpe(a) =:= tpe(b)).toString
        case Array("B", a, c) =>
          val bt = tpe(a).baseType(tpe(c).typeSymbol)
          if (bt == NoType) "none" else "some\t" + bt
        case Array("S", a, c) =>
          val t = tpe(a)
          val bt = t.baseType(tpe(c).typeSymbol)
          if (bt == NoType) "none" else (t <:< bt).toString
        case other => "exc\tbad query " + other.mkString(" ")
      } catch { case t: Throwable => "exc\t" + String.valueOf(t).replace('\n', ' ') }
    }
    ("OK" +: answers)
  }
}
