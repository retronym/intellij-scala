package tcoracle

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

/**
 * The scalac 2 side of the type-checking harness (`TcOracle`). Compiled at start-up by the
 * scalac it runs on and called through `java.util.function.Function`, so no scalac type crosses
 * the classloader boundary.
 *
 * Input: `n, path_1, text_1, ..., path_n, text_n, query*`. Output lines, tab-separated:
 *  - `D severity path line col message` per diagnostic (1-based positions; line 0 = no position)
 *  - `A answer` per query, in order (also when there are errors)
 *  - `X message` if the compiler crashed
 *
 * Runs through refchecks (variance, overriding). Queries: `C a b` (a <:< b), `E a b` (a =:= b), `B a cls` (baseType), `T a` (the type).
 */
class Oracle2(classpath: String, options: Array[String]) extends java.util.function.Function[Array[String], Array[String]] {

  // A fresh Global per request: a batch Global does not support re-running changed sources
  // that redefine the same classes. Jar indexes are cached across Globals by scalac.
  private def newGlobal(): Global = {
    val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    settings.classpath.value = classpath
    settings.stopAfter.value = List("refchecks")
    settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
    val (ok, rest) = settings.processArguments(options.toList, processAll = true)
    if (!ok || rest.nonEmpty) throw new IllegalArgumentException("bad scalacOptions: " + options.mkString(" "))
    new Global(settings, new StoreReporter(settings))
  }

  private def clean(s: String) = String.valueOf(s).replace('\t', ' ').replace('\n', ' ')

  def apply(in: Array[String]): Array[String] = {
    val n = in(0).toInt
    val sources = (0 until n).map(i => (in(1 + 2 * i), in(2 + 2 * i)))
    val queries = in.drop(1 + 2 * n)
    try answer(sources, queries)
    catch { case t: Throwable => Array("X\t" + clean(t.toString)) }
  }

  // Files named `__tc_*` only hold the aliases of query operands; an error in them would stop the
  // run before refchecks, so the diagnostics come from a run without them.
  private def answer(sources: Seq[(String, String)], queries: Array[String]): Array[String] = {
    val user = sources.filterNot(_._1.startsWith("__tc_"))
    if (queries.isEmpty) compile(user, Array.empty)
    else compile(user, Array.empty) ++ compile(sources, queries).filter(_.startsWith("A\t"))
  }

  private def compile(sources: Seq[(String, String)], queries: Array[String]): Array[String] = {
    val g = newGlobal()
    import g._
    val reporter = g.reporter.asInstanceOf[StoreReporter]

    // The queries are answered when typer is done: a run that ends with errors resets the
    // symbols of its sources (`compileUnitsInternal`), and later phases would not run at all.
    var answers: Seq[String] = null
    val run = new Run {
      override def advancePhase(): Unit = {
        if (answers == null && phase.name == "typer") answers = answerQueries(g)(this, queries)
        super.advancePhase()
      }
    }
    run.compileSources(sources.map { case (p, t) => new BatchSourceFile(p, t) }.toList)
    val diags = reporter.infos.toList.filter(i => i.severity == reporter.ERROR || i.severity == reporter.WARNING).map { i =>
      val sev = if (i.severity == reporter.ERROR) "error" else "warning"
      val (path, line, col) = if (i.pos.isDefined) (i.pos.source.path, i.pos.line, i.pos.column) else ("", 0, 0)
      s"D\t$sev\t$path\t$line\t$col\t${clean(i.msg)}"
    }
    if (answers == null) answers = queries.toSeq.map(_ => "A\texc not typed")
    (diags ++ answers).toArray
  }

  private def answerQueries(g: Global)(run: g.Run, queries: Array[String]): Seq[String] = {
    import g._
    val aliases = scala.collection.mutable.LinkedHashMap[String, Symbol]()
    val terms = scala.collection.mutable.LinkedHashMap[String, Symbol]()
    val classes = scala.collection.mutable.LinkedHashMap[String, Symbol]()
    def collect(t: Tree): Unit = {
      t match {
        case td: TypeDef if td.symbol.isAliasType => aliases.getOrElseUpdate(td.name.toString.stripPrefix("__q_"), td.symbol)
        case cd: ClassDef                         => classes.getOrElseUpdate(cd.name.toString, cd.symbol)
        case md: ModuleDef                        => terms.getOrElseUpdate(md.name.toString, md.symbol)
        case vd: ValOrDefDef if !vd.symbol.isSynthetic && !vd.symbol.isParameter && vd.name != nme.CONSTRUCTOR =>
          terms.getOrElseUpdate(vd.name.dropLocal.toString, vd.symbol)
        case _ =>
      }
      t.children.foreach(collect)
    }
    run.units.foreach(u => collect(u.body))

    def tpe(id: String): Type = {
      val alts = id.split('\u001f')
      alts.iterator.map(tpe1).collectFirst { case Some(t) => t }.getOrElse(throw new NoSuchElementException(s"unresolved ${alts.head}"))
    }
    def tpe1(id: String): Option[Type] =
      // one step: the alias's right-hand side as written (dealias would expand aliases in it too,
      // e.g. `G1.Edge` to `(Int, Int)`, and change the answer)
      aliases.get(id).map(sym => typeRef(sym.owner.thisType, sym, Nil).betaReduce).filterNot(_.isErroneous)
        .orElse(terms.get(id).map(_.info.finalResultType))
    // through aliases of type constructors (`type X = Seq`), which dealias does not expand
    def classOf(t: Type): Symbol = { val s = t.typeSymbolDirect; if (s.isAliasType) classOf(s.info.resultType) else t.typeSymbol }
    // An unapplied type constructor can't be aliased in Scala 2, so the class operand's text is
    // also typed as a higher-kinded type in the scope of the last source file.
    def typedClass(text: String): Option[Symbol] = {
      val tree = newUnitParser(text, "<cls>").typ()
      val typer = analyzer.newTyper(analyzer.rootContext(run.units.toList.last))
      val tpt = exitingTyper(typer.typedHigherKindedType(tree, scala.reflect.internal.Mode.NOmode))
      Option(tpt.tpe).filter(t => t != NoType && !t.isErroneous).map(classOf)
    }
    def cls(id: String): Symbol = {
      val alts = id.split('\u001f')
      classes.get(alts.head)
        .orElse(alts.iterator.map(a => aliases.get(a).map(s => classOf(s.info.resultType)).filter(s => s.isClass && !s.isErroneous)).collectFirst { case Some(s) => s })
        .orElse(terms.get(alts.head).map(s => classOf(s.info.finalResultType)))
        .orElse(typedClass(alts.head))
        .getOrElse(throw new NoSuchElementException(s"unresolved class ${alts.head}"))
    }

    queries.toSeq.map { q => exitingTyper {
      val a = try q.split('\t') match {
        case Array("C", a, b) => (tpe(a) <:< tpe(b)).toString
        case Array("E", a, b) => (tpe(a) =:= tpe(b)).toString
        case Array("B", a, c) =>
          val bt = tpe(a).baseType(cls(c))
          if (bt == NoType) "none" else bt.toString
        case Array("T", a) => tpe(a).toString
        case other => "exc bad query " + other.mkString(" ")
      } catch { case t: Throwable => "exc " + t }
      "A\t" + clean(a)
    }}
  }
}
