package org.jetbrains.plugins.scala.lang.typePbt

import org.jetbrains.plugins.scala.lang.typePbt.Ast.*

/**
 * A disagreement, after shrinking. `check` names the property: a differential
 * one (`conforms`, `equiv`, `baseType`: plugin vs scalac) or one the plugin
 * should satisfy by itself (`baseTypeIsSuper`: `T <: baseType(T, C)`), or a
 * plugin failure (`pluginException`, `pluginUnresolved`).
 */
final case class Finding(check: String, question: Question, scalac: String, plugin: String, program: Program) {

  /** Plugin says yes where scalac says no (`unsound`), or the reverse (`incomplete`). */
  def direction: String = (scalac.takeWhile(_ != '\t'), plugin.takeWhile(_ != '\t')) match {
    case ("false", "true") | ("none", "some") => "unsound"
    case ("true", "false") | ("some", "none") => "incomplete"
    case _                                    => "other"
  }

  /** The type forms in the program. */
  lazy val features: Set[String] = Classify.features(program)

  def signature: String = s"$check/$direction [${features.toList.sorted.mkString(",")}]"
}

/** A known cause of findings: a predicate over the shrunk finding. */
final case class KnownIssue(id: String, description: String, matches: Finding => Boolean)

object Classify {

  def features(p: Program): Set[String] = {
    val fs = Set.newBuilder[String]
    def path(x: Path, depth: Int): Unit = x match {
      case PThis(c)   => fs += (if (c.isDefined) "C.this" else "this"); if (depth >= 2) fs += "path>=2"
      case PId(_)     => if (depth >= 2) fs += "path>=2"
      case PSel(q, _) => path(q, depth + 1)
    }
    def tp(t: Tp): Unit = t match {
      case TRef(pre, n, args) =>
        pre.foreach(path(_, 1))
        if (pre.isDefined) fs += "path-type"
        if (Set("Box", "Inv", "Con")(n)) fs += s"variance:$n"
        args.foreach(tp)
      case TProj(q, _) => fs += "projection"; tp(q)
      case TSingle(x)  => fs += "singleton"; path(x, 1)
      case TWith(ps, refs) =>
        fs += "compound"; if (refs.nonEmpty) fs += "refinement"
        ps.foreach(tp); refs.foreach(r => tp(r._2))
      case TBuiltin(n) => if (n != "Any") fs += s"builtin:$n"
    }
    def member(m: Member, nested: Boolean): Unit = m match {
      case TypeMem(_, a, h) => fs += (if (a.isDefined) "type-alias" else "abstract-type"); a.foreach(tp); h.foreach(tp)
      case ValMem(_, t)     => tp(t)
      case Query(_, t)      => tp(t)
      case ClassMem(c) =>
        if (nested) fs += "inner-class"
        if (c.self.isDefined) fs += "self-type"
        if (c.parents.nonEmpty) fs += (if (nested) "inner-inheritance" else "inheritance")
        c.self.foreach(tp); c.parents.foreach(tp)
        c.members.foreach(member(_, nested = true))
    }
    p.members.foreach {
      case ClassMem(c) if Set("Box", "Inv", "Con")(c.name) => // library
      case m => member(m, nested = false)
    }
    fs.result()
  }

  // --- shape helpers for the predicates ---

  private def declarations(p: Program): List[(Option[String], Member)] = {
    def go(owner: Option[String], ms: List[Member]): List[(Option[String], Member)] = ms.flatMap {
      case ClassMem(c) => go(Some(c.name), c.members)
      case m           => List(owner -> m)
    }
    go(None, p.members)
  }

  private def queryType(p: Program, id: String): Option[Tp] =
    declarations(p).collectFirst { case (_, Query(`id`, t)) => t }

  private def abstractTypes(p: Program): Set[String] =
    declarations(p).collect { case (_, TypeMem(n, None, _)) => n }.toSet

  private def aliasTypes(p: Program): Set[String] =
    declarations(p).collect { case (_, TypeMem(n, Some(_), _)) => n }.toSet

  /** The name a type designates, if it is a (possibly prefixed or projected) type reference. */
  private def designated(t: Tp): Option[String] = t match {
    case TRef(_, n, Nil) => Some(n)
    case TProj(_, n)     => Some(n)
    case _               => None
  }

  private def lhs(f: Finding): Option[Tp] = queryType(f.program, f.question.ids.head)
  private def rhs(f: Finding): Option[Tp] = queryType(f.program, f.question.ids(1))

  /**
   * Known causes, most specific first. Add an entry once a shrunk finding has been
   * triaged; leave the description precise enough to find the code.
   */
  val known: List[KnownIssue] = List(
    KnownIssue(
      "baseType-singleton",
      "BaseTypes.baseType(p.type, C) is None where scalac widens p.type to p's type: supersOf widens a singleton only " +
        "through designatorSingletonType, which is gated to stable overridable members.",
      f => f.check == "baseType" && f.direction == "incomplete" && lhs(f).exists(_.isInstanceOf[TSingle])
    ),
    KnownIssue(
      "baseType-abstract-type",
      "BaseTypes.baseType(M, C) is None for an abstract type member M <: C; scalac takes the base type of the upper " +
        "bound. supersOf's IsTypeAlias only unwraps alias definitions, not declarations.",
      f => f.check == "baseType" && f.direction == "incomplete" &&
        lhs(f).flatMap(designated).exists(abstractTypes(f.program))
    ),
    KnownIssue(
      "null-conforms-abstract-type",
      "Null <: M holds in the plugin for an abstract type member M (seen with M <: Nothing); scalac needs Null <: M's lower bound.",
      f => f.check == "conforms" && f.direction == "unsound" &&
        lhs(f).contains(TBuiltin("Null")) && rhs(f).flatMap(designated).exists(abstractTypes(f.program))
    ),
    KnownIssue(
      "abstract-type-vs-mixed-in-alias",
      "A type member declared abstract in one trait and as an alias in another, unrelated trait (mixed in, or the self " +
        "type): in the composition the alias wins in scalac, but the plugin keeps the abstract member (k.M <: T#M " +
        "holds in the plugin, while scalac dealiases k.M). Cause not located yet.",
      f => Set("conforms", "equiv")(f.check) && f.direction == "unsound" && {
        val both = abstractTypes(f.program) intersect aliasTypes(f.program)
        (lhs(f).toList ++ rhs(f).toList).flatMap(designated).exists(both)
      }
    ),
  )

  def classify(f: Finding): Option[KnownIssue] = known.find(_.matches(f))
}
