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

  /** Names of member types (type members, inner classes): a bare reference to one means `this.M`. */
  private def memberTypeNames(p: Program): Set[String] = {
    def go(ms: List[Member], nested: Boolean): List[String] = ms.flatMap {
      case TypeMem(n, _, _) => List(n)
      case ClassMem(c)      => (if (nested) List(c.name) else Nil) ++ go(c.members, nested = true)
      case _                => Nil
    }
    go(p.members, nested = false).toSet
  }

  /** Whether `t` mentions `this`, explicitly or through a bare member type name in `members`. */
  private def mentionsThis(t: Tp, members: Set[String]): Boolean = {
    def path(p: Path): Boolean = p match {
      case PThis(_)   => true
      case PSel(q, _) => path(q)
      case PId(_)     => false
    }
    t match {
      case TRef(None, n, args) => members(n) || args.exists(mentionsThis(_, members))
      case TRef(pre, _, args)  => pre.exists(path) || args.exists(mentionsThis(_, members))
      case TProj(q, _)         => mentionsThis(q, members)
      case TSingle(p)          => path(p)
      case TWith(ps, refs)     => ps.exists(mentionsThis(_, members)) || refs.exists(r => mentionsThis(r._2, members))
      case TBuiltin(_)         => false
    }
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
        "holds in the plugin, while scalac dealiases k.M). The plugin does know K#M =:= Any, yet also says " +
        "K#M <: T1#M, where T1#M is the unrelated abstract M. Cause not located yet.",
      f => Set("conforms", "equiv")(f.check) && f.direction == "unsound" && {
        val both = abstractTypes(f.program) intersect aliasTypes(f.program)
        (lhs(f).toList ++ rhs(f).toList).flatMap(designated).exists(both)
      }
    ),
    KnownIssue(
      "unstable-prefix-this",
      "TCK group G (30-asf-unstable-prefix): a member whose type or alias mentions `this`, seen from an unstable prefix " +
        "(a projection T#M, or a path through a val typed T#I). scalac abstracts `this` existentially; the plugin " +
        "substitutes the class type or keeps a concrete path. Both directions: K1#M1 <: k1.M1 for `type M1 = Con[this.type]` " +
        "(unsound), k1.v.w.type <: T#M for `type M = this.v.w.type` (incomplete), v13.v7.I2 =:= k0.I2 for " +
        "`val v9: T0#I1; val v13: k0.v9.v7.I1` with `v7: T0.this.type` (unsound), and a self-type member: " +
        "`trait S0 { self: S3 => trait J1 { val w: J5 } }`, `val u: S0#J1` in S2, then s0.u.w.type <: S1#J5 (unsound).",
      f => Set("conforms", "equiv")(f.check) && {
        val members = memberTypeNames(f.program)
        val thisAliases = declarations(f.program).collect { case (_, TypeMem(m, Some(a), _)) if mentionsThis(a, members) => m }.toSet
        val projectsThisAlias = (lhs(f).toList ++ rhs(f).toList).exists {
          case TProj(_, m) => thisAliases(m)
          case _           => false
        }
        // a val typed by a projection, whose members' `this` is then seen from an unstable prefix
        val projectionTypedVal = declarations(f.program).exists {
          case (_, ValMem(_, TProj(_, _))) => true
          case _                           => false
        }
        projectsThisAlias || (projectionTypedVal && declarations(f.program).exists {
          case (_, ValMem(_, t)) => mentionsThis(t, members)
          case _                 => false
        })
      }
    ),
    KnownIssue(
      "a1-refined-compound-with-singleton",
      "SubstitutorInvariants A1 throws (Fail in tests) for a this-link minted in MixinNodes.SuperTypesData for a refined " +
        "compound with a singleton part, e.g. `(T0 with this.type) with this.I5 { type M3 = Any }` in a subtrait of the " +
        "declarer of I5/M3, or `(T0 with a15.I6 { type M3 = Any }) with v14.I6` with `v14: k0.type; a15: v14.type`. " +
        "Not yet analysed whether the link is wrong or A1 is too strict.",
      f => f.check == "pluginException" && f.plugin.contains("A1 violated") && f.features("refinement") && f.features("singleton")
    ),
  )

  def classify(f: Finding): Option[KnownIssue] = known.find(_.matches(f))
}
