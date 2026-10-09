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
      case PThis(_)    => true
      case PSel(q, _)  => path(q)
      case PId("self") => true // the self alias
      case PId(_)      => false
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

  /** Type member names, declared or refined (with repetitions). */
  private def typeMemberNames(p: Program): List[String] = {
    def refined(t: Tp): List[String] = t match {
      case TRef(_, _, args) => args.flatMap(refined)
      case TProj(q, _)      => refined(q)
      case TWith(ps, refs)  => ps.flatMap(refined) ++ refs.flatMap(r => r._1 :: refined(r._2))
      case _                => Nil
    }
    declarations(p).flatMap {
      case (_, TypeMem(n, a, h)) => n :: (a.toList ++ h.toList).flatMap(refined)
      case (_, ValMem(_, t))     => refined(t)
      case (_, Query(_, t))      => refined(t)
      case _                     => Nil
    } ++ p.members.collect { case ClassMem(c) => c }.flatMap(c => (c.parents ++ c.self.toList).flatMap(refined))
  }

  private def mentionsAny(t: Tp, names: Set[String]): Boolean = t match {
    case TRef(_, n, args) => names(n) || args.exists(mentionsAny(_, names))
    case TProj(q, n)      => names(n) || mentionsAny(q, names)
    case TSingle(_)       => false
    case TWith(ps, refs)  => ps.exists(mentionsAny(_, names)) || refs.exists(r => names(r._1) || mentionsAny(r._2, names))
    case TBuiltin(_)      => false
  }

  private def isNull(p: Program, t: Tp): Boolean = t == TBuiltin("Null") || designated(t).exists { n =>
    declarations(p).exists { case (_, TypeMem(`n`, Some(TBuiltin("Null")), _)) => true; case _ => false }
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
      "null-alias-vs-refinement",
      "An alias of Null on the left (`type M1 = Null`, then k0.M1) doesn't conform to a refined type such as " +
        "`Any { type M7 = Any }` or `AnyRef { def foo: Int }` in the plugin; scalac has Null <: every refinement of a " +
        "type admitting null. Plain Null does conform. (The opposite family, Null <: types equivalent to Nothing or " +
        "abstract types bounded by a class, is fixed in admitsNull.)",
      f => f.check == "conforms" && f.direction == "incomplete" && lhs(f).exists(isNull(f.program, _)) &&
        rhs(f).exists { case TWith(_, refs) => refs.nonEmpty; case _ => false }
    ),
    KnownIssue(
      "same-named-type-members",
      "Two type members of the same name meet in a composition (mixins, a self type, or a compound with a refinement) " +
        "and the plugin picks a different one than scalac. `T0 { type M }`, `T1 extends T0`, `T2 { type M = Any }`, " +
        "`K extends T1 with T2`: the plugin had K#M =:= Any but also K#M <: T1#M and k.M <: T1#M. Inside " +
        "`trait T2 { self: T1 => type M4 = Nothing }` with `T1 { type M4 = Any }`, scalac's this.M4 is T1's (Any), the " +
        "plugin's T2's. `(Any { type M4 = k1.M4 }) with K1 <: Any { type M4 = k1.M4 }` holds in the plugin; in scalac " +
        "K1's M4 (seen from the compound's this) wins. (The mixin case K#M <: T1#M, from the name-only arm of " +
        "ScalaConformance's projection visitor, is fixed: an alias member no longer takes that arm.) Incomplete too: `trait T2 extends T1 { self: T3 => type M5 = this.M1 }` " +
        "with M1 abstract in T0 and `= Nothing` in T3: scalac has v19.M5 <: Nothing for `v19: T2`, the plugin doesn't. " +
        "Cause not located yet.",
      f => Set("conforms", "equiv")(f.check) && {
        val twice = typeMemberNames(f.program).groupBy(identity).collect { case (n, ns) if ns.size > 1 => n }.toSet
        twice.nonEmpty && (lhs(f).toList ++ rhs(f).toList).exists(t => mentionsAny(t, twice))
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
        // anywhere in either side: `Con[K0#M1]` for `type M1 = Con[this.M2]` too
        def projectsThisAlias(t: Tp): Boolean = t match {
          case TProj(q, m)      => thisAliases(m) || projectsThisAlias(q)
          case TRef(_, _, args) => args.exists(projectsThisAlias)
          case TWith(ps, refs)  => ps.exists(projectsThisAlias) || refs.exists(r => projectsThisAlias(r._2))
          case _                => false
        }
        // a val typed by a projection, whose members' `this` is then seen from an unstable prefix
        val projectionTypedVal = declarations(f.program).exists {
          case (_, ValMem(_, TProj(_, _))) => true
          case _                           => false
        }
        (lhs(f).toList ++ rhs(f).toList).exists(projectsThisAlias) || (projectionTypedVal && declarations(f.program).exists {
          case (_, ValMem(_, t)) => mentionsThis(t, members)
          case _                 => false
        })
      }
    ),
    KnownIssue(
      "a1-mixin-nodes-refined-compound",
      "SubstitutorInvariants A1 throws (Fail in tests) for a this-link minted in MixinNodes.SuperTypesData for a refined " +
        "compound with a singleton part, e.g. `(T0 with this.type) with this.I5 { type M3 = Any }` in a subtrait of the " +
        "declarer of I5/M3, or `(T0 with a15.I6 { type M3 = Any }) with v14.I6` with `v14: k0.type; a15: v14.type`. " +
        "Not yet analysed whether the link is wrong or A1 is too strict.",
      f => f.check == "pluginException" && f.plugin.contains("A1 violated") && f.plugin.contains("MixinNodes")
    ),
    KnownIssue(
      "a1-projection-unstable-prefix",
      "SubstitutorInvariants A1 throws for a this-link minted in ScProjectionType.processType, reading a member through " +
        "a val typed by a projection: `val v12: K0#I5` in T2 (I5 extends I2, an inner trait of T2 with `val v13: T2`), " +
        "then `this.v12.v13.I1`. The link `this -> T2.this.v12.type asSeenFrom I2` maps its target to K0#v12. Related " +
        "to unstable-prefix-this.",
      f => f.check == "pluginException" && f.plugin.contains("A1 violated") && f.plugin.contains("ScProjectionType")
    ),
    KnownIssue(
      "baseType-merges-this-leak",
      "BaseTypes.baseType reaches a class along two parents (an inner class extending I2 with I1, I2 extends I1) and " +
        "merges two spellings of it into a compound, one with a this-type that isn't in scope: baseType(T1#I4, I1) = " +
        "`T0.this.I1 with T1#I1` (T0 is T1's self type); baseType(this.v11.M5, I1) = `T0.this.v11.I1 with K1.this.v11.I1`. " +
        "The type doesn't conform to that merged base type; it does to scalac's.",
      f => f.check == "baseTypeIsSuper" && f.plugin.contains(" with ")
    ),
  )

  def classify(f: Finding): Option[KnownIssue] = known.find(_.matches(f))
}
