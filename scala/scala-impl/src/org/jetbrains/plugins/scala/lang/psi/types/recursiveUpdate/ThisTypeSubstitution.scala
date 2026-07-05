package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import com.intellij.psi._
import org.jetbrains.annotations.Nullable
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.statements._, params._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel._, typedef._
import org.jetbrains.plugins.scala.lang.psi.types._, api._, designator._, nonvalue._
import org.jetbrains.plugins.scala.util.ScEquivalenceUtil

import scala.annotation.tailrec

private case class ThisTypeSubstitution(target: ScType, @Nullable seenFromClass: PsiClass) extends LeafSubstitution {

  override def toString: String = seenFromClass match {
    case null => s"`this` -> $target"
    case _    => s"`this` -> $target asSeenFrom $seenFromClass"
  }

  // Two rules make this-substitution sound without the former hasRecursiveThisType
  // structural guard (derived and validated in scala/scala's IntelliJ-replica model,
  // test/junit/scala/reflect/internal/ijmodel + AsSeenFromTest):
  //
  //   PROGRESS  (postcondition, in this PF): refuse a rewrite whose output is a
  //     this-ROOTED PATH still rooted, via inheritance, in the class being
  //     rewritten — no progress => self-embedding => the recirculation fuel of
  //     the growth pump.  See `progressBlocked`.
  //   CONSUMED  (composition scope, enforced with ScSubstitutor): once one chain
  //     element has spine-matched a this-class, later elements may not re-NARROW
  //     it to another THIS-type — scalac's first-match discipline (SCL-7008's
  //     NM.this -> SN.this -> F.this over-rewrite).  A this->PATH re-anchor still
  //     fires: paths are strictly more concrete, and a declaration-side hop's
  //     identity answer must not starve the use-site element carrying the real
  //     anchor (the scala/scala Trees cake: [ValDef.this sfc=Tree] derives
  //     Trees.this identity, then [pre.global.type sfc=Trees] must still
  //     re-anchor Trees.this).  See `isConsumed` / `noteConsumes` and
  //     ScSubstitutor.recursiveUpdateImpl.
  //
  // Versus the old guard: O(output spine) + one inheritance test per firing
  // instead of an O(type-size) target scan; blocks the cross-symbol pump the
  // guard provably missed (its inheritor arm tested the inverse direction); and
  // needs no object carve-out (an object-rooted return has a projection root, so
  // PROGRESS admits it by construction — the SCL-18532/SCL-3654 tension retires).
  override protected val subst: PartialFunction[LeafType, ScType] = {
    case th: ScThisType =>
      ThisTypeSubstitution.enter(s"thisTypeAsSeen($th)#${ThisTypeSubstitution.idOf(this)}  [pre=$target, seenFromClass=${ThisTypeSubstitution.nameOf(seenFromClass)}]")
      val res0 = doUpdateThisTypeFromClass(th, target, seenFromClass)
      val res =
        if ((res0 ne th) && progressBlocked(res0, th)) {
          ThisTypeSubstitution.line(s"PROGRESS-BLOCK: root of $res0 still denotes ${th.element.name}.this  -> keep $th")
          ThisTypeSubstitution.noteConsumes(false)
          th
        }
        else if (res0.isInstanceOf[ScThisType] && ThisTypeSubstitution.isConsumed(th.element)) {
          ThisTypeSubstitution.line(s"CONSUMED: keep $th (this->this re-spelling suppressed)")
          ThisTypeSubstitution.noteConsumes(false)
          th
        }
        else res0
      ThisTypeSubstitution.leave(res)
      res
  }

  // PROGRESS: a POSTcondition on the walk's output — block a this-rewrite iff the
  // returned type's prefix-spine root is a this-type whose class is the same as or
  // an inheritor of the class being rewritten. Rationale (modeled in scala/scala
  // AsSeenFromTest + ijmodel): such a return makes no progress — it claims
  // to eliminate `th` but is still rooted in a this-type denoting that same
  // instance (via inheritance), which is exactly the self-embedding the resolution
  // loop recirculates into unbounded growth. scalac's thisTypeAsSeen only ever
  // STRIPS prefixes from `pre`, so its output structurally cannot violate this.
  // Blocks both the exact pump AND the cross-symbol pump (which the production
  // guard misses: its inheritor arm tests the inverse direction), while admitting
  // SCL-7043's legitimate sequential re-anchor (CE.this.enum.type is rooted at
  // CE.this, and CE AGGREGATES an Enumeration rather than inheriting one).
  // O(output spine) + one inheritance test, vs the guard's O(type-size) scan.
  @tailrec
  private def spineRootThis(tp: ScType): Option[ScThisType] = tp match {
    case th: ScThisType                                 => Some(th)
    case ScProjectionType(pre, _)                       => spineRootThis(pre)
    case ParameterizedType(ScProjectionType(pre, _), _) => spineRootThis(pre)
    case _                                              => None
  }

  // Leaf→leaf narrowings (`Types.this -> Global.this`) are always progress: a bare
  // this-type carries no structure for the resolution loop to recirculate, and such
  // narrowing onto an inheritor's this IS the legitimate cake re-anchor (scalac's
  // matchesPrefixAndClass returns exactly this shape). Only a this-ROOTED PATH whose
  // root still subsumes the rewritten this is self-embedding fuel.
  private def progressBlocked(res: ScType, th: ScThisType): Boolean = res match {
    case _: ScThisType => false
    case _             => spineRootThis(res).exists(rootTh => isSameOrInheritor(rootTh.element, th))
  }

  // `escaped` tracks whether the climb has LEFT the target's own projection spine
  // through a ScThisType -> containingClass hop. A match reached after such an
  // escape is scalac's UNMATCHED case (the walk fell off `pre`; in scalac the
  // this-type would be returned unchanged and a later asSeenFrom hop still
  // applies), so it must NOT consume the this-class for the rest of the fused
  // chain (SCL-7043: [3/6] Enumeration.this exhausts ValueSet.this and climbs to
  // Enumeration.this — identity, no consumption — then the load-bearing [4/6]
  // still rewrites it). A match on the target's own spine is scalac's
  // matchesPrefixAndClass SUCCESS and consumes (SCL-7008: NM.this matched from
  // (Z.this baseType Z).prefix — first match wins, later chain elements may not
  // re-narrow NM.this to SN.this/F.this).
  @tailrec
  private def doUpdateThisType(thisTp: ScThisType, target: ScType, escaped: Boolean = false): ScType =
    if (isMoreNarrow(target, thisTp, Set.empty)) {
      ThisTypeSubstitution.line(s"isMoreNarrow(pre=$target, $thisTp) = true  -> $target")
      ThisTypeSubstitution.noteConsumes(!escaped)
      target
    }
    else {
      containingClassType(target) match {
        case Some(targetContext) =>
          ThisTypeSubstitution.line(s"isMoreNarrow(pre=$target, $thisTp) = false  -> climb enclosing to $targetContext")
          doUpdateThisType(thisTp, targetContext, escaped || target.isInstanceOf[ScThisType])
        case _                   =>
          ThisTypeSubstitution.line(s"isMoreNarrow(pre=$target, $thisTp) = false, no enclosing  -> keep $thisTp")
          ThisTypeSubstitution.noteConsumes(false)
          thisTp
      }
    }

  private def doUpdateThisTypeFromClass(thisTp: ScThisType, target: ScType, @Nullable clazz: PsiClass): ScType =
    if (clazz == null) {
      ThisTypeSubstitution.line(s"baseWalk: anchorless  -> narrow against pre=$target")
      doUpdateThisType(thisTp, target)
    }
    else if (clazz == thisTp.element || clazz.containingClass == null) {
      // ANCHOR DISCIPLINE (the cross-symbol pump's per-firing rule, complementing
      // PROGRESS and CONSUMED): an ANCHORED walk may rewrite `thisTp` only if its
      // owner-chain cursor can actually REACH thisTp's class — scalac's
      // matchesPrefixAndClass demands `clazz == candidate`, so a walk whose cursor
      // exhausts (top-level terminal, or the not-a-base bail below) without touching
      // thisTp.element is scalac's UNMATCHED case: the this-type is left unchanged
      // for a different (correctly-anchored) hop. Falling back to the anchorless
      // inheritor heuristic there is the poison: e.g. [target=typer.this.type
      // sfc=Typer] firing on `Infer.this` climbs Typer -> Typers (never Infer),
      // then isMoreNarrow rewrites `Infer.this -> Global.this.analyzer.type` merely
      // because the prefix widens to an Infer-INHERITOR (Analyzer) — embedding a
      // fresh `Global.this` root that the chain remainder re-anchors: +2 spine
      // segments per application, unbounded across re-derivations
      // (skeleton-minimal.scala, scalac model
      // AsSeenFromTest.crossSymbolPumpConfirmedInProduction). `isSameOrInheritor`
      // rather than scalac's exact `==`: IntelliJ climbs PSI containingClass and
      // spells cake self-types through the declaring trait, so the cursor may
      // legitimately surface as an inheritor of the leaf's class (and in the
      // not-a-base bail the leaf may be reachable only further up the cursor's
      // containing chain — the SCL-21947 Trees/AstTransformer shape, where Global
      // inherits Trees).
      if (anchoredNarrowAdmitted(clazz, target, thisTp)) {
        ThisTypeSubstitution.line(s"baseWalk: clazz=${ThisTypeSubstitution.nameOf(clazz)} terminal (cursor or target reaches ${thisTp.element.name})  -> narrow against pre=$target")
        doUpdateThisType(thisTp, target)
      }
      else {
        ThisTypeSubstitution.line(s"baseWalk: cursor ${ThisTypeSubstitution.nameOf(clazz)} exhausted without reaching ${thisTp.element.name}  -> UNMATCHED, keep $thisTp")
        ThisTypeSubstitution.noteConsumes(false)
        thisTp
      }
    }
    else {
      // Use the merged `baseType` (scalac's `pre baseType clazz`) rather than the
      // first iterator hit, so multiple/merged same-class contributions resolve to
      // one deterministic base type and we take its prefix — cf. AsSeenFromMap.thisTypeAsSeen.
      //
      // THE KEY DIVERGENCE: in scalac `pre baseType clazz` is a cached BaseTypeSeq
      // array lookup — inert data, no re-entry. Here it is a LIVE recompute that
      // itself invokes asSeenFrom, so any `thisTypeAsSeen(...)` firings printed
      // *indented under* this `baseType(...)` header are the re-entry that grows `pre`.
      ThisTypeSubstitution.enter(s"baseType(pre=$target, ${clazz.name})  [scalac: cached BaseTypeSeq lookup, no re-entry | IntelliJ: live recompute, re-enters asSeenFrom ↓]")
      val bt = BaseTypes.baseType(target, clazz)
      ThisTypeSubstitution.leave(bt)
      bt.flatMap(containingClassType) match {
        case Some(targetContext) =>
          ThisTypeSubstitution.line(s"(pre baseType ${clazz.name}).prefix = $targetContext  -> climb owner to ${ThisTypeSubstitution.nameOf(clazz.containingClass)}")
          doUpdateThisTypeFromClass(thisTp, targetContext, clazz.containingClass)
        // `clazz` is not a base type of `target` — e.g. `clazz` is an INNER CLASS reached
        // via a prefixed projection base (`global.AstTransformer`), so an inherited member's
        // ENCLOSING-universe this-type (`SymbolTable.this`/`ApiUniverse.this`, surfacing as
        // `Trees.this`) must re-anchor onto `target` directly. scalac's `thisTypeAsSeen` keeps
        // walking until the prefix is empty rather than bailing on `pre baseType clazz`; mirror
        // that by narrowing against `target` (guarded by `isMoreNarrow`, so unrelated this-types
        // stay put). SCL-21947, the OuterPathTransformer `currentClass` shape.
        // ANCHOR DISCIPLINE applies here too (see the terminal case above): the
        // narrow is only legitimate if the cursor's remaining containing chain can
        // reach thisTp's class (Trees via Global in the SCL-21947 shape); otherwise
        // this bail is scalac's UNMATCHED — the other door of the cross-symbol
        // pump ([target=Global.this.analyzer.type sfc=Typer] firing on Infer.this:
        // Typer is not a base of the target, and neither Typer nor Typers relates
        // to Infer — narrowing here embedded the poison Global.this root).
        case _                   =>
          if (anchoredNarrowAdmitted(clazz, target, thisTp)) {
            ThisTypeSubstitution.line(s"${clazz.name} not a base of pre=$target  -> narrow against pre")
            doUpdateThisType(thisTp, target)
          }
          else {
            ThisTypeSubstitution.line(s"${clazz.name} not a base of pre=$target and cursor chain never reaches ${thisTp.element.name}  -> UNMATCHED, keep $thisTp")
            ThisTypeSubstitution.noteConsumes(false)
            thisTp
          }
      }
    }

  private def containingClassType(tp: ScType): Option[ScType] = tp match {
    case ScThisType(template) =>
      template.containingClass match {
        case td: ScTemplateDefinition => Some(ScThisType(td))
        case _                        => None
      }
    case ScProjectionType(newType, _)                       => Some(newType)
    case ParameterizedType(ScProjectionType(newType, _), _) => Some(newType)
    case _                                                  => None
  }

  private def isSameOrInheritor(clazz: PsiClass, thisTp: ScThisType): Boolean =
    clazz == thisTp.element || isInheritorDeep(clazz, thisTp.element)

  // ANCHOR DISCIPLINE helper: could scalac's owner-chain climb, continuing from
  // `clazz` upward through containing classes, ever reach a cursor position that
  // matches `thisTp`'s class? scalac's matchesPrefixAndClass fires only at
  // `clazz == candidate`; same-or-INHERITOR compensates for IntelliJ spelling cake
  // self-types through the declaring trait. `areClassesEquivalent` rather than `==`:
  // an OBJECT member's declarationAnchor (getContainingClass) is a different PSI
  // handle than the ScThisType's ScObject (SCL-6549's `object SCL6549` cursor
  // failing to "reach" SCL6549 itself). When false, the walk is scalac's UNMATCHED
  // case and the this-type must be kept for a correctly-anchored hop.
  @tailrec
  private def cursorChainReaches(clazz: PsiClass, thisTp: ScThisType): Boolean =
    if (clazz == null) false
    else if (isSameOrInheritor(clazz, thisTp) || ScEquivalenceUtil.areClassesEquivalent(clazz, thisTp.element)) true
    else cursorChainReaches(clazz.containingClass, thisTp)

  // ANCHOR DISCIPLINE, second admission: the target path denotes EXACTLY the leaf's
  // own class/object (`implicitInstance.this` against pre `SCL6549.implicitInstance.type`,
  // SCL-6549) — an honest re-spelling of the same instance as a path. scalac's
  // matchesPrefixAndClass would fire on it under the correctly-anchored hop
  // (pre.widen.typeSymbol == the leaf's class); IntelliJ's fused chain reaches it
  // through a coarser anchor standing in for two sequential asSeenFroms. EXACT
  // class only — a strict-inheritor tip (`Global.this.analyzer.type` vs `Infer.this`,
  // Analyzer inherits Infer) is precisely the cross-symbol poison and stays blocked.
  private def targetDenotesLeafClass(target: ScType, thisTp: ScThisType)(implicit context: Context): Boolean =
    extractAll(target) match {
      case Some(cls: PsiClass) => cls == thisTp.element || ScEquivalenceUtil.areClassesEquivalent(cls, thisTp.element)
      case _                   => false
    }

  private def anchoredNarrowAdmitted(clazz: PsiClass, target: ScType, thisTp: ScThisType): Boolean =
    cursorChainReaches(clazz, thisTp) || targetDenotesLeafClass(target, thisTp)

  private def hasSameOrInheritor(compound: ScCompoundType, thisTp: ScThisType)(implicit context: Context): Boolean = {
    compound.components
      .exists {
        extractAll(_)
          .exists {
            case tp: ScCompoundType => hasSameOrInheritor(tp, thisTp)
            case tp: ScTypeParam =>
              (for {
                upper <- tp.upperBound.toOption
                cls   <- upper.extractClass
              } yield isSameOrInheritor(cls, thisTp)).getOrElse(false)
            // An abstract type member (`type Setting <: SettingValue`) reaches its
            // base classes only through its upper bound. Widen to it, mirroring the
            // `ScTypeParam` branch above and the top-level `isMoreNarrow` alias case,
            // so a `this`-type whose class sits under such a bound still re-anchors
            // (SCL-21947, the `MutableSettings` `BooleanSetting <: Setting {type T = ...}`
            // shape — without this the prefix is left as the raw `SettingValue.this`).
            case ta: ScTypeAlias => isMoreNarrow(ta.upperBound.getOrAny, thisTp, Set.empty)
            case cls: PsiClass => isSameOrInheritor(cls, thisTp)
            case _             => false
          }
      }
  }

  @tailrec
  private def isMoreNarrow(target: ScType, thisTp: ScThisType, visited: Set[PsiElement])(implicit context: Context): Boolean = {
    extractAll(target) match {
      case Some(pat: ScBindingPattern) =>
        if (visited.contains(pat)) false
        else isMoreNarrow(pat.`type`().getOrAny, thisTp, visited + pat)
      case Some(param: ScParameter)    => isMoreNarrow(param.`type`().getOrAny, thisTp, visited)
      case Some(typeParam: PsiTypeParameter) =>
        if (visited.contains(typeParam)) false
        else target match {
          case t: TypeParameterType => isMoreNarrow(t.upperType, thisTp, visited + typeParam)
          case p: ParameterizedType =>
            p.designator match {
              case tpt: TypeParameterType =>
                val upperType = ParameterizedType(tpt.upperType, p.typeArguments)
                isMoreNarrow(upperType, thisTp, visited + typeParam)
              case _                      =>
                isMoreNarrow(p.substitutor(TypeParameterType(typeParam)), thisTp, visited + typeParam)
            }
          case _ => isSameOrInheritor(typeParam, thisTp)
        }
      case Some(t: ScTypeDefinition) =>
        if (visited.contains(t)) false
        else if (isSameOrInheritor(t, thisTp)) true
        else
          t.selfType match {
            case Some(selfTp) => isMoreNarrow(selfTp, thisTp, visited + t)
            case _            => false
          }
      case Some(td: ScTypeAliasDeclaration) => isMoreNarrow(td.upperBound.getOrAny, thisTp, visited)
      case Some(cl: PsiClass)               => isSameOrInheritor(cl, thisTp)
      case Some(named: ScTypedDefinition)   =>
        named.`type`().getOrAny match {
          // A stable path whose declared type IS `thisTp` itself (`val global: Global.this.type`)
          // cannot narrow `thisTp`: collapsing `Global.this := <path>` where `<path>: Global.this.type`
          // is circular and, when this path is reached as a projection prefix
          // (`pre.genBCode.global`), folds `Global.this` onto the whole projection — a self-referential
          // type the resolver's recursion guard then prunes, dropping all members. Refuse the collapse
          // so `doUpdateThisType` keeps walking the prefix chain to a concrete outer prefix
          // (`pre.global`). (nsc `Global { val genBCode: SubComponent { val global: Global.this.type } }` shape)
          case ScThisType(c) if c == thisTp.element => false
          case nt                                   => isMoreNarrow(nt, thisTp, visited)
        }
      case Some(compound: ScCompoundType)   => hasSameOrInheritor(compound, thisTp)
      case _                                => false
    }
  }

  private def extractAll(tp: ScType)(implicit context: Context) = {
    // Like tp.extractDesignated(expandAliases = true)
    // But also return non-designator returns
    // Such as ScCompoundType, after dealiasing and dereferencing.

    def elem1(tp: DesignatorOwner) = tp match { case tp: ScProjectionType => tp.actualElement case _ => tp.element }
    def subst(tp: DesignatorOwner) = tp match { case tp: ScProjectionType => tp.actualSubst   case _ => ScSubstitutor.empty }

    def rec(tp: ScType, seen: Set[ScTypeAlias]): Either[ScType, PsiNamedElement] = tp match {
      case tp: NonValueType      => rec(tp.inferValueType, seen)
      case tp: DesignatorOwner   => elem1(tp) match {
        case ta: ScTypeAliasDefinition if !ta.isEffectivelyOpaque && !seen(ta) => ta.aliasedType.map(subst(tp)).fold(_ => Left(tp), rec(_, seen + ta))
        case tp                                     => Right(tp)
      }
      case tp: ParameterizedType => tp match {
        case AliasType(ta: ScTypeAliasDefinition, _, Right(ub), effectivelyOpaque) if !effectivelyOpaque && !seen(ta) => rec(ub, seen + ta)
        case _                                                               => rec(tp.designator, seen)
      }
      case tp: StdType           => tp.syntheticClass.toRight(tp)
      case tp: ScExistentialType => rec(tp.quantified, seen)
      case tp: TypeParameterType => Right(tp.psiTypeParameter)
      case tp: ScCompoundType    => Left(tp)
      case _                     => Left(tp)
    }

    rec(tp, Set.empty).fold(Some(_), Some(_))
  }
}

/**
 * asSeenFrom tracing, the IntelliJ analog of scalac's `LoggingAsSeenFromMap`
 * (`test/junit/scala/reflect/internal/AsSeenFromTest.scala` in scala/scala).
 *
 * Enable from a test body with `System.setProperty("scala.asf.trace", "true")`
 * BEFORE triggering type computation. (Passing `-Dscala.asf.trace` on the sbt
 * command line does NOT work — it isn't forwarded to the forked test JVM; the
 * property must be set from inside the same JVM, cf. SCL-21947 handoff notes.)
 *
 * Output mirrors scalac's indented style: each `thisTypeAsSeen(...)` firing (a
 * `ScThisType` leaf re-anchored onto the prefix `pre` = `target`) opens an indent
 * level; the internal owner-chain walk prints its `(pre baseType clazz).prefix`
 * climbs and `isMoreNarrow` narrowing decisions one level in — the counterpart of
 * scalac's `matchesPrefixAndClass` steps — and `leave` closes with `= <result>`.
 * `PROGRESS-BLOCK` lines mark rewrites refused by the progress postcondition
 * (no analog in scalac's `thisTypeAsSeen`, whose stripping walk cannot
 * self-embed by construction). Unlike scalac, which walks the finite owner chain
 * once and returns, IntelliJ feeds the substitution output back through the
 * generic `recursiveUpdate` engine — PROGRESS and the per-chain CONSUMED rule
 * are what keep that recirculation at a fixpoint.
 */
private object ThisTypeSubstitution {
  private def on: Boolean = System.getProperty("scala.asf.trace") != null

  private val indentTL: ThreadLocal[Int] = ThreadLocal.withInitial[Int](() => 0)
  private def pad: String = "  " * indentTL.get

  def nameOf(@Nullable c: PsiClass): String = Option(c).map(_.name).getOrElse("<null>")

  def idOf(x: AnyRef): String = Integer.toHexString(System.identityHashCode(x))

  // ── Canonicalize-at-mint (ON by default; disable with -Dscala.asf.nocanon) ────
  // Collapse non-canonical singleton val-path spellings (`…analyzer.global` for
  // `…global`) where they are minted: when a substitution pass rebuilds a
  // projection over a rewritten prefix (SubtypeUpdater.updateProjectionType), and
  // at the ScSubstitutor construction chokepoints. Mirrors the (private)
  // ScalaConformance collapse: own designatorSingletonType, then the compound
  // prefix's refinement (`new { val global: Global.this.type } with Analyzer`).
  // Without this, resolution recirculates fresh spellings as new substitutor
  // targets and paths compound (`analyzer.global.analyzer.global…`) until the
  // PROGRESS rule cuts them off — see testScratchInferencerTrace, which sets
  // scala.asf.nocanon to demonstrate the un-canonicalized behaviour.
  // Verified green across the SCL-21947 oracle (OverrideHighlightingTest +
  // TypeSystemTckTest + typeConformance.generated.* + TypeInferenceBugs5Test +
  // Singleton*ConformanceTest); TCK diffs exactly at the pinned Deferred baseline.
  // NOTE: canonicalization subsumes only the SPELLING-growth channel; termination
  // of the self-embedding channel (even canonical `Global.this.analyzer.type`
  // contains `Global.this`) is the PROGRESS rule's job — see
  // testScratchPumpFixpoint, which disables canon and still fixpoints.
  private def canonOn: Boolean = System.getProperty("scala.asf.nocanon") == null
  // CONSUMED: first-match-wins per this-class within a fused chain. Once one
  // chain update has MATCHED a this-leaf of class C on its target's own spine
  // (identity counts — scalac's thisTypeAsSeen stops at its first prefix/class
  // match), the remainder processing that update's output may not rewrite a
  // this-type of C again. It MAY rewrite this-types of NEW classes the output
  // introduced — the legitimate sequential composition (SCL-7043:
  // Enumeration.this -> CE.this.enum.type introduces CE.this for the next
  // update). Without this, redundant chain elements (followed()-concatenation
  // duplicates; 3224/3230 multi-this-subst chains) RE-NARROW an already-processed
  // this: SCL-7008's NM.this -> (identity) -> SN.this -> F.this over-rewrite,
  // where scalac stops at NM.this. Complementary to PROGRESS: Progress blocks
  // self-embedding (each pump hop introduces a new class, so consumption alone
  // cannot stop it); consumption blocks redundant re-narrowing (each redundant
  // hop reuses the same class, so Progress alone cannot stop it).
  private val consumedTL: ThreadLocal[Set[ScTemplateDefinition]] =
    ThreadLocal.withInitial[Set[ScTemplateDefinition]](() => Set.empty)

  // Whether the firing that just completed was a MATCH on the target's spine
  // (consumes its this-class) vs an escape/exhaustion (does not). Written by
  // doUpdateThisType, read synchronously by ScSubstitutor right after the
  // update returned ReplaceWith. Nested firings write earlier and are
  // overwritten by the outermost walk's final answer.
  private val lastConsumesTL: ThreadLocal[Boolean] = ThreadLocal.withInitial[Boolean](() => true)
  def noteConsumes(b: Boolean): Unit = lastConsumesTL.set(b)
  def lastFiringConsumes: Boolean = lastConsumesTL.get

  def isConsumed(elem: ScTemplateDefinition): Boolean =
    consumedTL.get.contains(elem)

  def withConsumed[T](elem: ScTemplateDefinition)(op: => T): T = {
    val saved = consumedTL.get
    consumedTL.set(saved + elem)
    try op finally consumedTL.set(saved)
  }

  /** Scope the consumed set to one top-level chain application: nested
   *  applications (e.g. baseType recomputes inside a firing) start fresh. */
  def freshConsumedScope[T](op: => T): T = {
    val saved = consumedTL.get
    if (saved.isEmpty) op
    else {
      consumedTL.set(Set.empty)
      try op finally consumedTL.set(saved)
    }
  }
  private val inCanon: ThreadLocal[Boolean] = ThreadLocal.withInitial[Boolean](() => false)

  /** Audit a fused chain at application: print chains carrying >= 2 this-substitutions,
   *  flagging duplicate targets (redundant fusion). Trace-gated. */
  def auditChain(updates: Array[Update], tp: ScType): Unit = if (on) {
    var count = 0
    var i = 0
    while (i < updates.length) { if (updates(i).isInstanceOf[ThisTypeSubstitution]) count += 1; i += 1 }
    if (count >= 2) {
      val ttss = new Array[ThisTypeSubstitution](count)
      var j = 0; i = 0
      while (i < updates.length) {
        updates(i) match { case t: ThisTypeSubstitution => ttss(j) = t; j += 1; case _ => }
        i += 1
      }
      val rendered = ttss.map(t => s"#${idOf(t)} target=${t.target} sfc=${nameOf(t.seenFromClass)}").mkString("  |  ")
      val distinctTargets = ttss.map(_.target.toString).distinct.length
      val dup = if (distinctTargets < count) s"  <== DUPLICATE TARGETS ($distinctTargets distinct of $count)" else ""
      System.err.println(s"${pad}CHAIN-AUDIT[${updates.length} updates, $count this-substs]$dup  applying to: $tp\n$pad   $rendered")
    }
  }

  private def isSingletonLike(t: ScType): Boolean = t match {
    case _: ScThisType      => true
    case d: DesignatorOwner => d.isSingleton
    case _                  => false
  }

  // NOTE: written lambda-free (explicit matches/loop) — the incremental jar
  // packager misses freshly-introduced nested anonfun classes.
  private def projSingleton(proj: ScProjectionType): Option[ScType] = {
    val own = proj.designatorSingletonType match {
      case s @ Some(t) if isSingletonLike(t) => s
      case _                                 => None
    }
    if (own.isDefined) own
    else proj.projected match {
      case pp: ScProjectionType =>
        pp.designatorSingletonType match {
          case Some(ct: ScCompoundType) => refinedSingleton(ct, proj)
          case _                        => None
        }
      case _ => None
    }
  }

  private def refinedSingleton(ct: ScCompoundType, proj: ScProjectionType): Option[ScType] = {
    val it = ct.signatureMap.iterator
    while (it.hasNext) {
      val (sig, tpe) = it.next()
      if (sig.name == proj.element.name) {
        val substed = proj.actualSubst(tpe)
        if (isSingletonLike(substed)) return Some(substed)
      }
    }
    None
  }

  def canonicalizeTarget(tp: ScType): ScType =
    if (!canonOn || inCanon.get) tp
    else {
      inCanon.set(true)
      try {
        @annotation.tailrec
        def collapse(t: ScType, fuel: Int): ScType = t match {
          case proj: ScProjectionType if fuel > 0 =>
            val stable = proj.element match {
              case d: ScTypedDefinition => d.isStable
              case _                    => false
            }
            if (!stable) t
            else projSingleton(proj) match {
              case Some(s) if s ne proj => collapse(s, fuel - 1)
              case _                    => t
            }
          case _ => t
        }
        val res = collapse(tp, 8)
        if ((res ne tp) && on)
          System.err.println(s"${pad}CANON  $tp  ==>  $res")
        res
      } finally inCanon.set(false)
    }

  // ORIGIN HUNT: one-shot full-stack dump at the FIRST construction whose target
  // matches -Dscala.asf.origin=<regex> (e.g. "analyzer\\.global") — names the code
  // path that first fed a non-canonical (uncollapsed) path back in as a target.
  @volatile private var originDumped = false
  private def originHunt(inst: ThisTypeSubstitution): Unit = {
    val re = System.getProperty("scala.asf.origin")
    if (re != null && !originDumped && re.r.findFirstIn(inst.target.toString).isDefined) {
      originDumped = true
      val stack = Thread.currentThread.getStackTrace.iterator
        .drop(1)
        .filter(e => e.getClassName.startsWith("org.jetbrains"))
        .map(e => s"  ${e.getClassName.substring(e.getClassName.lastIndexOf('.') + 1)}.${e.getMethodName}:${e.getLineNumber}")
        .mkString("\n")
      System.err.println(s"ORIGIN #${idOf(inst)}  first target matching /$re/: ${inst.target}\n$stack")
    }
  }

  /** Log the CONSTRUCTION of a ThisTypeSubstitution: instance id, params, and the
   *  (filtered) call site — so firings with grown targets can be traced back to
   *  whoever built a substitutor out of a previously-substituted type. */
  def traceNew(inst: ThisTypeSubstitution): ThisTypeSubstitution = {
    originHunt(inst)
    if (on) {
      val site = Thread.currentThread.getStackTrace.iterator
        .drop(1)
        .filter { e =>
          val cn = e.getClassName
          !cn.startsWith("java.") && !cn.startsWith("jdk.") && !cn.startsWith("scala.") &&
            !cn.contains("recursiveUpdate")
        }
        .take(3)
        .map(e => s"${e.getClassName.substring(e.getClassName.lastIndexOf('.') + 1)}.${e.getMethodName}:${e.getLineNumber}")
        .mkString("  <  ")
      System.err.println(s"${pad}NEW #${idOf(inst)}  target=${inst.target}  seenFromClass=${nameOf(inst.seenFromClass)}\n$pad     at $site")
    }
    inst
  }

  /** Every this-substitution REWRITE, with its position in the applying chain:
   *  `[k/n]` — n == 1 means a BARE (single-update) substitutor; k < n means the
   *  rewritten output is handed to the remaining fused updates (cf. the leaf-only
   *  warning at ScSubstitutor.recursiveUpdateImpl): later this-substitutions may
   *  rewrite this-types INSIDE this output — path concatenation within one pass. */
  def traceRewrite(s: ThisTypeSubstitution, from: ScType, to: ScType, k: Int, n: Int): Unit =
    if (on) {
      val feed = if (k < n) s"  -> fed to ${n - k} more fused update(s)" else ""
      System.err.println(s"${pad}REWRITE #${idOf(s)} [update $k/$n]  ($from -> $to)$feed")
    }

  /** Print `msg` at the current indent, then descend one level. */
  def enter(msg: => String): Unit = if (on) {
    System.err.println(s"$pad$msg")
    indentTL.set(indentTL.get + 1)
  }

  /** Ascend one level, then print `= result` at that indent (matches scalac). */
  def leave(result: Any): Unit = if (on) {
    indentTL.set(math.max(0, indentTL.get - 1))
    System.err.println(s"$pad= $result")
  }

  /** A single step at the current indent (owner-chain climb / narrowing decision). */
  def line(msg: => String): Unit = if (on) System.err.println(s"$pad$msg")
}
