package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import com.intellij.psi._
import org.jetbrains.annotations.Nullable
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.statements._, params._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel._, typedef._
import org.jetbrains.plugins.scala.lang.psi.types._, api._, designator._, nonvalue._

import scala.annotation.tailrec

private case class ThisTypeSubstitution(target: ScType, @Nullable seenFromClass: PsiClass) extends LeafSubstitution {

  override def toString: String = seenFromClass match {
    case null => s"`this` -> $target"
    case _    => s"`this` -> $target asSeenFrom $seenFromClass"
  }

  override protected val subst: PartialFunction[LeafType, ScType] = {
    case th: ScThisType if !hasRecursiveThisType(target, th.element) =>
      ThisTypeSubstitution.enter(s"thisTypeAsSeen($th)#${ThisTypeSubstitution.idOf(this)}  [pre=$target, seenFromClass=${ThisTypeSubstitution.nameOf(seenFromClass)}]")
      val res = doUpdateThisTypeFromClass(th, target, seenFromClass)
      ThisTypeSubstitution.leave(res)
      res
  }

  @tailrec
  private def doUpdateThisType(thisTp: ScThisType, target: ScType): ScType =
    if (isMoreNarrow(target, thisTp, Set.empty)) {
      ThisTypeSubstitution.line(s"isMoreNarrow(pre=$target, $thisTp) = true  -> $target")
      target
    }
    else {
      containingClassType(target) match {
        case Some(targetContext) =>
          ThisTypeSubstitution.line(s"isMoreNarrow(pre=$target, $thisTp) = false  -> climb enclosing to $targetContext")
          doUpdateThisType(thisTp, targetContext)
        case _                   =>
          ThisTypeSubstitution.line(s"isMoreNarrow(pre=$target, $thisTp) = false, no enclosing  -> keep $thisTp")
          thisTp
      }
    }

  private def doUpdateThisTypeFromClass(thisTp: ScThisType, target: ScType, @Nullable clazz: PsiClass): ScType =
    if (clazz == null || clazz == thisTp.element || clazz.containingClass == null) {
      ThisTypeSubstitution.line(s"baseWalk: clazz=${ThisTypeSubstitution.nameOf(clazz)} terminal  -> narrow against pre=$target")
      doUpdateThisType(thisTp, target)
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
        case _                   =>
          ThisTypeSubstitution.line(s"${clazz.name} not a base of pre=$target  -> narrow against pre")
          doUpdateThisType(thisTp, target)
      }
    }

  private def hasRecursiveThisType(tp: ScType, clazz: ScTemplateDefinition): Boolean = {
    // PROBE: -Dscala.asf.noguard disables the guard entirely — combined with
    // scala.asf.canonicalize this tests whether mint-point canonicalization
    // subsumes the guard (without it, this fixture StackOverflows).
    val res = !ThisTypeSubstitution.noGuard && hasRecursiveThisType0(tp, clazz)
    ThisTypeSubstitution.traceGuard(this, tp, clazz, res)
    res
  }

  private def hasRecursiveThisType0(tp: ScType, clazz: ScTemplateDefinition): Boolean =
    tp.subtypeExists {
      // Genuine recursion: the target already mentions `clazz`'s own this-type.
      // Substituting would nest the target inside itself, so always guard this.
      case ScThisType(`clazz`)                   => true
      // Object this-types are terminal: an `object`'s linearization is fixed and its
      // `this` re-anchors to a concrete path exactly once (e.g. `gen.this` ->
      // `pre.gen`), leaving only the prefix's (super-)trait this-types, which are
      // handled by their own substitution. So the inheritor-direction suppression
      // below (added for SCL-18532, a runaway-recursion *perf* fix on the nsc cake
      // `Typers.scala`) is unnecessary for objects and wrongly blocks re-anchoring
      // an object member reached through a path (SCL-21947 shape 7).
      // TODO revisit: the broader trait self-type tension (SCL-18532 <-> SCL-3654)
      //   is still resolved coarsely by `isSameOrInheritor` below; a principled fix
      //   would make this direction-aware rather than object-scoped.
      case _: ScThisType if clazz.is[ScObject]   => false
      case tpe: ScThisType                       => isSameOrInheritor(clazz, tpe)
      case _                                     => false
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
 * `hasRecursiveThisType` decisions (no analog in scalac's `thisTypeAsSeen`) print
 * as `GUARD` lines. Unlike scalac, which walks the finite owner chain once and
 * returns, IntelliJ feeds the substitution output back through the generic
 * `recursiveUpdate` engine, so successive top-level firings show `pre` regrowing
 * (`…analyzer.global.analyzer.global…`) with `GUARD` cutting the regrowth off.
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
  // targets and paths compound (`analyzer.global.analyzer.global…`) until
  // hasRecursiveThisType cuts them off — see testScratchInferencerTrace, which
  // sets scala.asf.nocanon to demonstrate the un-canonicalized behaviour.
  // Verified green across the SCL-21947 oracle (OverrideHighlightingTest +
  // TypeSystemTckTest + typeConformance.generated.* + TypeInferenceBugs5Test +
  // Singleton*ConformanceTest); TCK diffs exactly at the pinned Deferred baseline.
  // NOTE: only the guard's SPELLING-growth role is subsumed; its termination role
  // (self-embedding: even canonical `Global.this.analyzer.type` contains
  // `Global.this`) still needs the guard — see testScratchInferencerCanonNoGuard.
  private def canonOn: Boolean = System.getProperty("scala.asf.nocanon") == null
  def noGuard: Boolean = System.getProperty("scala.asf.noguard") != null
  private val inCanon: ThreadLocal[Boolean] = ThreadLocal.withInitial[Boolean](() => false)

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

  /** A this-substitution's REWRITTEN output being handed to further fused updates in
   *  the same chain (cf. the leaf-only warning at ScSubstitutor.recursiveUpdateImpl):
   *  later this-substitutions may rewrite this-types INSIDE this output — path
   *  concatenation within a single pass. */
  def traceChainFeed(s: ThisTypeSubstitution, from: ScType, to: ScType, remaining: Int): Unit =
    if (on) System.err.println(s"${pad}CHAIN-FEED #${idOf(s)}  ($from -> $to) fed to $remaining more fused update(s)")

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

  /** The guard that scalac has no analog for — print both outcomes, flag blocks. */
  def traceGuard(inst: AnyRef, target: ScType, clazz: ScTemplateDefinition, guarded: Boolean): Unit =
    if (on) System.err.println(
      s"${pad}GUARD#${idOf(inst)} hasRecursiveThisType(${clazz.name}.this, pre=$target) = $guarded${if (guarded) "  -> BLOCK" else ""}")
}
