package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{ScDesignatorType, ScProjectionType, ScThisType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{Contravariant, Covariant, Invariant, JavaArrayType, TypeParameter, Variance}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.{Parameter, ScMethodType, ScTypePolymorphicType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, ReplaceWith, Stop}
import org.jetbrains.plugins.scala.lang.psi.types._

private abstract class SubtypeUpdater(needVariance: Boolean, needUpdate: Boolean) {

  protected implicit def implicitThis: SubtypeUpdater = this

  private def updateCompoundType(ct: ScCompoundType,
                         variance: Variance,
                         substitutor: ScSubstitutor)
                        (implicit visited: Set[ScType]): ScType = {

    val updSignatureMap = ct.signatureMap.map { case (s: TermSignature, tp) =>
      val tParams =
        s.typeParams.map(tparamsByClause => tparamsByClause.map(updateTypeParameter(_, substitutor, Invariant)))

      val paramTypes =
        s.substitutedTypes.map(_.map(f => () => substitutor.recursiveUpdateImpl(f(), variance, isLazySubtype = true)))

      val updSignature = new TermSignature(
        s.name,
        paramTypes,
        tParams,
        s.substitutor.followed(substitutor),
        s.namedElement,
        s.exportedInfo,
        s.hasRepeatedParam
      )

      (updSignature, substitutor.recursiveUpdateImpl(tp, Covariant))
    }

    val updatedTypes = ct.typesMap.map {
      case (s, ta) =>
        val substTps: Seq[TypeParameter] = ta.typeParams.map(updateTypeParameter(_, substitutor))
        val substLower: ScType = substitutor.recursiveUpdateImpl(ta.lowerBound)
        val substUpper: ScType = substitutor.recursiveUpdateImpl(ta.upperBound)
        val combinedSubstitutor: ScSubstitutor = ta.substitutor.followed(substitutor)

        (s, TypeAliasSignature(ta.typeAlias, ta.name, substTps, substLower, substUpper, ta.isDefinition, combinedSubstitutor))
    }
    val updatedComponents = ct.components.smartMap(substitutor.recursiveUpdateImpl(_, variance))

    ScCompoundType(
      updatedComponents,
      forceRefinement = ct.forceRefinement,
      updSignatureMap,
      updatedTypes
    )(ct.projectContext)
  }

  private def updateExistentialArg(exArg: ScExistentialArgument,
                           substitutor: ScSubstitutor)
                          (implicit visited: Set[ScType]): ScType = {
    exArg.copyWithBounds(
      substitutor.recursiveUpdateImpl(exArg.lower, Contravariant, exArg.isLazy),
      substitutor.recursiveUpdateImpl(exArg.upper, Covariant, exArg.isLazy)
    )
  }

  private def updateExistentialType(exType: ScExistentialType,
                            variance: Variance,
                            substitutor: ScSubstitutor)
                           (implicit visited: Set[ScType]): ScType = {
    val quantified = exType.quantified
    val updatedQ = substitutor.recursiveUpdateImpl(quantified, variance)

    if (!needUpdate || (updatedQ eq quantified)) exType
    else ScExistentialType(updatedQ)
  }

  private def updateParameterizedType(pt: ScParameterizedType,
                              variance: Variance,
                              substitutor: ScSubstitutor)
                             (implicit visited: Set[ScType]): ScType = {

    val designator = pt.designator
    val typeArguments = pt.typeArguments
    val typeParameterVariances =
      if (!needVariance) Seq.empty
      else designator.extractDesignated(expandAliases = false) match {
        case Some(n: ScTypeParametersOwner) => n.typeParameters.map(_.variance)
        case _ => Seq.empty
      }
    val newDesignator = substitutor.recursiveUpdateImpl(designator, variance)
    val newTypeArgs = typeArguments.smartMapWithIndex {
      case (ta, i) =>
        val v = if (i < typeParameterVariances.length) typeParameterVariances(i) else Invariant
        substitutor.recursiveUpdateImpl(ta, v * variance)
    }

    if (!needUpdate || (newDesignator eq designator) && (newTypeArgs eq typeArguments)) pt
    else ScParameterizedType(newDesignator, newTypeArgs, substitutor)
  }


  private def updateJavaArrayType(arrType: JavaArrayType,
                          substitutor: ScSubstitutor)
                         (implicit visited: Set[ScType]): ScType = {
    JavaArrayType(substitutor.recursiveUpdateImpl(arrType.argument, Invariant))
  }

  private def updateProjectionType(pt: ScProjectionType,
                           substitutor: ScSubstitutor)
                          (implicit visited: Set[ScType]): ScType = {

    val projected = pt.projected
    val updatedType = substitutor.recursiveUpdateImpl(projected, Covariant)

    if (!needUpdate || (updatedType eq projected)) pt
    else {
      // Canonicalize-at-mint (default ON, -Dscala.asf.nocanon to disable): THE MINT
      // POINT of non-canonical path spellings — rebuilding a projection over a
      // freshly-substituted prefix is where `…analyzer.global` gets spelled for
      // `…global` (cf. the ORIGIN stack in testScratchInferencerTrace). Collapse
      // the new spelling right here so downstream resolution never recirculates it
      // as a substitutor target.
      //
      // NOTE (cross-symbol pump post-mortem): both this canonicalization probe and
      // ScProjectionType.apply's eager alias collapse RESOLVE the rebuilt projection
      // synchronously inside the applying pass (unlike scalac's asSeenFrom, which
      // never consults findMember). That re-entrancy is bounded ONLY because the
      // ANCHOR DISCIPLINE in ThisTypeSubstitution (cursorChainReaches) keeps every
      // firing's output — and hence every prefix these resolutions process and every
      // target they mint — at scalac-sanctioned spellings. Suppressing the poison
      // rewrites at the source proved both necessary and sufficient
      // (testScratchSkeletorCakeCrossSymbolPumpMinimal); making this rebuild path
      // resolution-free instead was NOT sufficient (the same accretion re-entered
      // through conformance-side collapse probes) and broke alias-collapse goldens
      // (SCL-6549/7100/7268/7474).
      ThisTypeSubstitution.canonicalizeTarget(ScProjectionType(updatedType, pt.element))
    }
  }

  private def updateMethodType(mt: ScMethodType,
                       variance: Variance,
                       substitutor: ScSubstitutor)
                      (implicit visited: Set[ScType]): ScType = {

    def updateParameterType(tp: ScType) = substitutor.recursiveUpdateImpl(tp, -variance, isLazySubtype = true)

    def updateParameter(p: Parameter): Parameter = p.copy(
      paramType = updateParameterType(p.paramType),
      expectedType = updateParameterType(p.expectedType),
      defaultType = p.defaultType.map(updateParameterType)
    )

    ScMethodType(
      substitutor.recursiveUpdateImpl(mt.result, variance),
      mt.params.map(updateParameter),
      hasImplicitKW = mt.hasImplicitKW,
      hasUsingKW    = mt.hasUsingKW,
    )(mt.elementScope)
  }

  private def updateTypePolymorphicType(tpt: ScTypePolymorphicType,
                                variance: Variance,
                                substitutor: ScSubstitutor)
                               (implicit visited: Set[ScType]): ScType =
    ScTypePolymorphicType(
      substitutor.recursiveUpdateImpl(tpt.internalType, variance),
      tpt.typeParameters.map(updateTypeParameter(_, substitutor, -variance)),
      isLambdaTypeElement = tpt.isLambdaTypeElement
    )

  private def updateMatchType(mt: ScMatchType,
                              variance: Variance,
                              substitutor: ScSubstitutor)
                             (implicit visited: Set[ScType]): ScType = {
    // TODO Temporary workaround to avoid SOE - type aliases can be recursive, SCL-23190, SCL-20263
    //    mt.upperBound match {
    //      case Some(t) =>
    //        return substitutor.recursiveUpdateImpl(t, variance)
    //      case _ =>
    //    }

    val scrutinee = substitutor.recursiveUpdateImpl(mt.scrutinee, variance)

    val cases = mt.cases.map { cse =>
      () => {
        val (pat, res) = cse.apply()

        substitutor.recursiveUpdateImpl(pat, variance) ->
          substitutor.recursiveUpdateImpl(res, variance)
      }
    }

    ScMatchType(
      scrutinee,
      cases,
      mt.upperBound.map(substitutor.recursiveUpdateImpl(_, variance))
    )
  }

  def updateTypeParameter(tp: TypeParameter,
                          substitutor: ScSubstitutor,
                          variance: Variance = Invariant)
                         (implicit visited: Set[ScType]): TypeParameter =
    TypeParameter(
      tp.psiTypeParameter,
      tp.typeParameters.map(updateTypeParameter(_, substitutor, variance)),
      substitutor.recursiveUpdateImpl(tp.lowerType, variance, isLazySubtype = true),
      substitutor.recursiveUpdateImpl(tp.upperType, variance, isLazySubtype = true)
    )

  private def updateAndType(
    andType: ScAndType,
    variance: Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScType]
  ): ScType = {
    val updatedLhs = substitutor.recursiveUpdateImpl(andType.lhs, variance)
    val updatedRhs = substitutor.recursiveUpdateImpl(andType.rhs, variance)

    if (!needUpdate || ((updatedLhs eq andType.lhs) && (updatedRhs eq andType.rhs)))
      andType
    else
      ScAndType(updatedLhs, updatedRhs)
  }

  private def updateOrType(
    orType: ScOrType,
    variance: Variance,
    substitutor: ScSubstitutor
  )(implicit
    visisted: Set[ScType]
  ): ScType = {
    val updatedLhs = substitutor.recursiveUpdateImpl(orType.lhs, variance)
    val updatedRhs = substitutor.recursiveUpdateImpl(orType.rhs, variance)

    if (!needUpdate || ((updatedLhs eq orType.lhs) && (updatedRhs eq orType.rhs)))
      orType
    else
      ScOrType(updatedLhs, updatedRhs)
  }

  final def updateSubtypes(
    scType:      ScType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScType]
  ): ScType =
    RecursiveUpdateDepthGuard.guarded(scType, substitutor) {
      scType match {
        case t: ScCompoundType        => updateCompoundType(t, variance, substitutor)
        case t: ScOrType              => updateOrType(t, variance, substitutor)
        case t: ScAndType             => updateAndType(t, variance, substitutor)
        case t: ScExistentialArgument => updateExistentialArg(t, substitutor)
        case t: ScExistentialType     => updateExistentialType(t, variance, substitutor)
        case t: ScParameterizedType   => updateParameterizedType(t, variance, substitutor)
        case t: JavaArrayType         => updateJavaArrayType(t, substitutor)
        case t: ScProjectionType      => updateProjectionType(t, substitutor)
        case t: ScMethodType          => updateMethodType(t, variance, substitutor)
        case t: ScTypePolymorphicType => updateTypePolymorphicType(t, variance, substitutor)
        case t: ScMatchType           => updateMatchType(t, variance, substitutor)
        case leaf                     => leaf
      }
    }

  final def recursiveUpdate(scType: ScType, variance: Variance, update: Update): ScType =
    update(scType, variance) match {
      case ReplaceWith(res) => res
      case Stop => scType
      case ProcessSubtypes => updateSubtypes(scType, variance, ScSubstitutor(update))(Set.empty)
    }

}

object SubtypeUpdater {
  implicit class TypeParameterUpdateExt(private val typeParameter: TypeParameter) extends AnyVal {
    def update(substitutor: ScSubstitutor)
              (implicit visited: Set[ScType] = Set.empty): TypeParameter =
      SubtypeUpdaterNoVariance.updateTypeParameter(typeParameter, substitutor)
  }
}

private object SubtypeUpdaterVariance extends SubtypeUpdater(needVariance = true, needUpdate = true)

private object SubtypeUpdaterNoVariance extends SubtypeUpdater(needVariance = false, needUpdate = true)

private object SubtypeTraverser extends SubtypeUpdater(needVariance = false, needUpdate = false)

/**
 * `recursiveUpdateImpl` and `updateSubtypes`/`updateProjectionType`/`updateCompoundType`
 * mutually recurse across separate stack frames (only the `recursiveUpdateImpl` ->
 * `recursiveUpdateImpl` self-call is `@tailrec`-optimized), so a genuinely cyclic type
 * shape — e.g. a projection whose `projected` resolves back through a compound type's
 * component to itself, neither of which is traversed with `isLazySubtype = true` and so
 * never enters the `visited` set (see `ScSubstitutor.recursiveUpdateImpl`) — blows the
 * JVM stack with an undiagnosable `StackOverflowError` deep in library collection code.
 * `updateSubtypes` is the single choke point every hop of that mutual recursion passes
 * through, so a depth counter there catches the cycle before the real SOE and reports
 * the type/substitutor that's looping instead of a wall of `Map.map` frames.
 */
private object RecursiveUpdateDepthGuard {
  private val maxDepth = 1000
  private val depth = new ThreadLocal[Int] {
    override def initialValue(): Int = 0
  }

  // A repro-oriented dump: every helper below is a plain structural pattern
  // match over the type/substitutor ADTs — PsiClass.getQualifiedName and the
  // raw case-class fields only, NEVER .toString/presentableText/extractClass or
  // ScProjectionType.actualElement. Those all re-enter BaseTypes/ThisTypeSubstitution
  // and, for the exact pathological type this guard exists to catch, recurse right
  // back into `guarded` from inside the diagnostic itself (seen in practice: the
  // first cut of this message called scType.toString and produced a SECOND,
  // interleaved depth-exceeded stack before the first could even be logged).
  // The output is meant to be copy-pasted into a standalone unit test: qualified
  // class names + member names are enough to re-select the same PsiClass/PsiMember
  // from the real sources and reconstruct the identical ScThisType/ScProjectionType
  // chain and ScSubstitutor(ThisTypeSubstitution(target, seenFromClass) >> ...)
  // outside the IDE, without needing the runaway resolution to fire again.
  private def safeQualifiedName(clazz: PsiClass): String =
    if (clazz == null) "null"
    else try Option(clazz.getQualifiedName).getOrElse(clazz.getName) catch { case _: Throwable => "<unnamed>" }

  private def rawDump(tp: ScType, fuel: Int = 16): String = {
    if (fuel <= 0) "..."
    else try {
      tp match {
        case ScThisType(clazz)        => s"ThisType(${safeQualifiedName(clazz)})"
        case ScProjectionType(projected, element) =>
          s"Projection(${rawDump(projected, fuel - 1)}, member=${element.getName})"
        case ScDesignatorType(element) => s"Designator(${element.getName})"
        case ParameterizedType(designator, args) =>
          s"Parameterized(${rawDump(designator, fuel - 1)}, [${args.map(rawDump(_, fuel - 1)).mkString(", ")}])"
        case ScCompoundType(comps, _, _) =>
          s"Compound(${comps.map(rawDump(_, fuel - 1)).mkString(" with ")})"
        case _ => tp.getClass.getSimpleName
      }
    } catch { case _: Throwable => s"<unprintable ${tp.getClass.getSimpleName}>" }
  }

  private def dumpUpdate(u: Update): String = u match {
    case ThisTypeSubstitution(target, seenFromClass) =>
      s"ThisTypeUpd(target=${rawDump(target)}, seenFromClass=${safeQualifiedName(seenFromClass)})"
    case other => other.getClass.getSimpleName
  }

  private def dumpSubstitutor(s: ScSubstitutor): String =
    s.substitutions.drop(s.fromIndex).map(dumpUpdate).mkString(" >> ")

  def guarded[T](scType: ScType, substitutor: ScSubstitutor)(body: => T): T = {
    val d = depth.get()
    if (d >= maxDepth) {
      val message =
        s"ScSubstitutor.recursiveUpdateImpl: depth limit ($maxDepth) exceeded, likely a cyclic type.\n" +
          s"  cycling type:  ${rawDump(scType)}\n" +
          s"  substitutor:   ${dumpSubstitutor(substitutor)}\n" +
          "  (structural dump — qualified class + member names only, safe to copy into a standalone repro;" +
          " see RecursiveUpdateDepthGuard's doc comment)"
      ScSubstitutor.LOG.error(message, new Throwable("recursive-update depth exceeded (see message for the cycling type)"))
      throw new RecursiveUpdateOverflowException(message)
    }
    depth.set(d + 1)
    try body
    finally depth.set(d)
  }
}

final class RecursiveUpdateOverflowException(message: String) extends RuntimeException(message)