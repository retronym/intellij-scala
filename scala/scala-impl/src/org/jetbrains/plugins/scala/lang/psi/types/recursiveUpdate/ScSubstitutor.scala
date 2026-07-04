package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import com.intellij.openapi.diagnostic.Logger
import com.intellij.psi.{PsiClass, PsiMember, PsiNamedElement}
import org.jetbrains.plugins.scala.extensions.{ArrayExt, PsiNamedElementExt}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScTypeArgs, ScTypeArgument, ScTypeElementExt}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, TypeParamId, TypeParamIdOwner}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScThisType
import org.jetbrains.plugins.scala.lang.psi.types.Compatibility.Expression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.{Covariant, TypeParameter, TypeParameterType, UndefinedType, Variance}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.Parameter
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, ReplaceWith, Stop}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

import scala.annotation.tailrec
import scala.collection.immutable.LongMap
import scala.util.hashing.MurmurHash3

/**
 * [[ScSubstitutor]] is a transformation of a type which is applied recursively from top to leaves.
 *
 * Examples of such transformation:
 *  - replacing type parameter with it's bound or actual argument
 *  - replacing `this` type to a concrete inheritor type
 *
 * It's also possible to chain several substitutors to create a compound one.
 *
 * If it's known that every substitution in a chain may update only leaf subtypes,
 * then we may avoid recursively traversing a type several times.
 */
final class ScSubstitutor private(_substitutions: Array[Update],   //Array is used for the best concatenation performance, it is effectively immutable
                                  _fromIndex: Int = 0)
  extends (ScType => ScType) {

  import ScSubstitutor._

  private[recursiveUpdate] val substitutions = _substitutions
  private[recursiveUpdate] val fromIndex = _fromIndex
  private[recursiveUpdate] def next: ScSubstitutor = new ScSubstitutor(substitutions, fromIndex + 1)

  private[recursiveUpdate] lazy val hasNonLeafSubstitutions: Boolean = hasNonLeafSubstitutionsImpl

  private[this] def hasNonLeafSubstitutionsImpl: Boolean = {
    var idx = fromIndex
    while (idx < substitutions.length) {
      if (!substitutions(idx).isInstanceOf[LeafSubstitution])
        return true

      idx += 1
    }
    false
  }

  //positive fromIndex is possible only for temporary substitutors during recursive update
  private def assertFullSubstitutor(): Unit = LOG.assertTrue(fromIndex == 0)

  override def apply(`type`: ScType): ScType = {
    if (cacheSubstitutions)
      cache ++= this.allTypeParamsMap

    ThisTypeSubstitution.auditChain(substitutions, `type`)
    ThisTypeSubstitution.freshConsumedScope {
      recursiveUpdateImpl(`type`)(SubtypeUpdaterNoVariance, Set.empty)
    }
  }

  //This method allows application of different `Update` functions in a single pass (see ScSubstitutor).
  //WARNING: If several updates are used, they should be applicable only for leaf types, e.g. which return themselves
  //from `updateSubtypes` method
  @tailrec
  private[recursiveUpdate] def recursiveUpdateImpl(scType: ScType,
                                                   variance: Variance = Covariant,
                                                   isLazySubtype: Boolean = false)
                                                  (implicit subtypeUpdater: SubtypeUpdater,
                                                   visited: Set[ScType] = Set.empty): ScType = {
    if (fromIndex >= substitutions.length || visited(scType)) scType
    else {
      val currentUpdate = substitutions(fromIndex)

      currentUpdate(scType, variance) match {
        case ReplaceWith(res) =>
          currentUpdate match {
            case tts: ThisTypeSubstitution if res ne scType =>
              // position [k/n]: n == 1 -> bare substitutor; k < n -> output fed to the rest of the fused chain
              ThisTypeSubstitution.traceRewrite(tts, scType, res, fromIndex + 1, substitutions.length)
            case _ =>
          }
          // The CONSUMED rule: a this-leaf matched by this update ON ITS TARGET'S SPINE
          // (identity included) is consumed for its class — the remainder processing of
          // `res` may not rewrite a this-type of the same class again (first-match-wins,
          // as in scalac's thisTypeAsSeen), though it may rewrite this-types of NEW
          // classes `res` introduced (the legitimate sequential composition, SCL-7043).
          // A match reached only through the enclosing-this escape climb does NOT
          // consume (lastFiringConsumes = false) — scalac's unmatched case, where a
          // later hop must still apply. See ThisTypeSubstitution.
          val consumedClass: ScTemplateDefinition = currentUpdate match {
            case _: ThisTypeSubstitution if ThisTypeSubstitution.lastFiringConsumes =>
              scType match {
                case th: ScThisType => th.element
                case _              => null
              }
            case _ => null
          }
          if (consumedClass != null)
            continueConsumed(consumedClass, res, variance, isLazySubtype)(subtypeUpdater, visited)
          else next.recursiveUpdateImpl(res, variance, isLazySubtype)(subtypeUpdater, visited)
        case Stop => scType
        case ProcessSubtypes =>
          val newVisited = if (isLazySubtype) visited + scType else visited

          if (hasNonLeafSubstitutions) {
            val withCurrentUpdate = subtypeUpdater.updateSubtypes(scType, variance, ScSubstitutor(currentUpdate))(newVisited)
            next.recursiveUpdateImpl(withCurrentUpdate, variance)(subtypeUpdater, Set.empty)
          }
          else {
            subtypeUpdater.updateSubtypes(scType, variance, this)(newVisited)
          }
      }
    }
  }

  // Out-of-line so recursiveUpdateImpl's own tail call stays a tail call.
  private def continueConsumed(consumedClass: ScTemplateDefinition, res: ScType,
                               variance: Variance, isLazySubtype: Boolean)
                              (subtypeUpdater: SubtypeUpdater, visited: Set[ScType]): ScType =
    ThisTypeSubstitution.withConsumed(consumedClass) {
      next.recursiveUpdateImpl(res, variance, isLazySubtype)(subtypeUpdater, visited)
    }

  def followed(other: ScSubstitutor): ScSubstitutor = {
    assertFullSubstitutor()

    if (this.isEmpty)
      if (other.fromIndex > 0) new ScSubstitutor(other.substitutions)
      else other
    else if (other.isEmpty) this
    else {
      val thisLength = substitutions.length
      val otherLength = other.substitutions.length
      val newLength = thisLength + otherLength

      if (newLength > followLimit) {
        LOG.error(s"Too large substitutors concatenated (of size $thisLength and $otherLength")
      }

      val newArray = new Array[Update](newLength)
      substitutions.copyToArray(newArray, 0)
      other.substitutions.copyToArray(newArray, thisLength)

      new ScSubstitutor(newArray)
    }
  }

  def followUpdateThisType(tp: ScType): ScSubstitutor = {
    ScSubstitutor(tp).followed(this)
  }

  def followUpdateThisType(tp: ScType, seenFromClass: PsiClass): ScSubstitutor = {
    ScSubstitutor(tp, seenFromClass).followed(this)
  }

  def withBindings(from: Iterable[TypeParameter], target: Iterable[TypeParameter]): ScSubstitutor = {
    assertFullSubstitutor()

    def simple: ScSubstitutor = bind(from, target)(TypeParameterType(_))

    def mergeHead(old: TypeParamSubstitution): ScSubstitutor = {
      val newMap = TypeParamSubstitution.buildMap(from, target, old.tvMap)(TypeParameterType(_))
      val cloned = substitutions.clone()
      cloned(0) = TypeParamSubstitution(newMap)
      new ScSubstitutor(cloned)
    }

    if (from.isEmpty || target.isEmpty) this
    else if (this.isEmpty) simple
    else {
      substitutions.head match {
        case tps: TypeParamSubstitution => mergeHead(tps)
        case _ => simple.followed(this)
      }
    }
  }

  override def hashCode(): Int = MurmurHash3.arrayHash(substitutions)

  override def equals(obj: Any): Boolean = obj match {
    case other: ScSubstitutor => other.substitutions sameElements substitutions
    case _ => false
  }

  override def toString: String = {
    val text = substitutions.mkString(" >> ")
    s"ScSubstitutor($text)"
  }

  def isEmpty: Boolean = substitutions.isEmpty

  def isApplicableToTypeParam[T: TypeParamId](tparam: T): Boolean = {
    val id = tparam.typeParamId
    allTypeParamsMap.contains(id)
  }

  private lazy val allTypeParamsMap: LongMap[ScType] =
    substitutions.foldLeft(LongMap.empty[ScType]) { (map, substitution) =>
      substitution match {
        case TypeParamSubstitution(tvMap) => map ++ tvMap
        case _                            => map
      }
    }
}

object ScSubstitutor {
  val LOG: Logger = Logger.getInstance("#org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor")

  /**
   * Substitutor that returns the same type object instance without any modifications
   * i.e. `ScSubstitutor.empty(t) eq t == true`
   *
   * TODO: consider rewriting ScSubstitutor with a seled trait with an Empty implementation which just returns same instance.
   *  (!) but before that investigate the performance impact, because Substitutor.apply is a VERY HOT method !,
   *  in theory JIT-compiler can work worse after rewriting (according Nikolay)
   */
  val empty: ScSubstitutor = new ScSubstitutor(Array.empty)

  private[recursiveUpdate] def apply(s: Update) = new ScSubstitutor(Array(s))

  private val followLimit = 800

  var cacheSubstitutions = false

  var cache: LongMap[ScType] = LongMap.empty

  def apply(tvMap: LongMap[ScType]): ScSubstitutor = {
    if (tvMap.isEmpty) ScSubstitutor.empty
    else ScSubstitutor(TypeParamSubstitution(tvMap))
  }

  def apply(updateThisType: ScType): ScSubstitutor =
    ScSubstitutor(ThisTypeSubstitution.traceNew(ThisTypeSubstitution(ThisTypeSubstitution.canonicalizeTarget(updateThisType), null)))

  def apply(updateThisType: ScType, seenFromClass: PsiClass): ScSubstitutor =
    ScSubstitutor(ThisTypeSubstitution.traceNew(ThisTypeSubstitution(ThisTypeSubstitution.canonicalizeTarget(updateThisType), seenFromClass)))

  /** The declaration-site anchor for an asSeenFrom-style this-substitution over
   *  `member`'s type: the class containing `member`'s name context — scalac's
   *  `sym.owner` in `sym.info.asSeenFrom(pre, sym.owner)`. Null when there is no
   *  containing class (top-level / synthetic / local), which degrades to the
   *  legacy anchorless `isMoreNarrow` walk in [[ThisTypeSubstitution]]. */
  def declarationAnchor(member: PsiNamedElement): PsiClass = member.nameContext match {
    case m: PsiMember => m.getContainingClass
    case _            => null
  }

  def paramToExprType(parameters: Seq[Parameter], expressions: Seq[Expression], useExpected: Boolean = true): ScSubstitutor =
    ScSubstitutor(ParamsToExprs(parameters, expressions, useExpected))

  def paramToParam(fromParams: Seq[ScParameter], toParams: Seq[ScParameter]): ScSubstitutor =
    ScSubstitutor(ParamToParam(fromParams, toParams))

  def paramToType(fromParams: Seq[Parameter], types: Seq[ScType]): ScSubstitutor =
    ScSubstitutor(ParamToType(fromParams, types))

  def bind[T: TypeParamId](typeParamsLike: Iterable[T])(toScType: T => ScType): ScSubstitutor = {
    val tvMap = TypeParamSubstitution.buildMap(typeParamsLike, typeParamsLike)(toScType)
    ScSubstitutor(tvMap)
  }

  def bind[T: TypeParamId, S](typeParamsLike: Iterable[T], targets: Iterable[S])(toScType: S => ScType): ScSubstitutor = {
    val tvMap = TypeParamSubstitution.buildMap(typeParamsLike, targets)(toScType)
    ScSubstitutor(tvMap)
  }

  def bind[T: TypeParamId](typeParamsLike: Iterable[T], typeArguments: Seq[ScTypeArgument]): ScSubstitutor = {
    if (!typeArguments.exists(_.isNamed))
      bind(typeParamsLike, typeArguments.flatMap(_.typeElement))(_.calcType)
    else {
      val typeParamsSeq = typeParamsLike.toIndexedSeq

      val matchedNamedTypeArgs = typeArguments.flatMap { typeArg =>
        for {
          argName     <- typeArg.name
          typeElement <- typeArg.typeElement
          typeParam   <- typeParamsSeq.find(typeParam =>
            typeParam.typeParamName.exists(ScalaNamesUtil.equivalent(_, argName))
          )
        } yield typeParam -> typeElement.calcType
      }

      bind(matchedNamedTypeArgs.map(_._1), matchedNamedTypeArgs.map(_._2))
    }
  }

  def bind[T: TypeParamId](typeParamsLike: Iterable[T], typeArgs: ScTypeArgs): ScSubstitutor = {
    val typeArguments = typeArgs.typeArguments
    bind(typeParamsLike, typeArguments)
  }

  def bind[T: TypeParamId](typeParamsLike: Iterable[T], types: Iterable[ScType]): ScSubstitutor = {
    val tvMap = TypeParamSubstitution.buildMap(typeParamsLike, types)(identity)
    ScSubstitutor(tvMap)
  }

  def updateThisTypeDeep(subst: ScSubstitutor): Option[ScType] = {
    subst.substitutions.collectFirstByType[ThisTypeSubstitution, ScType](_.target)
  }

  def undefineTypeParams(tps: Seq[TypeParameter]): ScSubstitutor = bind(tps)(UndefinedType(_))
}
