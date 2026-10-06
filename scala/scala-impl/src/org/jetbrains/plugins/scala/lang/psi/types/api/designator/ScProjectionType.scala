package org.jetbrains.plugins.scala.lang.psi.types.api
package designator

import com.intellij.psi._
import org.jetbrains.plugins.scala.caches.{BlockModificationTracker, RecursionManager, cachedWithRecursionGuard}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScBindingPattern
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticClass
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.ScTypePolymorphicType
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.jetbrains.plugins.scala.lang.psi.types.{AliasType, ConstraintSystem, ConstraintsResult, Context, ScCompoundType, ScLiteralType, ScType, ScTypeExt, ScalaTypeVisitor}
import org.jetbrains.plugins.scala.lang.resolve.processor.ResolveProcessor
import org.jetbrains.plugins.scala.lang.resolve.{ResolveTargets, ScalaResolveResult, ScalaResolveState}
import org.jetbrains.plugins.scala.util.HashBuilder._
import org.jetbrains.plugins.scala.util.ScEquivalenceUtil

/**
 * This type means type projection:
 * SomeType#member
 * member can be class or type alias
 */
final class ScProjectionType private(val projected: ScType,
                                     override val element: PsiNamedElement) extends DesignatorOwner {

  override protected def calculateAliasType(implicit context: Context): Option[AliasType] = calculateAliasTypeAux(actualElement, actualSubst)

  override def isStable: Boolean = (projected match {
    case designatorOwner: DesignatorOwner => designatorOwner.isStable
    case _ => false
  }) && super.isStable

  // scalac's `pre.memberType(sym)`: the singleton's underlying follows the member resolved
  // on the prefix (`actual`), not the static `element`, which may be an abstract declaration
  // (`IGen#global: SymbolTable`) overridden by a singleton-typed member
  // (`val global: X.this.type`).
  override private[types] def designatorSingletonType: Option[ScType] = actualElement match {
    case _: ScObject                                          => None
    case parameter: ScParameter if parameter.isStable         => parameter.insideParamType.toOption.map(actualSubst)
    case definition: ScTypedDefinition if definition.isStable => definition.`type`().toOption.map(actualSubst)
    case _                                                    => None
  }

  private def actualImpl(projected: ScType, updateWithProjectionSubst: Boolean)(implicit context: Context): Option[(PsiNamedElement, ScSubstitutor)] = cachedWithRecursionGuard("actualImpl", element, Option.empty[(PsiNamedElement, ScSubstitutor)], BlockModificationTracker(element), (projected, updateWithProjectionSubst)) {
    val resolvePlace = {
      def fromClazz(definition: ScTypeDefinition): PsiElement =
        definition.extendsBlock.templateBody
          .flatMap(_.lastChildStub)
          .getOrElse(definition.extendsBlock)

      projected.tryExtractDesignatorSingleton.extractClass match {
        case Some(definition: ScTypeDefinition) => fromClazz(definition)
        case _ =>
          projected match {
            case ScThisType(definition: ScTypeDefinition) => fromClazz(definition)
            case _                                        => element
          }
      }
    }

    import org.jetbrains.plugins.scala.lang.resolve.ResolveTargets._
    def processType(kinds: Set[ResolveTargets.Value] = ValueSet(CLASS)): Option[(PsiNamedElement, ScSubstitutor)] = {
      def elementClazz: Option[PsiClass] = element match {
        case named: ScBindingPattern => Option(named.containingClass)
        case member: ScMember        => Option(member.containingClass)
        case _                       => None
      }

      projected match {
        case ScDesignatorType(clazz: PsiClass)
          if elementClazz.exists(ScEquivalenceUtil.areClassesEquivalent(_, clazz)) =>
          return Some(element, ScSubstitutor(projected, clazz))
        case p @ ParameterizedType(ScDesignatorType(clazz: PsiClass), _)
          if elementClazz.exists(ScEquivalenceUtil.areClassesEquivalent(_, clazz)) =>
          return Some(element, ScSubstitutor(projected, clazz).followed(p.substitutor))
        case p: ScProjectionType =>
          p.actualElement match {
            case `element` if element.is[ScTypeAlias] => //rare case of recursive projection, see SCL-15345
              return Some(element, p.actualSubst)
            case clazz: PsiClass
              if elementClazz.exists(ScEquivalenceUtil.areClassesEquivalent(_, clazz)) =>
              return Some(element, ScSubstitutor(projected, clazz).followed(p.actualSubst))
            case _ => //continue with processor :(
          }
        case ScThisType(clazz)
          if elementClazz.exists(ScEquivalenceUtil.areClassesEquivalent(_, clazz)) =>
          //for this type we shouldn't put this substitutor because of possible recursions
          //and we don't need that, because all types are already calculated with proper this type
          return Some(element, ScSubstitutor.empty)
        case ScCompoundType(_, _, typesMap) =>
          typesMap.get(element.name) match {
            case Some(taSig) => return Some(taSig.typeAlias, taSig.substitutor)
            case _           =>
          }
        case _ => //continue with processor :(
      }


      val processor = new ResolveProcessor(kinds, resolvePlace, element.name) {
        doNotCheckAccessibility()

        override protected def addResults(results: Iterable[ScalaResolveResult]): Boolean = {
          candidatesSet ++= results
          true
        }
      }

      processor.processType(projected, resolvePlace, ScalaResolveState.empty, updateWithProjectionSubst)

      ScProjectionType.mostSpecific(processor.candidates) match {
        case Some(candidate) => candidate.element match {
          case candidateElement: PsiNamedElement =>
            if (ScProjectionType.debugMemberType && processor.candidates.length > 1)
              System.err.println(s"[memberType] $this: ${processor.candidates.map(c => c.element.name + "@" + c.element.findContextOfType(classOf[PsiClass]).map(_.name).orNull).mkString(", ")} -> ${candidateElement.findContextOfType(classOf[PsiClass]).map(_.name).orNull}")
            // scalac's `sym.info.asSeenFrom(pre, sym.owner)`: anchor at the *resolved* member's owner,
            // which for an override is not the static `element`'s.
            val anchorElement = if (element.is[PsiClass]) element else candidateElement
            val thisSubstitutor = ScSubstitutor(projected, anchorElement.findContextOfType(classOf[PsiClass]).orNull)
            val defaultSubstitutor =
              projected match {
                case _: ScThisType => candidate.substitutor
                case _ => thisSubstitutor.followed(candidate.substitutor)
              }
            val needSuperSubstitutor = element match {
              case _: PsiClass => element != candidateElement
              case _ => false
            }
            if (needSuperSubstitutor) {
              Some(element,
                ScalaPsiUtil.superTypeSignatures(candidateElement)
                  .find(_.namedElement == element)
                  .map(typeSig => typeSig.substitutor.followed(defaultSubstitutor))
                  .getOrElse(defaultSubstitutor))

            } else {
              Some(candidateElement, defaultSubstitutor)
            }
          case null => None
        }
        case _ => None
      }
    }

    element match {
      case d: ScTypedDefinition if d.isStable => //val's, objects, parameters
        processType(ValueSet(VAL, OBJECT))
      case _: ScTypeAlias | _: PsiClass =>
        processType(ValueSet(CLASS))
      case _ => None
    }
  }

  private def actual(updateWithProjectionSubst: Boolean = true)(implicit context: Context): (PsiNamedElement, ScSubstitutor) =
    actualImpl(projected, updateWithProjectionSubst).getOrElse(element, ScSubstitutor.empty)

  def actualElement: PsiNamedElement = actual()._1
  def actualSubst: ScSubstitutor = actual()._2

  override def equivInner(r: ScType, constraints: ConstraintSystem, falseUndef: Boolean)(implicit context: Context): ConstraintsResult = {
    def isEligibleForPrefixUnification(proj: ScType): Boolean = proj.subtypeExists {
      case _: UndefinedType => true
      case _                => false
    }

    def checkDesignatorType(e: PsiNamedElement, other: ScType): ConstraintsResult = e match {
      case td: ScTypedDefinition if td.isStable =>
        val tp = actualSubst(td.`type`().getOrAny)
        tp match {
          case designatorOwner: DesignatorOwner if designatorOwner.isSingleton =>
            tp.equiv(other, constraints, falseUndef)
          case lit: ScLiteralType => lit.equiv(other, constraints, falseUndef)
          case _                  => ConstraintsResult.Left
        }
      case _ => ConstraintsResult.Left
    }

    val desRes = checkDesignatorType(actualElement, r)
    if (desRes.isRight) return desRes

    r match {
      case tpt: ScTypePolymorphicType =>
        return ScEquivalenceUtil
          .isTypeConstructorEquivalentToPolyType(this, tpt, constraints, falseUndef)
          .getOrElse(ConstraintsResult.Left)
      case _ => ()
    }

    val res = r match {
      case t: StdType =>
        element match {
          case synth: ScSyntheticClass => synth.stdType.equiv(t, constraints, falseUndef)
          case _                       => ConstraintsResult.Left
        }
      case ParameterizedType(ScProjectionType(_, _), _) =>
        r match {
          case AliasType(_: ScTypeAliasDefinition, Right(lower), _, effectivelyOpaque) if !effectivelyOpaque =>
            this.equiv(lower, constraints, falseUndef)
          case _ => ConstraintsResult.Left
        }
      case proj2 @ ScProjectionType(p1, _) =>
        val desRes = checkDesignatorType(proj2.actualElement, this)
        if (desRes.isRight) return desRes

        val lElement = actualElement
        val rElement = proj2.actualElement

        val sameElements = ScEquivalenceUtil.smartEquivalence(lElement, rElement) || {
          lElement.name == rElement.name &&
            (isEligibleForPrefixUnification(projected) || isEligibleForPrefixUnification(p1))
        }

        if (sameElements) projected.equiv(p1, constraints, falseUndef)
        else
          r match {
            case AliasType(_: ScTypeAliasDefinition, Right(lower), _, effectivelyOpaque) if !effectivelyOpaque =>
              this.equiv(lower, constraints, falseUndef)
            case _ => ConstraintsResult.Left
          }
      case thisType @ ScThisType(thisClazz) =>
        element match {
          case _: ScObject                        => ConstraintsResult.Left
          case t: ScTypedDefinition if t.isStable =>
            t.`type`() match {
              case Right(singleton: DesignatorOwner) if singleton.isSingleton =>
                val newSubst = actualSubst.followed(ScSubstitutor(projected, ScSubstitutor.declarationAnchor(t)))
                r.equiv(newSubst(singleton), constraints, falseUndef)
              // Cake-pattern stable path: `pre.global` (this projection) where `global: Global`
              // (not singleton-typed) vs `Global.this`. When the val's type class matches the
              // this-type's class, they denote the same instance. (SCL-21947)
              case Right(tp) =>
                tp.extractClass match {
                  case Some(cls) if ScEquivalenceUtil.areClassesEquivalent(thisClazz, cls) => constraints
                  case _ => ConstraintsResult.Left
                }
              case _ => ConstraintsResult.Left
            }
          case _ => ConstraintsResult.Left
        }
      case _ => ConstraintsResult.Left
    }

    res match {
      case cs: ConstraintSystem   => cs
      case ConstraintsResult.Left =>
        this match {
          case AliasType(_: ScTypeAliasDefinition, Right(lower), _, effectivelyOpaque) if !effectivelyOpaque =>
            lower.equiv(r, constraints, falseUndef)
          case _ => ConstraintsResult.Left
        }
    }
  }

  override def isFinalType(implicit context: Context): Boolean = actualElement match {
    case cl: PsiClass if cl.isEffectivelyFinal => true
    case alias: ScTypeAliasDefinition if !alias.isEffectivelyOpaque => alias.aliasedType.exists(_.isFinalType)
    case _                                     => false
  }

  override def visitType(visitor: ScalaTypeVisitor): Unit = visitor.visitProjectionType(this)

  def canEqual(other: Any): Boolean = other.is[ScProjectionType]

  override def equals(other: Any): Boolean = other match {
    case that: ScProjectionType =>
      (that `canEqual` this) &&
        projected == that.projected &&
        element == that.element
    case _ => false
  }

  private var hash: Int = -1

  //noinspection HashCodeUsesVar
  override def hashCode: Int = {
    if (hash == -1)
      hash = projected #+ element

    hash
  }

  override def typeDepth: Int = projected.typeDepth
}

object ScProjectionType {

  private val guard = RecursionManager.RecursionGuard[ScType, Nothing]("aliasProjectionGuard")

  private[designator] val debugMemberType: Boolean = java.lang.Boolean.getBoolean("scala.debug.memberType")

  /**
   * scalac's `pre.member(name)` picks the most specific member; IntelliJ's resolver may
   * return several candidates for one name (an abstract `type Symbol` and the `class Symbol`
   * realizing it through a self type; an abstract `val global` and its override). Pick the
   * single candidate that no other one overrides: a concrete member over an abstract one, a
   * member of a subclass over one of its superclass. `None` when that isn't unique.
   */
  private def mostSpecific(candidates: Array[ScalaResolveResult]): Option[ScalaResolveResult] = candidates match {
    case Array(c) => Some(c)
    case Array()  => None
    case cs =>
      def owner(r: ScalaResolveResult): PsiClass = r.element.findContextOfType(classOf[PsiClass]).orNull
      def concrete(r: ScalaResolveResult): Boolean = ScalaPsiUtil.isConcreteElement(r.element.nameContext)
      def overrides(a: ScalaResolveResult, b: ScalaResolveResult): Boolean =
        (concrete(a) && !concrete(b)) || (concrete(a) == concrete(b) && {
          val (oa, ob) = (owner(a), owner(b))
          oa != null && ob != null && oa != ob && oa.isInheritor(ob, /*checkDeep*/ true)
        })
      cs.filterNot(b => cs.exists(a => (a ne b) && overrides(a, b))) match {
        case Array(c) => Some(c)
        case _        => None
      }
  }

  private[designator] def isSingletonLike(t: ScType): Boolean = t match {
    case _: ScThisType      => true
    case d: DesignatorOwner => d.isSingleton
    case _                  => false
  }

  /**
   * Collapse a stable val-path projection to the singleton it is known to be, to a
   * fixpoint: `global.analyzer.global`, where `analyzer`'s type refines
   * `val global: Global.this.type`, normalizes to `global`. scalac follows a path's
   * singleton type when comparing prefixes of path-dependent types; without this, a cake
   * member reached through such an alias yields an unreduced prefix and a spurious
   * mismatch (SCL-21947, `Infer.inferTypedPattern`). `fuel` bounds the walk.
   */
  @annotation.tailrec
  private[types] def collapseSingletonPath(tp: ScType, fuel: Int = 8): ScType = tp match {
    case proj: ScProjectionType if fuel > 0 =>
      val stable = proj.element match {
        case d: ScTypedDefinition => d.isStable
        case _                    => false
      }
      if (!stable) tp
      else projectionSingleton(proj) match {
        case Some(singleton) if singleton ne proj => collapseSingletonPath(singleton, fuel - 1)
        case _                                    => tp
      }
    case _ => tp
  }

  /**
   * The singleton type of the stable path `proj`: its own (override-aware)
   * `designatorSingletonType`, or else, when the member resolved to an abstract
   * declaration (`Analyzer#global: Global`), the singleton its prefix's refinement
   * declares (`new { val global: Global.this.type } with Analyzer`).
   */
  private def projectionSingleton(proj: ScProjectionType): Option[ScType] =
    proj.designatorSingletonType.filter(isSingletonLike).orElse {
      proj.projected match {
        case pp: ScProjectionType =>
          pp.designatorSingletonType match {
            case Some(ct: ScCompoundType) =>
              ct.signatureMap.iterator.collectFirst {
                case (sig, tpe) if sig.name == proj.element.name && isSingletonLike(proj.actualSubst(tpe)) => proj.actualSubst(tpe)
              }
            case _ => None
          }
        case _ => None
      }
    }

  def simpleAliasProjection(p: ScProjectionType): ScType = {
    p.actual() match {
      case (td: ScTypeAliasDefinition, subst) if td.typeParameters.isEmpty =>
        val upper = guard.doPreventingRecursion(p) {
          td.upperBound.map(subst).toOption
        }
        upper
          .flatten
          .filter(_.typeDepth < p.typeDepth)
          .getOrElse(p)
      case _ => p
    }
  }

  def apply(projected: ScType, element: PsiNamedElement): ScType = {

    val simple = new ScProjectionType(projected, element)
    simple.actualElement match {
      case td: ScTypeAliasDefinition if td.typeParameters.isEmpty =>
        val manager = ScalaPsiManager.instance(element.getProject)
        manager.simpleAliasProjectionCached(simple).nullSafe.getOrElse(simple)
      case _ => simple
    }
  }

  def unapply(proj: ScProjectionType): Some[(ScType, PsiNamedElement)] = {
    Some(proj.projected, proj.element)
  }

  object withActual {
    private val extractor = new withActual(true)

    def unapply(proj: ScProjectionType): Some[(PsiNamedElement, ScSubstitutor)] = extractor.unapply(proj)
  }

  class withActual(updateWithProjectionSubst: Boolean) {
    def unapply(proj: ScProjectionType)(implicit context: Context): Some[(PsiNamedElement, ScSubstitutor)] =
      Some(proj.actual(updateWithProjectionSubst))
  }
}
