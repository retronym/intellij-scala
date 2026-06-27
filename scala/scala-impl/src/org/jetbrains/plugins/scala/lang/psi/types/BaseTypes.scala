package org.jetbrains.plugins.scala.lang.psi.types

import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.extensions.PsiTypeExt
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{ScDesignatorType, ScProjectionType, ScThisType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor

import java.util
import scala.annotation.tailrec
import scala.collection.mutable

object BaseTypes {

  def iterator(tp: ScType)(implicit context: Context): Iterator[ScType] = new BaseTypesIterator(tp)

  def get(t: ScType)(implicit context: Context): Seq[ScType] = reduce(iterator(t).toList)

  /**
   * The base type of `t` at class `clazz`, with same-symbol contributions merged —
   * the analogue of scalac's `t baseType clazz` (used by `AsSeenFromMap`). When `t`
   * reaches `clazz` through several parents with different arguments/prefixes, the
   * contributions are combined with `glb`, which performs scalac's variance-aware
   * `mergePrefixAndArgs` (covariant -> glb of args, contravariant -> lub). This is
   * deterministic, unlike `iterator(t).find(_.extractClass.contains(clazz))`.
   */
  def baseType(t: ScType, clazz: PsiClass)(implicit context: Context): Option[ScType] = {
    val sameClass = (Iterator(t) ++ iterator(t)).filter(_.extractClass.contains(clazz)).toList
    if (sameClass.isEmpty) None
    else Some(mergeSameClass(sameClass, clazz))
  }

  /**
   * Merge several base types of the same class into one — scalac's
   * `mergePrefixAndArgs`: combine arguments per position by the class's variance
   * (covariant -> glb, contravariant -> lub, invariant -> kept). IntelliJ's plain
   * `glb` does NOT do this — for incomparable args it yields the intersection of
   * the applied types (`Box[Dog] with Box[Cat]`) rather than the merge
   * (`Box[Dog with Cat]`), so we do it explicitly.
   */
  private def mergeSameClass(types: Seq[ScType], clazz: PsiClass)(implicit context: Context): ScType =
    if (types.lengthCompare(1) <= 0) types.head
    else clazz match {
      case owner: ScTypeParametersOwner =>
        val variances = owner.typeParameters.map(_.variance)
        types.reduce { (a, b) =>
          (a, b) match {
            case (ParameterizedType(designator, as), ParameterizedType(_, bs))
                if as.sizeCompare(bs) == 0 && as.sizeCompare(variances) == 0 =>
              val merged = variances.indices.map { i =>
                val v = variances(i)
                if (v.isCovariant) as(i).glb(bs(i))
                else if (v.isContravariant) as(i).lub(bs(i))
                else as(i) // invariant: contributions are equivalent
              }
              ScParameterizedType(designator, merged)
            case _ => a.glb(b)
          }
        }
      case _ => types.reduce((a, b) => a.glb(b))
    }

  /**
   * Ordered, deduplicated, same-symbol-merged base type sequence — one entry per
   * base class, more-derived classes first (an order consistent with subtyping).
   * Mirrors scalac's `baseTypeSeq` (modulo the exact symbol-id tie-break among
   * unrelated classes, which is deterministic here but by base-class count + name).
   */
  def baseTypeSeq(t: ScType)(implicit context: Context): Seq[ScType] = {
    val all = (Iterator(t) ++ iterator(t)).toList
    val perClass = all.flatMap(tp => tp.extractClass.map(_ -> tp)).groupBy(_._1)
    val merged = perClass.toSeq.map { case (c, ps) => mergeSameClass(ps.map(_._2), c) }
    merged.sortBy { tp =>
      val name = tp.extractClass.flatMap(c => Option(c.getQualifiedName)).getOrElse("")
      (-baseClassCount(tp), name)
    }
  }

  /** Number of transitive base classes — a subtyping-consistent ordering key
   *  (a subtype has a superset of its supertype's base classes). */
  private def baseClassCount(t: ScType)(implicit context: Context): Int =
    t.extractClass match {
      case Some(c) =>
        val seen = mutable.Set.empty[PsiClass]
        def go(c: PsiClass): Unit = if (seen.add(c)) c.getSupers.foreach(go)
        go(c)
        seen.size
      case None => 0
    }

  // One base type per class. Same-class contributions are *merged*
  // (mergeSameClass) rather than the previous "keep the most specific arm", so a
  // class reached via several paths with different arguments yields the variance
  // merge (e.g. Box[Dog with Cat]) instead of a single arm (Box[Dog] or Box[Cat]).
  private def reduce(types: Seq[ScType])(implicit context: Context): Seq[ScType] =
    types
      .flatMap(t => t.extractClass.map(_ -> t))
      .groupBy(_._1)
      .iterator
      .map { case (clazz, ps) => mergeSameClass(ps.map(_._2), clazz) }
      .toList
}

private class BaseTypesIterator(tp: ScType)(implicit context: Context) extends Iterator[ScType] {
  import tp.projectContext

  private val initialCapacity = 4
  private val queue = new util.ArrayDeque[ScType](initialCapacity)
  private val visitedAliases = mutable.Set.empty[ScTypeAlias]
  private val seenTypes = mutable.Set.empty[ScType]

  enqueueSupers(tp)

  override def hasNext: Boolean = !queue.isEmpty

  override def next(): ScType = {
    val tp = queue.pollLast()
    enqueueSupers(tp)
    tp
  }

  private def enqueue(tp: ScType): Unit = {
    if (!seenTypes.contains(tp)) {
      queue.addFirst(tp)
      seenTypes += tp
    }
  }

  private def enqueueSupersForClass(c: PsiClass, substitutor: ScSubstitutor = ScSubstitutor.empty): Unit = {
    val superTypes = c match {
      case td: ScTemplateDefinition => td.superTypes
      case _ => c.getSuperTypes.toSeq.map(_.toScType())
    }
    superTypes.foreach { st =>
      val substed = substitutor(st)
      enqueue(substed)
    }
  }

  @tailrec
  private def enqueueSupers(t: ScType): Unit = {
    t match {
      case InterestedIn(r) => enqueueSupers(r)
      case ClassType(c, subst) => enqueueSupersForClass(c, subst)
      case JavaArrayType(_) => enqueue(Any)
      case ScCompoundType(comps, _, _) => comps.foreach(enqueue)
      case _ =>
    }
  }

  private object IsTypeAlias {
    def unapply(tp: ScType): Option[(ScTypeAliasDefinition, ScSubstitutor)] = tp match {
      case ScDesignatorType(ta: ScTypeAliasDefinition) => Some((ta, ScSubstitutor.empty))
      case ScProjectionType.withActual((ta: ScTypeAliasDefinition, actualSubst)) => Some((ta, actualSubst))
      case ParameterizedType(ScDesignatorType(ta: ScTypeAliasDefinition), args) =>
        val genericSubst = ScSubstitutor.bind(ta.typeParameters, args)
        Some((ta, genericSubst))
      case ParameterizedType(ScProjectionType.withActual(ta: ScTypeAliasDefinition, actualSubst), args) =>
        val genericSubst = ScSubstitutor.bind(ta.typeParameters, args)
        val s = actualSubst.followed(genericSubst)
        Some((ta, s))
      case _ => None
    }
  }

  private object ClassType {
    def unapply(tp: ScType): Option[(PsiClass, ScSubstitutor)] = tp match {
      case ScDesignatorType(c: PsiClass) => Some((c, ScSubstitutor.empty))
      case p : ScParameterizedType =>
        p.designator.extractClass match {
          case Some(clazz) => Some((clazz, p.substitutor))
          case _ => None
        }
      case ScProjectionType.withActual(c: PsiClass, subst) =>
        Some((c, subst))
      case _ => None
    }
  }

  private object InterestedIn {
    def unapply(tp: ScType): Option[ScType] = {
      if (seenTypes.contains(tp)) return None
      seenTypes += tp

      tp match {
        case IsTypeAlias(ta, s) if !ta.isEffectivelyOpaque =>
          if (!visitedAliases.contains(ta)) {
            visitedAliases += ta.physical
            ta.aliasedType match {
              case Right(aliased) => Some(s(aliased))
              case _ => None
            }
          }
          else None
        case ScThisType(clazz) =>
          // Given:
          //   trait Father[A] { trait Son }
          //   trait Charles extends Father[Int] {
          //     trait William extends Father[String] with Son
          //   }
          // then William.this.type.baseType(trait Son)
          // should return Charles.this.Son not Charles#Son
          // (what `clazz.getTypeWithProjections()` returns)
          val classType = clazz.`type`().toOption
          // `X.this` is known to satisfy `X`'s self type, so its base types must
          // include the self type's bases too. Without this, types/members reachable
          // only via the self type are missed — e.g. defeating the seenFromClass walk
          // in ThisTypeSubstitution (`BaseTypes.iterator(target).find(...)`).
          val selfType = clazz match {
            case td: ScTemplateDefinition => td.selfType
            case _                        => None
          }
          (classType, selfType) match {
            case (Some(ct), Some(st)) => Some(ScCompoundType(Seq(ct, st)))
            case (Some(ct), None)     => Some(ct)
            case (None, st)           => st
          }
        case tpt: TypeParameterType =>
          Some(tpt.upperType)
        case ScExistentialArgument(_, Nil, _, upper) =>
          Some(upper)
        case ex: ScExistentialType =>
          Some(ex.quantified.unpackedType)
        case _ => None
      }
    }
  }
}
