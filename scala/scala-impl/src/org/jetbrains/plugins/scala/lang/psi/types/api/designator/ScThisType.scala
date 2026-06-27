package org.jetbrains.plugins.scala.lang.psi.types.api
package designator

import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.{ConstraintSystem, ConstraintsResult, Context, LeafType, ScType, ScTypeExt, ScalaTypeVisitor}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.util.ScEquivalenceUtil

final case class ScThisType(override val element: ScTemplateDefinition) extends DesignatorOwner with LeafType {
  element.getClass
  //throw NPE if clazz is null...

  override val isSingleton = true

  override private[types] def designatorSingletonType = None

  override def equivInner(`type`: ScType, constraints: ConstraintSystem, falseUndef: Boolean)(implicit context: Context): ConstraintsResult = {
    (this, `type`) match {
      case (ScThisType(clazz1), ScThisType(clazz2)) =>
        if (ScEquivalenceUtil.areClassesEquivalent(clazz1, clazz2) || sameThisInstance(clazz1, clazz2)) constraints
        else ConstraintsResult.Left
      case (ScThisType(obj1: ScObject), ScDesignatorType(obj2: ScObject)) =>
        if (ScEquivalenceUtil.areClassesEquivalent(obj1, obj2)) constraints
        else ConstraintsResult.Left
      case (_, ScDesignatorType(_: ScObject)) =>
        ConstraintsResult.Left
      case (_, ScDesignatorType(typed: ScTypedDefinition)) if typed.isStable =>
        typed.`type`() match {
          case Right(tp: DesignatorOwner) if tp.isSingleton =>
            this.equiv(tp, constraints, falseUndef)
          case _ =>
            ConstraintsResult.Left
        }
      case (_, ScProjectionType(_, _: ScObject)) => ConstraintsResult.Left
      case (_, p@ScProjectionType(tp, elem: ScTypedDefinition)) if elem.isStable =>
        elem.`type`() match {
          case Right(singleton: DesignatorOwner) if singleton.isSingleton =>
            val newSubst = p.actualSubst.followed(ScSubstitutor(tp))
            this.equiv(newSubst(singleton), constraints, falseUndef)
          case _ => ConstraintsResult.Left
        }
      case _ => ConstraintsResult.Left
    }
  }

  /**
   * Two `this`-types denote the same enclosing instance when self types tie them
   * together (the cake pattern): each class's `this` is forced, via its self type,
   * to be an instance of the other. E.g. `trait Types { self: SymbolTable => }` and
   * `class SymbolTable extends Types` — `Types.this` and `SymbolTable.this` are one
   * and the same instance. scalac canonicalizes such `this` references to a single
   * symbol; IntelliJ does not, so override-matching of path-dependent members (the
   * scala/scala reflect cake, SCL-21947) must treat the two forms as equal.
   */
  private def sameThisInstance(c1: ScTemplateDefinition, c2: ScTemplateDefinition)
                              (implicit context: Context): Boolean = {
    def selfClass(c: ScTemplateDefinition): PsiClass = c.selfType.flatMap(_.extractClass).getOrElse(c)
    def isSameOrSub(a: PsiClass, b: PsiClass): Boolean =
      ScEquivalenceUtil.areClassesEquivalent(a, b) || a.isInheritor(b, /*checkDeep*/ true)
    isSameOrSub(selfClass(c1), c2) && isSameOrSub(selfClass(c2), c1)
  }

  override def visitType(visitor: ScalaTypeVisitor): Unit = visitor.visitThisType(this)
}
