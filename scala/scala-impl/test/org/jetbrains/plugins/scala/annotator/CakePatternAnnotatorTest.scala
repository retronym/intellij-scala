package org.jetbrains.plugins.scala.annotator

import org.intellij.lang.annotations.Language
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall}

class CakePatternAnnotatorTest extends AnnotatorSimpleTestCase {

  final val Header = ""

  def testCake(): Unit =
    assertMatches(messages(
      s"""
         |object api {
         |  abstract class Universe extends Trees {
         |    def useTree(t: Tree) = ()
         |  }
         |
         |  trait Trees {
         |    self: Universe =>
         |    type Tree
         |    type Block <: Tree
         |  }
         |
         |  abstract class TreeGen {
         |    val global: Universe
         |  }
         |}
         |
         |object nsc {
         |  abstract class TreeGen extends api.TreeGen{
         |    self: Global =>
         |    val global: Global
         |  }
         |  abstract class Global extends api.Universe {
         |    val gen: TreeGen { val global: Global.this.type }
         |  }
         |
         |  abstract class RefChecks {
         |    val global: Global
         |    val tree: RefChecks.this.global.gen.global.Tree
         |    val block: RefChecks.this.global.gen.global.Block
         |    def testMe(): Unit = {
         |      global.useTree(tree) // OK
         |      global.useTree(block) // was NOK
         |    }
         |  }
         |}
         |""".stripMargin)) {
      case Nil =>
    }

  // TODO Fix the actual problem that is typeing the qualifier `global.gen.C` in the pattern match
  //      with an unstable type.
  def ignoreCakeyPatterns(): Unit =
    assertMatches(messages(
      s"""
         |object scala {
         |  class Option[+A]
         |}
         |abstract class ApiUniverse extends ApiTrees {
         |  def useTree(t: Tree) = ()
         |}
         |
         |trait ApiTrees {
         |  self: ApiUniverse =>
         |  type Tree
         |}
         |
         |abstract class ApiTreeGen {
         |  val global: ApiUniverse
         |  case class C(t: global.Tree)
         |  object D {
         |    def unapply(a: Any): scala.Option[global.Tree] = null
         |  }
         |}
         |
         |abstract class Global extends ApiUniverse {
         |  object gen extends { val global: Global.this.type = Global.this } with ApiTreeGen {}
         |
         |  new AnyRef match {
         |    case gen.C(t) => useTree(t) // OK
         |  }
         |}
         |
         |abstract class RefChecks {
         |  val global: Global
         |
         |  def test = {
         |    val C2 = global.gen.C
         |
         |    new AnyRef match {
         |      case global.gen.C(t1) => global.useTree(t1) // NOK
         |      case global.gen.D(t2) => global.useTree(t2) // NOK
         |      case C2(t3) => global.useTree(t3) // OK
         |    }
         |
         |    /* SCALAC:
         |      val C2: RefChecks.this.global.gen.C.type = RefChecks.this.global.gen.C;
         |      new java.lang.Object() match {
         |        case (t: RefChecks.this.global.gen.global.Tree): RefChecks.this.global.gen.C((t @ _)) => RefChecks.this.global.useTree(t)
         |        case RefChecks.this.global.gen.D.unapply(<unapply-selector>) <unapply> ((t @ _)) => RefChecks.this.global.useTree(t)
         |        case (t: RefChecks.this.global.gen.global.Tree): RefChecks.this.global.gen.C((t @ _)) => RefChecks.this.global.useTree(t)
         |      }
         |    }
         |     */
         |  }
         |}
         |
         |
         |""".stripMargin)) {
      case Nil =>
    }

  def messages(@Language(value = "Scala", prefix = Header) code: String): List[Message] = {
    val annotator = new ScalaAnnotator()
    val parse: ScalaFile = (Header + code).parse

    implicit val mock: AnnotatorHolderMock = new AnnotatorHolderMock(parse)

    parse.depthFirst().foreach {
      case t =>
        annotator.annotate(t, typeAware = true)
    }

    mock.annotations
  }
}
