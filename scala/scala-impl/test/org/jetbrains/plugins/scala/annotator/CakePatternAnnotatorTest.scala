package org.jetbrains.plugins.scala.lang.resolve

import org.intellij.lang.annotations.Language
import org.jetbrains.plugins.scala.annotator.{AnnotatorHolderMock, AnnotatorSimpleTestCase, Message, ScalaAnnotator}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

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
         |//      global.useTree(tree) // OK
         |      global.useTree(block) // NOK
         |    }
         |  }
         |}
         |""".stripMargin)) {
      case Nil =>
    }

  def messages(@Language(value = "Scala", prefix = Header) code: String): List[Message] = {
    val annotator = new ScalaAnnotator()
    val parse: ScalaFile = (Header + code).parse

    implicit val mock: AnnotatorHolderMock = new AnnotatorHolderMock(parse)

    parse.depthFirst().filterByType[ScExpression].foreach {
      annotator.annotate(_, typeAware = true)
    }

    mock.annotations
  }
}
