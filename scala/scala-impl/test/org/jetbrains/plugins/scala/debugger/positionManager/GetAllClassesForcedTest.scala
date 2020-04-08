package org.jetbrains.plugins.scala
package debugger
package positionManager

import java.nio.charset.StandardCharsets

import org.jetbrains.plugins.scala.debugger.filters.ScalaDebuggerSettings
import org.junit.experimental.categories.Category

/**
 * @author Nikolay.Tropin
 */
@Category(Array(classOf[DebuggerTests]))
class GetAllClassesForceTest_since_2_12 extends GetAllClassesForceTestBase {
  override protected def supportedIn(version: ScalaVersion): Boolean = version >= Scala_2_12
}


abstract class GetAllClassesForceTestBase extends PositionManagerTestBase {
  private val synthetic = "//<SYNTHETIC>"

  setupFile("Simple.scala",
    s"""
      |object Simple {
      |  def main(args: Array[String]) {
      |    class syntheticClass1 { class syntheticClass2 { $synthetic
      |    $offsetMarker"" $bp
      |    }}                                              $synthetic
      |  }
      |}
    """.stripMargin.trim)

  private def removingSyntheticLineContents(fileName: String)(f: => Unit): Unit ={
    val virtualFile = getVirtualFile(getFileInSrc(fileName))
    val charset = StandardCharsets.UTF_8
    val origiContent = virtualFile.contentsToByteArray()
    val content = new String(origiContent, charset)
    val edited = content.linesIterator.map(line => if (line.contains(synthetic)) "" else line).mkString(System.lineSeparator)
    virtualFile.setBinaryContent(edited.getBytes(charset))
    try f finally {
      virtualFile.setBinaryContent(origiContent)
    }
  }
  
  def testSimple(): Unit = {
    val settings = ScalaDebuggerSettings.getInstance()
    val saved = settings.FORCE_POSITION_LOOKUP_IN_NESTED_TYPES
    val allClasses = Seq("Simple$", "Simple$syntheticClass1$1", "Simple$syntheticClass1$syntheticClass2$1")
    try {
      settings.FORCE_POSITION_LOOKUP_IN_NESTED_TYPES = false
      checkGetAllClasses(allClasses: _*)
      removingSyntheticLineContents("Simple.scala") {
        checkGetAllClasses("Simple$")
        settings.FORCE_POSITION_LOOKUP_IN_NESTED_TYPES = true
        checkGetAllClasses(allClasses: _*)
      }
    } finally {
      settings.FORCE_POSITION_LOOKUP_IN_NESTED_TYPES = saved
    }
  }
}