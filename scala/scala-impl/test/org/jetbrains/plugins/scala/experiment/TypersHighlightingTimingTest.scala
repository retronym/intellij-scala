package org.jetbrains.plugins.scala.experiment

import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.psi.PsiManager
import com.intellij.testFramework.LoggedErrorProcessor
import com.intellij.util.ThrowableRunnable
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters._

// EXPERIMENT ONLY (not for commit). Times highlighting of scala/scala files mounted as a
// source root. Env: TIMING_SRC = merged src/reflect+src/compiler snapshot, TIMING_FILES =
// comma-separated paths relative to it, TIMING_OUT = result file, TIMING_REPS = warm reps.
class TypersHighlightingTimingTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_2_13)
  override protected def supportedIn(version: ScalaVersion): Boolean = version == ScalaVersion.Latest.Scala_2_13

  private def srcRoot: Path = Paths.get(sys.env("TIMING_SRC"))

  override protected def sourceRootPath: Path = srcRoot

  private def out(line: String): Unit = {
    println(line)
    sys.env.get("TIMING_OUT").foreach { f =>
      Files.writeString(Paths.get(f), line + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    }
  }

  private def dropCaches(): Unit = {
    ScalaPsiManager.instance(getProject).clearAllCachesAndWait()
    PsiManager.getInstance(getProject).dropPsiCaches()
  }

  def testTiming(): Unit = {
    val gate  = sys.env.getOrElse("SCPROJ_GATE", "on")
    val reps  = sys.env.getOrElse("TIMING_REPS", "3").toInt
    val files = sys.env.getOrElse("TIMING_FILES", "scala/tools/nsc/typechecker/Typers.scala").split(',').toSeq
    for (rel <- files) {
      val vf = LocalFileSystem.getInstance.refreshAndFindFileByNioFile(srcRoot.resolve(rel))
      assert(vf != null, rel)
      myFixture.configureFromExistingVirtualFile(vf)
      val times = (0 to reps).map { i =>
        dropCaches()
        val t0 = System.nanoTime()
        // Swallow (and count) platform errors logged during highlighting, e.g. an upstream
        // UAST mismatch in InlayHintsPass, so they don't fail the timing run.
        var logged = 0
        var infosJ: java.util.List[com.intellij.codeInsight.daemon.impl.HighlightInfo] = null
        LoggedErrorProcessor.executeWith(new LoggedErrorProcessor {
          override def processError(category: String, message: String, details: Array[String], t: Throwable): java.util.Set[LoggedErrorProcessor.Action] = {
            logged += 1
            if (logged <= 3) out(s"  LOGGED ${message.take(160)}")
            java.util.EnumSet.noneOf(classOf[LoggedErrorProcessor.Action])
          }
        }, (() => infosJ = myFixture.doHighlighting()): ThrowableRunnable[RuntimeException])
        val infos = infosJ.asScala
        val ms = (System.nanoTime() - t0) / 1000000
        val errors = infos.filter(_.getSeverity == HighlightSeverity.ERROR)
        if (i == 0) {
          out(s"ERRORS gate=$gate file=$rel count=${errors.size} loggedPlatformErrors=$logged")
          errors.take(40).foreach { e =>
            val line = getEditor.getDocument.getLineNumber(e.getStartOffset) + 1
            out(s"  ERR $rel:$line ${e.getDescription}")
          }
        }
        ms
      }
      out(s"TIMING gate=$gate file=$rel first=${times.head}ms reps=${times.tail.mkString(",")}ms min=${times.tail.min}ms")
    }
  }
}
