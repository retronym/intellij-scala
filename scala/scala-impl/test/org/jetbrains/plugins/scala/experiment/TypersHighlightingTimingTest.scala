package org.jetbrains.plugins.scala.experiment

import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.psi.PsiManager
import com.intellij.testFramework.LoggedErrorProcessor
import com.intellij.util.ThrowableRunnable
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.base.libraryLoaders.{LibraryLoader, LocalJarLibraryLoader, SmartJDKLoader}
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.pom.java.LanguageLevel
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.TypeRecursionGuard
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.{AsfStats => AsfStatsX}

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters._

// EXPERIMENT ONLY (not for commit). Times highlighting of scala/scala files mounted as a
// source root. Env: TIMING_SRC = merged src/reflect+src/compiler snapshot, TIMING_FILES =
// comma-separated paths relative to it, TIMING_OUT = result file, TIMING_REPS = warm reps.
class TypersHighlightingTimingTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_2_13)
  override protected def supportedIn(version: ScalaVersion): Boolean = version == ScalaVersion.Latest.Scala_2_13

  private def srcRoot: Path = Paths.get(sys.env("TIMING_SRC"))

  // A real JDK (java.desktop, java.xml, java.management, ...), not the mock one, and the compiler's own
  // dependencies (TIMING_JARS, comma-separated: scala-asm, ...), so that missing classes don't drown the
  // type errors.
  override protected def projectJdk: Sdk = SmartJDKLoader.getOrCreateJDK(LanguageLevel.JDK_17)

  override protected def additionalLibraries: Seq[LibraryLoader] =
    sys.env.get("TIMING_JARS").toSeq.flatMap(_.split(',')).filter(_.nonEmpty).zipWithIndex.map {
      case (jar, i) => LocalJarLibraryLoader(s"timing-lib-$i", Paths.get(jar))
    }

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
    // count recursion-guard trips per file instead of failing on the first one
    System.setProperty("scala.types.recursionGuard.failHard", sys.env.getOrElse("TIMING_GUARD_FAIL_HARD", "false"))
    val gate  = sys.env.getOrElse("SCPROJ_GATE", "on")
    val reps  = sys.env.getOrElse("TIMING_REPS", "3").toInt
    val files = sys.env.getOrElse("TIMING_FILES", "scala/tools/nsc/typechecker/Typers.scala").split(',').toSeq
    for (rel <- files) {
      val vf = LocalFileSystem.getInstance.refreshAndFindFileByNioFile(srcRoot.resolve(rel))
      assert(vf != null, rel)
      myFixture.configureFromExistingVirtualFile(vf)
      TypeRecursionGuard.resetTrips()
      // ASF_VARIANTS=base,bt,walk,both: interleave memo variants rep by rep in this JVM (fair A/B).
      val variants = sys.env.get("ASF_VARIANTS").map(_.split(',').toSeq).getOrElse(Seq("env"))
      val byVariant = scala.collection.mutable.LinkedHashMap[String, List[Long]]()
      def setVariant(v: String): Unit = v match {
        case "base" => AsfStatsX.cacheBaseType = false; AsfStatsX.cacheWalk = false; AsfStatsX.cacheCanon = false
        case "canon" => AsfStatsX.cacheBaseType = false; AsfStatsX.cacheWalk = false; AsfStatsX.cacheCanon = true
        case "all"  => AsfStatsX.cacheBaseType = true;  AsfStatsX.cacheWalk = true; AsfStatsX.cacheCanon = true
        case "bt"   => AsfStatsX.cacheBaseType = true;  AsfStatsX.cacheWalk = false
        case "walk" => AsfStatsX.cacheBaseType = false; AsfStatsX.cacheWalk = true
        case "both" => AsfStatsX.cacheBaseType = true;  AsfStatsX.cacheWalk = true
        case _      =>
      }
      val times = (0 to reps).flatMap(i => variants.map(v => (i, v))).map { case (i, variant) =>
        setVariant(variant)
        dropCaches()
        org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AsfStats.reset()
        val profile = sys.env.get("ASF_PROFILE").filter(_ => i == reps && reps > 0).map(_ + "-" + variant)
        def asprof(args: String*): Unit = {
          val pb = new ProcessBuilder(("/opt/homebrew/bin/asprof" +: args :+ ProcessHandle.current().pid().toString)*).inheritIO()
          pb.start().waitFor()
        }
        profile.foreach(_ => asprof("start", "-e", sys.env.getOrElse("ASF_EVENT", "cpu"), "-i", "1ms"))
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
        profile.foreach { f =>
          asprof("stop", "-o", "collapsed", "-f", f + ".collapsed")
        }
        if (org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AsfStats.enabled)
          out(s"STATS rep=$i variant=$variant file=$rel ms=$ms\n" + org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AsfStats.report())
        val errors = infos.filter(_.getSeverity == HighlightSeverity.ERROR)
        if (i > 0) byVariant(variant) = ms :: byVariant.getOrElse(variant, Nil)
        if (variants.size > 1) out(s"VARIANT $variant rep=$i ms=$ms errors=${infos.count(_.getSeverity == HighlightSeverity.ERROR)}")
        if (i == 0 && variant == variants.head) {
          out(s"ERRORS gate=$gate file=$rel count=${errors.size} loggedPlatformErrors=$logged guardTrips=${TypeRecursionGuard.trips}")
          errors.take(sys.env.getOrElse("TIMING_MAXERR", "40").toInt).foreach { e =>
            val line = getEditor.getDocument.getLineNumber(e.getStartOffset) + 1
            out(s"  ERR $rel:$line ${e.getDescription}")
          }
        }
        ms
      }
      byVariant.foreach { case (v, ts) => out(s"VARIANT-SUMMARY $v reps=${ts.reverse.mkString(",")} min=${ts.min} median=${ts.sorted.apply(ts.size / 2)}") }
      out(s"TIMING gate=$gate file=$rel first=${times.head}ms reps=${times.tail.mkString(",")}ms min=${times.tail.minOption.getOrElse(times.head)}ms")
    }
  }
}
