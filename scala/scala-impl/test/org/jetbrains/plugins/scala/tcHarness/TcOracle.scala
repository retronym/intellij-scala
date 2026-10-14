package org.jetbrains.plugins.scala.tcHarness

import org.jetbrains.plugins.scala.DependencyManager
import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.tcHarness.TcProtocol.{Diagnostic, SourceFile}
import org.jetbrains.plugins.scala.util.TestUtils

import java.io.File
import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import scala.collection.mutable

/**
 * scalac (2 or 3) as the oracle, embedded in the test JVM. The compiler jars of a version are
 * loaded in a classloader isolated from the plugin's, and `testdata/tcHarness/Oracle{2,3}.scala`
 * is compiled by that compiler (once per version and oracle source; the classes are kept under
 * `~/.cache/tc-harness`).
 */
final class TcOracle private (impl: java.util.function.Function[Array[String], Array[String]]) {

  final case class Result(diagnostics: Seq[Diagnostic], answers: Seq[String], crash: Option[String])

  def run(files: Seq[SourceFile], queries: Seq[String]): Result = synchronized {
    val in = (files.size.toString +: files.flatMap(f => Seq(f.path, f.text))) ++ queries
    val out = impl.apply(in.toArray).toSeq.map(_.split("\t", -1).toSeq)
    Result(
      diagnostics = out.collect { case Seq("D", sev, path, line, col, msg*) => Diagnostic(path, line.toInt, col.toInt, sev, msg.mkString("\t")) },
      answers = out.collect { case Seq("A", a*) => a.mkString("\t") },
      crash = out.collectFirst { case Seq("X", m*) => m.mkString("\t") }
    )
  }
}

object TcOracle {

  private val oracles = mutable.Map.empty[(ScalaVersion, Seq[String], Seq[String]), TcOracle]

  def get(version: ScalaVersion, classpath: Seq[String], options: Seq[String]): TcOracle = synchronized {
    oracles.getOrElseUpdate((version, classpath, options), create(version, classpath, options))
  }

  /** The compiler's classpath, and the library part of it (what user code compiles against). */
  def jars(implicit version: ScalaVersion): (Seq[Path], Seq[Path]) = {
    val compiler =
      if (version.isScala3) DependencyManager.resolve(scalaCompilerDescription.transitive()).map(_.file)
      else DependencyManager.resolve(scalaCompilerDescription, scalaLibraryDescription, scalaReflectDescription).map(_.file)
    require(compiler.exists(_.getFileName.toString.matches("scala3?-compiler.*")), s"no compiler in $compiler")
    val library = compiler.filter(_.getFileName.toString.matches("scala3?-library.*"))
    (compiler, library)
  }

  private def create(version: ScalaVersion, classpath: Seq[String], options: Seq[String]): TcOracle = {
    implicit val v: ScalaVersion = version
    val (compilerJars, libraryJars) = jars
    val compilerLoader = new URLClassLoader(compilerJars.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
    val name = if (version.isScala3) "Oracle3" else "Oracle2"
    val source = Paths.get(TestUtils.getTestDataPath, "tcHarness", s"$name.scala")
    val out = compileOracle(compilerLoader, compilerJars, source)
    val implLoader = new URLClassLoader(Array[URL](out.toUri.toURL), compilerLoader)
    val userCp = (libraryJars.map(_.toString) ++ classpath).mkString(File.pathSeparator)
    val impl = implLoader.loadClass(s"tcoracle.$name")
      .getConstructor(classOf[String], classOf[Array[String]])
      .newInstance(userCp, options.toArray)
    new TcOracle(impl.asInstanceOf[java.util.function.Function[Array[String], Array[String]]])
  }

  private def compileOracle(loader: ClassLoader, compilerJars: Seq[Path], source: Path)(implicit version: ScalaVersion): Path = {
    val sha = MessageDigest.getInstance("SHA-1").digest(Files.readAllBytes(source)).take(6).map("%02x".format(_)).mkString
    val out = Paths.get(sys.props("user.home"), ".cache", "tc-harness", "oracle", s"${version.minor}-$sha")
    if (!Files.isDirectory(out.resolve("tcoracle"))) {
      val tmp = Files.createTempDirectory("tc-oracle")
      val args = Array("-classpath", compilerJars.mkString(File.pathSeparator), "-d", tmp.toString, source.toString)
      val ok =
        if (version.isScala3) {
          val main = loader.loadClass("dotty.tools.dotc.Main$")
          val reporter = main.getMethod("process", classOf[Array[String]]).invoke(main.getField("MODULE$").get(null), args)
          !reporter.getClass.getMethod("hasErrors").invoke(reporter).asInstanceOf[Boolean]
        } else {
          loader.loadClass("scala.tools.nsc.Main").getMethod("process", classOf[Array[String]]).invoke(null, args) == java.lang.Boolean.TRUE
        }
      require(ok, s"could not compile $source with ${version.minor}")
      Files.createDirectories(out.getParent)
      if (!Files.isDirectory(out)) Files.move(tmp, out)
    }
    out
  }
}
