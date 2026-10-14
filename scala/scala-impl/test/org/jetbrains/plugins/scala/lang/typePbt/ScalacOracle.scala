package org.jetbrains.plugins.scala.lang.typePbt

import org.jetbrains.plugins.scala.{DependencyManager, ScalaVersion}
import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.util.TestUtils

import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.util.function.BiFunction

/**
 * scalac as the oracle, embedded in the test JVM. The compiler jars of `version`
 * are loaded in a classloader isolated from the plugin's (whose Scala is 3), and
 * the oracle's code (`testdata/typePbt/OracleImpl.scala`) is compiled by that
 * scalac at start-up. The two sides talk through `BiFunction[String, Array[String],
 * Array[String]]`; see `OracleImpl` for the protocol.
 */
final class ScalacOracle private (impl: BiFunction[String, Array[String], Array[String]]) {

  /** `Left(errors)` if scalac rejects `source`, else one answer per query. */
  def ask(source: String, queries: Seq[String]): Either[Seq[String], Seq[String]] = {
    val result = impl.apply(source, queries.toArray).toSeq
    if (result.head == "OK") Right(result.tail) else Left(result.tail)
  }
}

object ScalacOracle {
  private var instance: ScalacOracle = null

  def get(version: ScalaVersion): ScalacOracle = synchronized {
    if (instance == null) instance = create(version)
    instance
  }

  private def create(implicit version: ScalaVersion): ScalacOracle = {
    val jars: Seq[Path] = DependencyManager
      .resolve(scalaCompilerDescription, scalaLibraryDescription, scalaReflectDescription)
      .map(_.file)
    require(jars.exists(_.getFileName.toString.contains("scala-compiler")), s"no scala-compiler in $jars")
    val compilerLoader = new URLClassLoader(jars.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)

    val implSource = Paths.get(TestUtils.getTestDataPath, "typePbt", "OracleImpl.scala")
    val out = Files.createTempDirectory("pbt-oracle")
    val cp = jars.mkString(java.io.File.pathSeparator)
    val main = compilerLoader.loadClass("scala.tools.nsc.Main")
    val ok = main.getMethod("process", classOf[Array[String]])
      .invoke(null, Array("-classpath", cp, "-d", out.toString, implSource.toString))
    require(ok == java.lang.Boolean.TRUE, s"could not compile $implSource")

    val implLoader = new URLClassLoader(Array[URL](out.toUri.toURL), compilerLoader)
    val libraryCp = jars.filterNot(_.getFileName.toString.contains("scala-compiler")).mkString(java.io.File.pathSeparator)
    val impl = implLoader.loadClass("pbtoracle.OracleImpl").getConstructor(classOf[String]).newInstance(libraryCp)
    new ScalacOracle(impl.asInstanceOf[BiFunction[String, Array[String], Array[String]]])
  }
}
