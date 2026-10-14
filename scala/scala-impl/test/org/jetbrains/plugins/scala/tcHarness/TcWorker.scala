package org.jetbrains.plugins.scala.tcHarness

import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.application.WriteAction
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.pom.java.LanguageLevel
import com.intellij.psi.{PsiClass, PsiDocumentManager, PsiElement, PsiFile}
import com.intellij.testFramework.{IdeaTestUtil, LoggedErrorProcessor}
import com.intellij.util.ThrowableRunnable
import com.intellij.util.lang.JavaVersion
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{LibraryLoader, LocalJarLibraryLoader, SmartJDKLoader}
import org.jetbrains.plugins.scala.base.{ScalaLightCodeInsightFixtureTestCase, SharedTestProjectToken}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.{BaseTypes, Context, ScType, ScTypeExt, TypePresentationContext}
import org.jetbrains.plugins.scala.tcHarness.TcProtocol.*

import java.nio.file.Paths
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * One warm light project for one [[TcProtocol.Env]]. `setUp` is called once by [[TcServerTest]]
 * when a request first needs this environment, `tearDown` when a request needs another one.
 * Between requests the project stays open: only the request's source files are replaced.
 */
class TcWorker(val env: Env) extends ScalaLightCodeInsightFixtureTestCase {

  setName("testWorker")
  def testWorker(): Unit = ()

  def start(): Unit = setUp()
  def stop(): Unit = tearDown()

  val scalaVersion: ScalaVersion = TcWorker.parseVersion(env.scalaVersion)
  injectedScalaVersion = scalaVersion

  override protected def projectJdk: Sdk = env.jdk match {
    case "mock" => IdeaTestUtil.getMockJdk(JavaVersion.compose(17))
    case n      => SmartJDKLoader.getOrCreateJDK(Option(LanguageLevel.parse(n)).getOrElse(throw new IllegalArgumentException(s"bad jdk $n")))
  }

  override protected def additionalLibraries: Seq[LibraryLoader] =
    env.classpath.zipWithIndex.map { case (jar, i) => LocalJarLibraryLoader(s"tc-lib-$i", Paths.get(jar)) }

  override protected def additionalCompilerOptions: Seq[String] = env.scalacOptions

  override protected def sharedProjectToken: SharedTestProjectToken = SharedTestProjectToken(("tc-harness", env))

  /** Must run on the EDT. */
  def check(req: Request): (Seq[Diagnostic], Seq[String], Seq[String]) = {
    val sourceRoot = ModuleRootManager.getInstance(getModule).getSourceRoots.head
    WriteAction.run[Throwable](() => sourceRoot.getChildren.foreach(_.delete(this)))
    val psiFiles = req.files.map(f => myFixture.addFileToProject(f.path, f.text))

    val logged = mutable.ArrayBuffer.empty[String]
    val diagnostics = mutable.ArrayBuffer.empty[Diagnostic]
    for ((f, psi) <- req.files.zip(psiFiles) if f.path.endsWith(".scala") && !f.path.startsWith(TcWorker.SyntheticPrefix)) {
      myFixture.openFileInEditor(psi.getVirtualFile)
      var infos: java.util.List[HighlightInfo] = null
      LoggedErrorProcessor.executeWith(new LoggedErrorProcessor {
        override def processError(category: String, message: String, details: Array[String], t: Throwable): java.util.Set[LoggedErrorProcessor.Action] = {
          logged += s"${f.path}: $message${Option(t).map(t => s" ($t)").getOrElse("")}"
          java.util.EnumSet.noneOf(classOf[LoggedErrorProcessor.Action])
        }
      }, (() => infos = myFixture.doHighlighting()): ThrowableRunnable[RuntimeException])
      val doc = PsiDocumentManager.getInstance(getProject).getDocument(psi)
      infos.asScala
        .filter(i => i.getSeverity == HighlightSeverity.ERROR || (req.warnings && i.getSeverity == HighlightSeverity.WARNING))
        .filter(_.getDescription != null)
        .sortBy(_.getStartOffset)
        .foreach { i =>
          val line = doc.getLineNumber(i.getStartOffset)
          val col = i.getStartOffset - doc.getLineStartOffset(line)
          val sev = if (i.getSeverity == HighlightSeverity.ERROR) "error" else "warning"
          diagnostics += Diagnostic(f.path, line + 1, col + 1, sev, i.getDescription)
        }
    }
    val answers = if (req.queries.isEmpty) Nil else new Queries(psiFiles).answer(req.queries)
    (diagnostics.toSeq, answers, logged.toSeq)
  }

  /** The plugin's answers to the queries; see [[TcProtocol.Query]] for how operands resolve. */
  private class Queries(files: Seq[PsiFile]) {
    private val aliases = mutable.LinkedHashMap.empty[String, ScTypeAliasDefinition]
    private val terms = mutable.LinkedHashMap.empty[String, ScTypedDefinition]
    private val classes = mutable.LinkedHashMap.empty[String, PsiClass]
    files.foreach(_.depthFirst().foreach {
      case ta: ScTypeAliasDefinition => aliases.getOrElseUpdate(ta.name.stripPrefix("__q_"), ta)
      case _: ScParameter            =>
      case f: ScFunction if f.isConstructor =>
      case o: ScObject               => terms.getOrElseUpdate(o.name, o)
      case c: PsiClass               => classes.getOrElseUpdate(c.getName, c)
      case t: ScTypedDefinition      => terms.getOrElseUpdate(t.name, t)
      case _                         =>
    })
    private val place: PsiElement = files.head
    private implicit val context: Context = Context(place)

    // an operand is `name` or `name\u001falias`: a name in the sources, else the server's synthetic alias
    private def alternatives(id: String): Seq[String] = id.split('\u001f').toSeq

    private def tpe1(id: String) = aliases.get(id).map(_.aliasedType).orElse(terms.get(id).map {
      case f: ScFunction => f.returnType
      case t             => t.`type`()
    })

    private def tpe(id: String): ScType = {
      val alts = alternatives(id)
      val result = alts.iterator.flatMap(tpe1).nextOption().getOrElse(throw new NoSuchElementException(s"unresolved ${alts.head}"))
      result.getOrElse(throw new NoSuchElementException(s"${alts.head}: $result"))
    }

    private def cls(id: String): PsiClass = alternatives(id).iterator
      .flatMap(a => classes.get(a).orElse(tpe1(a).flatMap(_.toOption).flatMap(_.extractClass)))
      .nextOption().getOrElse(throw new NoSuchElementException(s"unresolved class ${alternatives(id).head}"))

    private def show(t: ScType): String = t.presentableText(using TypePresentationContext.emptyContext, context)

    def answer(queries: Seq[Query]): Seq[String] = queries.map { q =>
      try q match {
        case Query.Conforms(a, b) => tpe(a).conforms(tpe(b)).toString
        case Query.Equiv(a, b)    => tpe(a).equiv(tpe(b)).toString
        case Query.BaseType(a, c) => TcWorker.baseType(tpe(a), cls(c)).fold("none")(show)
        case Query.TypeOf(a)      => show(tpe(a))
      } catch {
        case e: ProcessCanceledException => throw e
        case e: Throwable                => "exc " + String.valueOf(e).replace('\n', ' ')
      }
    }
  }
}

object TcWorker {
  /** Files the server adds (the aliases standing for query operands); not highlighted, not diffed. */
  val SyntheticPrefix = "__tc_"

  /** `2.13` / `3.3` mean the latest of that line known to the plugin's tests; else an exact version. */
  def parseVersion(s: String): ScalaVersion =
    ScalaVersion.Latest.allStableWithoutScalaNext.find(_.major == s)
      .orElse(ScalaVersion.fromString(s))
      .getOrElse(throw new IllegalArgumentException(s"bad scalaVersion $s"))

  /** `BaseTypes.baseType` where the tree has it, else the first base type of class `cls`. */
  private lazy val baseTypeMethod = BaseTypes.getClass.getMethods.find(_.getName == "baseType")

  def baseType(t: ScType, cls: PsiClass)(implicit context: Context): Option[ScType] = baseTypeMethod match {
    case Some(m) => m.invoke(BaseTypes, t, cls, context).asInstanceOf[Option[ScType]]
    case None    => (Iterator.single(t) ++ BaseTypes.iterator(t)).find(_.extractClass.contains(cls))
  }
}
