package org.jetbrains.plugins.scala
package refactoring.move

import org.jetbrains.plugins.scala.util.TestUtils
import com.intellij.testFramework.{CodeInsightTestCase, PlatformTestUtil, PsiTestUtil}
import com.intellij.openapi.projectRoots.impl.JavaSdkImpl
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import java.util
import java.io.File
import com.intellij.psi.impl.source.PostprocessReformattingAspect
import com.intellij.psi._
import com.intellij.psi.search.GlobalSearchScope
import collection.mutable.ArrayBuffer
import lang.psi.impl.{ScalaFileImpl, ScalaPsiManager}
import com.intellij.refactoring.move.moveClassesOrPackages.{SingleSourceRootMoveDestination, MoveClassesOrPackagesProcessor}
import com.intellij.refactoring.PackageWrapper
import com.intellij.openapi.fileEditor.FileDocumentManager
import lang.psi.api.toplevel.typedef.ScObject

/**
 * @author Alefas
 * @since 30.10.12
 */
class ScalaMoveClassTest extends CodeInsightTestCase {
  def testPackageObject() {
    doTest("packageObject", Array("com.`package`"), "org")
  }

  def testPackageObject2() {
    doTest("packageObject2", Array("com"), "org")
  }

  def testSimple() {
    doTest("simple", Array("com.A"), "org")
  }

  def testSCL2625() {
    doTest("scl2625", Array("somepackage.Dummy", "somepackage.MoreBusiness", "somepackage.Business", "somepackage.AnotherEnum"), "dest")
  }

  def testSCL4623() {
    doTest("scl4623", Array("moveRefactoring.foo.B"), "moveRefactoring.bar")
  }

  def testSCL4613() {
    doTest("scl4613", Array("moveRefactoring.foo.B"), "moveRefactoring.bar")
  }

  def testSCL4621() {
    doTest("scl4621", Array("moveRefactoring.foo.O"), "moveRefactoring.bar")
  }

  def testSCL4619() {
    doTest("scl4619", Array("foo.B"), "bar")
  }

  def testSCL4875() {
    doTest("scl4875", Array("com.A"), "org")
  }

  def testSCL4878() {
    doTest("scl4878", Array("org.B"), "com")
  }

  def testSCL4894() {
    doTest("scl4894", Array("moveRefactoring.foo.B", "moveRefactoring.foo.BB"), "moveRefactoring.bar")
  }

  def doTest(testName: String, classNames: Array[String], newPackageName: String) {
    val root: String = TestUtils.getTestDataPath + "/move/" + testName
    val rootBefore: String = root + "/before"
    PsiTestUtil.removeAllRoots(myModule, JavaSdkImpl.getMockJdk17)
    val rootDir: VirtualFile = PsiTestUtil.createTestProjectStructure(getProject, myModule, rootBefore, new util.HashSet[File]())
    performAction(classNames, newPackageName)
    val rootAfter: String = root + "/after"
    val rootDir2: VirtualFile = LocalFileSystem.getInstance.findFileByPath(rootAfter.replace(File.separatorChar, '/'))
    getProject.getComponent(classOf[PostprocessReformattingAspect]).doPostponedFormatting()
    PlatformTestUtil.assertDirectoriesEqual(rootDir2, rootDir, PlatformTestUtil.CVS_FILE_FILTER)
  }

  private def performAction(classNames: Array[String], newPackageName: String) {
    val classes = new ArrayBuffer[PsiClass]()
    for (name <- classNames) {
      classes ++= ScalaPsiManager.instance(getProject).getCachedClasses(GlobalSearchScope.allScope(getProject), name).filter {
        case o: ScObject if o.isSyntheticObject => false
        case _ => true
      }
    }
    val aPackage: PsiPackage = JavaPsiFacade.getInstance(getProject).findPackage(newPackageName)
    val dirs: Array[PsiDirectory] = aPackage.getDirectories
    assert(dirs.length == 1)
    ScalaFileImpl.performMoveRefactoring {
      new MoveClassesOrPackagesProcessor(getProject, classes.toArray,
        new SingleSourceRootMoveDestination(PackageWrapper.create(JavaDirectoryService.getInstance.getPackage(dirs(0))), dirs(0)), true, true, null).run()
    }
    PsiDocumentManager.getInstance(getProject).commitAllDocuments()
    FileDocumentManager.getInstance.saveAllDocuments()
  }
}
