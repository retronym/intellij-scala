package org.jetbrains.plugins.scala.lang.resolve2.packagealias

import org.jetbrains.plugins.scala.lang.resolve2.ResolveTestBase
import org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration

class PackageAliasTest extends ResolveTestBase{
  override def folderPath: String = {
    super.folderPath + "packagealias/"
  }
  private[this] def setUpAlias(): Unit =
    setCompilerOptions(s"-Yalias-package:p1.newname=p1.oldname")

  private[this] def setCompilerOptions(options: String*): Unit = {
    val defaultProfile = ScalaCompilerConfiguration.instanceIn(getProject).defaultProfile
    val newSettings    = defaultProfile.getSettings.copy(
      additionalCompilerOptions = options
    )
    defaultProfile.setSettings(newSettings)
  }
  def testReferenceViaAlias(): Unit = {
    setUpAlias()
    doTest()
  }

  def testReferencePackageObjectMemberViaAlias(): Unit = {
    setUpAlias()
    doTest()
  }

  def testReferencePackageObjectMemberViaAlias2(): Unit = {
    setUpAlias()
    doTest()
  }

  def testReferenceNestedViaAlias(): Unit = {
    setUpAlias()
    doTest()
  }

  def testReferenceNestedViaAlias2(): Unit = {
    setUpAlias()
    doTest()
  }
}
