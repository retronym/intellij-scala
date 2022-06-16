package org.jetbrains.plugins.scala.lang.resolve2.packagealias

import org.jetbrains.plugins.scala.lang.resolve2.ResolveTestBase

class PackageAliasTest extends ResolveTestBase{
  override def folderPath: String = {
    super.folderPath + "packagealias/"
  }

  def testReferenceViaAlias(): Unit =
    doTest()

  def testReferencePackageObjectMemberViaAlias(): Unit =
    doTest()

  def testReferencePackageObjectMemberViaAlias2(): Unit =
    doTest()

  def testReferenceNestedViaAlias(): Unit =
    doTest()
  def testReferenceNestedViaAlias2(): Unit =
    doTest()
}
