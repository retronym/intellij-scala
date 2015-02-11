package org.jetbrains.plugins.scala.testingSupport.scalatest.singleTest

import org.jetbrains.plugins.scala.testingSupport.scalatest.generators.FlatSpecGenerator

/**
 * @author Roman.Shein
 * @since 20.01.2015.
 */
trait FlatSpecSingleTestTest extends FlatSpecGenerator {
  def testFlatSpec() {
    addFlatSpec()

    runTestByLocation(7, 1, "FlatSpecTest.scala",
      checkConfigAndSettings(_, "FlatSpecTest", "A FlatSpecTest should be able to run single test"),
      root => checkResultTreeHasExactNamedPath(root, "[root]", "FlatSpecTest", "A FlatSpecTest", "should be able to run single test") &&
          checkResultTreeDoesNotHaveNodes(root, "should not run other tests"),
      debug = true
    )
  }
}
