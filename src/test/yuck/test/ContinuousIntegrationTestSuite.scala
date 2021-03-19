package yuck.test

import org.junit._

import yuck.flatzinc.test._

/**
 * Test suite for CI runs
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[UnitTestSuite],
        classOf[FrontEndTestSuite],
        classOf[HelloWorldTestSuite],
        classOf[TractableMiniZincExamples]))
@Test
final class ContinuousIntegrationTestSuite {
}
