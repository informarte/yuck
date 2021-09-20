package yuck.test

import org.junit._

import yuck.flatzinc.test._
import yuck.flatzinc.test.util.test.MiniZincSolutionVerifierTest

/**
 * Test suite for CI runs
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[UnitTestSuite],
        classOf[HelloWorldTestSuite],
        classOf[MiniZincSolutionVerifierTest],
        classOf[FrontEndTestSuite],
        classOf[TractableMiniZincExamples]))
final class ContinuousIntegrationTestSuite
