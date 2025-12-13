package yuck.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

import yuck.flatzinc.test.*
import yuck.flatzinc.test.util.test.MiniZincSolutionVerifierTest

/**
 * Test suite for CI runs
 */
@Suite
@SelectClasses(
    Array(
        classOf[UnitTestSuite],
        classOf[HelloWorldTestSuite],
        classOf[MiniZincSolutionVerifierTest],
        classOf[FrontEndTestSuite],
        classOf[TractableMiniZincExamples]))
final class ContinuousIntegrationTestSuite
