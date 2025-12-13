package yuck.flatzinc.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

/**
 * Test suite that exercises Yuck's FlatZinc front end
 */
@Suite
@SelectClasses(
    Array(
        classOf[FlatZincBaseTest],
        classOf[FlatZincExtensionTest],
        classOf[GlobalConstraintCompilationTest],
        classOf[ProgressiveTighteningTest]))
class FrontEndTestSuite
