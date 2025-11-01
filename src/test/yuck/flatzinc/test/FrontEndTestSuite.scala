package yuck.flatzinc.test

import org.junit.*

/**
 * Test suite that exercises Yuck's FlatZinc front end
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[FlatZincBaseTest],
        classOf[FlatZincExtensionTest],
        classOf[GlobalConstraintCompilationTest],
        classOf[ProgressiveTighteningTest]))
class FrontEndTestSuite
