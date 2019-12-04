package yuck.flatzinc.test

import org.junit._

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
        classOf[GlobalConstraintCompilationTest]))
@Test
class FrontEndTestSuite {
}
