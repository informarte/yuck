package yuck.core.test

import org.junit._

import yuck.core.Probability
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class ProbabilityTest extends UnitTest {

    @Test
    def testCreationFromFloatValue {
        assertEq(Probability.from(0.0).value, 0.0)
        assertEq(Probability.from(0.5).value, 0.5)
        assertEq(Probability.from(1.0).value, 1.0)
        assertEx(Probability.from(-0.1))
        assertEx(Probability.from(1.1))
    }

    @Test
    def testCreationFromPercentage {
        assertEq(Probability.from(0).value, 0.0)
        assertEq(Probability.from(50).value, 0.5)
        assertEq(Probability.from(100).value, 1.0)
        assertEx(Probability.from(-1))
        assertEx(Probability.from(101))
    }

}
