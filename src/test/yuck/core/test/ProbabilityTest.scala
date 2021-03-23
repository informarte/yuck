package yuck.core.test

import org.junit._

import yuck.core.Probability
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class ProbabilityTest extends UnitTest {

    @Test
    def testCreationFromFloatValue: Unit = {
        assertEq(Probability(0.0).value, 0.0)
        assertEq(Probability(0.5).value, 0.5)
        assertEq(Probability(1.0).value, 1.0)
        assertEx(Probability(-0.1))
        assertEx(Probability(1.1))
    }

    @Test
    def testCreationFromPercentage: Unit = {
        assertEq(Probability(0).value, 0.0)
        assertEq(Probability(50).value, 0.5)
        assertEq(Probability(100).value, 1.0)
        assertEx(Probability(-1))
        assertEx(Probability(101))
    }

}
