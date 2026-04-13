package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.Probability
import yuck.test.util.UnitTest

final class ProbabilityTest extends UnitTest {

    @Test
    def testCreationFromFloatValue(): Unit = {
        assertEq(Probability(0.0).value, 0.0)
        assertEq(Probability(0.5).value, 0.5)
        assertEq(Probability(1.0).value, 1.0)
        assertThrows(Probability(-0.1))
        assertThrows(Probability(1.1))
    }

    @Test
    def testCreationFromPercentage(): Unit = {
        assertEq(Probability(0).value, 0.0)
        assertEq(Probability(50).value, 0.5)
        assertEq(Probability(100).value, 1.0)
        assertThrows(Probability(-1))
        assertThrows(Probability(101))
    }

}
