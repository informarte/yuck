package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerValueOrderingCostModelTest extends UnitTest with IntegerValueTestData {

    private val costModel = IntegerValueOrderingCostModel

    @Test
    def testOrderingCostModel: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(costModel.eqViolation(a, b).truthValue, a == b)
                assertEq(costModel.neViolation(a, b).truthValue, a != b)
                assertEq(costModel.ltViolation(a, b).truthValue, a < b)
                assertEq(costModel.leViolation(a, b).truthValue, a <= b)
                for (c <- testData) {
                    if (a < b && a < c && b < c) {
                        assertLe(costModel.eqViolation(a, b).violation, costModel.eqViolation(a, c).violation)
                        assertLe(costModel.ltViolation(a, b).violation, costModel.ltViolation(a, c).violation)
                    }
                }
            }
        }
    }

    @Test
    def testOverflowCheckingInCostComputation: Unit = {
        costModel.ltViolation(IntegerValue(Int.MaxValue - 1), Zero)
        assertEx(costModel.ltViolation(IntegerValue(Int.MaxValue), Zero), classOf[ArithmeticException])
        costModel.leViolation(IntegerValue(Int.MaxValue), Zero)
        assertEx(costModel.leViolation(IntegerValue(Int.MaxValue), MinusOne), classOf[ArithmeticException])
    }

}
