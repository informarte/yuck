package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerValueOrderingCostModelTest extends UnitTest with IntegerValueTestData {

    private val costModel = IntegerValueOrderingCostModel

    @Test
    def testOrderingCostModel(): Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(BooleanValue(costModel.eqViolation(a, b)).truthValue, a == b)
                assertEq(BooleanValue(costModel.neViolation(a, b)).truthValue, a != b)
                assertEq(BooleanValue(costModel.ltViolation(a, b)).truthValue, a < b)
                assertEq(BooleanValue(costModel.leViolation(a, b)).truthValue, a <= b)
                for (c <- testData) {
                    if (a < b && a < c && b < c) {
                        assertLe(costModel.eqViolation(a, b), costModel.eqViolation(a, c))
                        assertLe(costModel.ltViolation(a, b), costModel.ltViolation(a, c))
                    }
                }
            }
        }
    }

    @Test
    def testOverflowCheckingInCostComputation(): Unit = {
        costModel.ltViolation(IntegerValue(Int.MaxValue - 1), Zero)
        assertEx(costModel.ltViolation(IntegerValue(Int.MaxValue), Zero), classOf[ArithmeticException])
        costModel.leViolation(IntegerValue(Int.MaxValue), Zero)
        assertEx(costModel.leViolation(IntegerValue(Int.MaxValue), MinusOne), classOf[ArithmeticException])
    }

}
