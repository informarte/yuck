package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.test.util.UnitTest

final class IntegerValueOrderingCostModelTest extends UnitTest with IntegerValueTestData {

    private val costModel = IntegerValueOrderingCostModel

    @Test
    def testOrderingCostModel(): Unit = {
        for a <- testData do {
            for b <- testData do {
                assertEq(BooleanValue(costModel.eqViolation(a, b)).truthValue, a == b)
                assertEq(BooleanValue(costModel.neViolation(a, b)).truthValue, a != b)
                assertEq(BooleanValue(costModel.ltViolation(a, b)).truthValue, a < b)
                assertEq(BooleanValue(costModel.leViolation(a, b)).truthValue, a <= b)
                for c <- testData do {
                    if a < b && a < c && b < c then {
                        assertLe(costModel.eqViolation(a, b), costModel.eqViolation(a, c))
                        assertLe(costModel.ltViolation(a, b), costModel.ltViolation(a, c))
                    }
                }
            }
        }
    }

    @Test
    def testOverflowCheckingInCostComputation(): Unit = {
        costModel.ltViolation(IntegerValue(Long.MaxValue - 1), Zero)
        assertEx(costModel.ltViolation(IntegerValue(Long.MaxValue), Zero), classOf[ArithmeticException])
        costModel.leViolation(IntegerValue(Long.MaxValue), Zero)
        assertEx(costModel.leViolation(IntegerValue(Long.MaxValue), MinusOne), classOf[ArithmeticException])
    }

}
