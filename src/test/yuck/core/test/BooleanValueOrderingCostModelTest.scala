package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.test.util.UnitTest

final class BooleanValueOrderingCostModelTest extends UnitTest with BooleanValueTestData {

    private val costModel = BooleanValueOrderingCostModel

    @Test
    def testOrderingCostModel(): Unit = {
        for a <- testData do {
            for b <- testData do {
                assertEq(BooleanValue(costModel.eqViolation(a, b)).truthValue, a.truthValue == b.truthValue)
                assertEq(BooleanValue(costModel.neViolation(a, b)).truthValue, a.truthValue != b.truthValue)
                assertEq(BooleanValue(costModel.ltViolation(a, b)).truthValue, ! a.truthValue && b.truthValue)
                assertEq(BooleanValue(costModel.leViolation(a, b)).truthValue, ! a.truthValue || b.truthValue)
            }
        }
    }

    @Test
    def testOverflowCheckingInCostComputation(): Unit = {
        costModel.eqViolation(True, BooleanValue(Long.MaxValue - 1))
        assertThrows(costModel.eqViolation(True, BooleanValue(Long.MaxValue)), classOf[ArithmeticException])
        costModel.neViolation(False, BooleanValue(Long.MaxValue - 1))
        assertThrows(costModel.neViolation(False, BooleanValue(Long.MaxValue)), classOf[ArithmeticException])
        costModel.ltViolation(True, BooleanValue(Long.MaxValue - 1))
        assertThrows(costModel.ltViolation(True, BooleanValue(Long.MaxValue)), classOf[ArithmeticException])
        costModel.leViolation(True, BooleanValue(Long.MaxValue - 1))
        assertThrows(costModel.leViolation(True, BooleanValue(Long.MaxValue)), classOf[ArithmeticException])
    }

}
