package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.test.util.UnitTest

final class IntegerSetValueOrderingCostModelTest extends UnitTest with IntegerSetValueTestData {

    override protected val randomGenerator = new JavaRandomGenerator
    private val costModel = IntegerSetValueOrderingCostModel

    @Test
    def testOrderingCostModel(): Unit = {
        for a <- testData do {
            for b <- testData do {
                assertEq(BooleanValue(costModel.eqViolation(a, b)).truthValue, a == b)
                assertEq(BooleanValue(costModel.neViolation(a, b)).truthValue, a != b)
                assertEq(BooleanValue(costModel.ltViolation(a, b)).truthValue, a < b)
                assertEq(BooleanValue(costModel.leViolation(a, b)).truthValue, a <= b)
            }
        }
    }

}
