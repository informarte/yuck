package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetValueOrderingCostModelTest extends UnitTest with IntegerSetValueTestData {

    override protected val randomGenerator = new JavaRandomGenerator
    private val costModel = IntegerSetValueOrderingCostModel

    @Test
    def testOrderingCostModel: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(BooleanValue(costModel.eqViolation(a, b)).truthValue, a == b)
                assertEq(BooleanValue(costModel.neViolation(a, b)).truthValue, a != b)
                assertEq(BooleanValue(costModel.ltViolation(a, b)).truthValue, a < b)
                assertEq(BooleanValue(costModel.leViolation(a, b)).truthValue, a <= b)
            }
        }
    }

}
