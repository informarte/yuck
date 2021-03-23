package yuck.core.test

import org.junit._

import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanValueOrderingCostModelTest extends UnitTest with BooleanValueTestData {

    private val costModel = BooleanValueOrderingCostModel

    @Test
    def testOrderingCostModel: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(costModel.eqViolation(a, b).truthValue, a.truthValue == b.truthValue)
                assertEq(costModel.neViolation(a, b).truthValue, a.truthValue != b.truthValue)
                assertEq(costModel.ltViolation(a, b).truthValue, ! a.truthValue && b.truthValue)
                assertEq(costModel.leViolation(a, b).truthValue, ! a.truthValue || b.truthValue)
            }
        }
    }

    @Test
    def testOverflowCheckingInCostComputation: Unit = {
        costModel.eqViolation(True, BooleanValue(Long.MaxValue - 1))
        assertEx(costModel.eqViolation(True, BooleanValue(Long.MaxValue)), classOf[ArithmeticException])
        costModel.neViolation(False, BooleanValue(Long.MaxValue - 1))
        assertEx(costModel.neViolation(False, BooleanValue(Long.MaxValue)), classOf[ArithmeticException])
        costModel.ltViolation(True, BooleanValue(Long.MaxValue - 1))
        assertEx(costModel.ltViolation(True, BooleanValue(Long.MaxValue)), classOf[ArithmeticException])
        costModel.leViolation(True, BooleanValue(Long.MaxValue - 1))
        assertEx(costModel.leViolation(True, BooleanValue(Long.MaxValue)), classOf[ArithmeticException])
    }

}
