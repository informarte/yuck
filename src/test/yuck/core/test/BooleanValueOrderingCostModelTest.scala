package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanValueOrderingCostModelTest extends UnitTest with BooleanValueTestData {

    private val costModel = BooleanValueOrderingCostModel

    @Test
    def testOrderingCostModel: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(costModel.eq(a, b).truthValue, a.truthValue == b.truthValue)
                assertEq(costModel.ne(a, b).truthValue, a.truthValue != b.truthValue)
                assertEq(costModel.lt(a, b).truthValue, ! a.truthValue && b.truthValue)
                assertEq(costModel.le(a, b).truthValue, ! a.truthValue || b.truthValue)
            }
        }
    }

    @Test
    def testOverflowCheckingInCostComputation: Unit = {
        costModel.eq(True, BooleanValue.get(Long.MaxValue - 1))
        assertEx(costModel.eq(True, BooleanValue.get(Long.MaxValue)), classOf[ArithmeticException])
        costModel.ne(False, BooleanValue.get(Long.MaxValue - 1))
        assertEx(costModel.ne(False, BooleanValue.get(Long.MaxValue)), classOf[ArithmeticException])
        costModel.lt(True, BooleanValue.get(Long.MaxValue - 1))
        assertEx(costModel.lt(True, BooleanValue.get(Long.MaxValue)), classOf[ArithmeticException])
        costModel.le(True, BooleanValue.get(Long.MaxValue - 1))
        assertEx(costModel.le(True, BooleanValue.get(Long.MaxValue)), classOf[ArithmeticException])
    }

}
