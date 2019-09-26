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
final class IntegerValueOrderingCostModelTest extends UnitTest with IntegerValueTestData {

    private val costModel = IntegerValueOrderingCostModel

    @Test
    def testOrderingCostModel: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(costModel.eq(a, b).truthValue, a == b)
                assertEq(costModel.ne(a, b).truthValue, a != b)
                assertEq(costModel.lt(a, b).truthValue, a < b)
                assertEq(costModel.le(a, b).truthValue, a <= b)
                for (c <- testData) {
                    if (a < b && a < c && b < c) {
                        assertLe(costModel.eq(a, b).violation, costModel.eq(a, c).violation)
                        assertLe(costModel.lt(a, b).violation, costModel.lt(a, c).violation)
                    }
                }
            }
        }
    }

    @Test
    def testOverflowCheckingInCostComputation: Unit = {
        costModel.lt(IntegerValue.get(Int.MaxValue - 1), Zero)
        assertEx(costModel.lt(IntegerValue.get(Int.MaxValue), Zero), classOf[ArithmeticException])
        costModel.le(IntegerValue.get(Int.MaxValue), Zero)
        assertEx(costModel.le(IntegerValue.get(Int.MaxValue), MinusOne), classOf[ArithmeticException])
    }

}
