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
final class IntegerSetValueOrderingCostModelTest extends UnitTest with IntegerSetValueTestData {

    private val costModel = IntegerSetValueOrderingCostModel

    @Test
    def testOrderingCostModel: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(costModel.eq(a, b).truthValue, a == b)
                assertEq(costModel.ne(a, b).truthValue, a != b)
                assertEq(costModel.lt(a, b).truthValue, a < b)
                assertEq(costModel.le(a, b).truthValue, a <= b)
            }
        }
    }

}
