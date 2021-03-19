package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

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
                assertEq(costModel.eqViolation(a, b).truthValue, a == b)
                assertEq(costModel.neViolation(a, b).truthValue, a != b)
                assertEq(costModel.ltViolation(a, b).truthValue, a < b)
                assertEq(costModel.leViolation(a, b).truthValue, a <= b)
            }
        }
    }

}
