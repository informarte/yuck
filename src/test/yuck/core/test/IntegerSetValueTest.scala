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
final class IntegerSetValueTest extends UnitTest {

    private val helper = new OrderedValueTestHelper[IntegerSetValue]
    private val baseData =
        List(
            EmptyIntegerRange, NonNegativeIntegerRange, CompleteIntegerRange,
            new IntegerRange(Zero, Two), new IntegerRange(One, Three), new IntegerRange(Two, Four),
            new IntegerRange(Zero, Five))
    private val testData =
        baseData.map(new IntegerSetValue(_))

    @Test
    def testConstruction {
        for (a <- baseData) {
            assertEq(new IntegerSetValue(a).set, a)
        }
    }

    @Test
    def testSpecialValues {
        assert(EmptyIntegerSetValue.set.isEmpty)
        assert(CompleteIntegerSetValue.set.isComplete)
    }

    @Test
    def testEquality {
        helper.testEquality(testData)
        for (a <- testData) {
            val b = new IntegerSetValue(a.set)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, Zero)
            assertNe(Zero, a)
        }
    }

    @Test
    def testOrdering {
        helper.testOrdering(testData ++ testData)
    }

    @Test
    def testOrderingCostModel {
        val costModel = IntegerSetOrderingCostModel
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
