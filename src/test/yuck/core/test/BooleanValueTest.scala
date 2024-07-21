package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanValueTest extends UnitTest with BooleanValueTestData {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new OrderedValueTestHelper[BooleanValue](randomGenerator)

    @Test
    def testConstruction(): Unit = {
        for (a <- testRange) {
            assertEq(new BooleanValue(a).violation, a)
        }
        assertEx(new BooleanValue(-1))
    }

    @Test
    def testSpecialValues(): Unit = {
        assertEq(True.violation, 0)
        assertEq(False.violation, 1)
        assertEq(False2.violation, 2)
        assertEq(False3.violation, 3)
        assertEq(False4.violation, 4)
        assertEq(False5.violation, 5)
        assertEq(False6.violation, 6)
        assertEq(False7.violation, 7)
        assertEq(False8.violation, 8)
        assertEq(False9.violation, 9)
        assertEq(False10.violation, 10)
    }

    @Test
    def testValueFactory(): Unit = {
        assertEq(BooleanValue(false), False)
        assertEq(BooleanValue(true), True)
        for (a <- testRange) {
            assertEq(BooleanValue(a).violation, a)
            assert(BooleanValue(a).eq(BooleanValue(a)))
        }
        assertEx(new BooleanValue(-1))
    }

    @Test
    def testEquality(): Unit = {
        helper.testEquality(testData)
        for (a <- testData) {
            val b = new BooleanValue(a.violation)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, Zero)
            assertNe(Zero, a)
            for (b <- testData) {
                assert(if (a.eq(b)) a == b else a != b)
            }
        }
    }

    @Test
    def testOrdering(): Unit = {
        helper.testOrdering(testData)
        for (a <- testData) {
            for (b <- testData) {
                assertEq(a.compare(b).sign, a.violation.compare(b.violation).sign)
            }
        }
    }

    @Test
    def testConfiguration(): Unit = {
        import BooleanValue.{given}
        assertEq(ordering, BooleanValueOrdering)
        assertEq(traits, BooleanValueTraits)
    }

}
