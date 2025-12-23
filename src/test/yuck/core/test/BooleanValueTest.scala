package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.core.test.util.OrderingTestTooling
import yuck.test.util.UnitTest

final class BooleanValueTest
    extends UnitTest
       with OrderingTestTooling[BooleanValue]
       with BooleanValueTestData
{

    override protected val randomGenerator = new JavaRandomGenerator

    @Test
    def testConstruction(): Unit = {
        for a <- testRange do {
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
        for a <- testRange do {
            assertEq(BooleanValue(a).violation, a)
            assert(BooleanValue(a).eq(BooleanValue(a)))
        }
        assertEx(new BooleanValue(-1))
    }

    @Test
    def testEquality(): Unit = {
        testEquality(testData)
        for a <- testData do {
            val b = new BooleanValue(a.violation)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, Zero)
            assertNe(Zero, a)
            for b <- testData do {
                assert(if a.eq(b) then a == b else a != b)
            }
        }
    }

    @Test
    def testOrdering(): Unit = {
        testOrdering(testData)
        for a <- testData do {
            for b <- testData do {
                assertEq(a.compare(b).sign, a.violation.compare(b.violation).sign)
            }
        }
    }

    @Test
    def testConfiguration(): Unit = {
        import BooleanValue.given
        def testOrdering()(using ordering: Ordering[BooleanValue]) = {
            assertEq(ordering, BooleanValueOrdering)
        }
        testOrdering()
        def testTraits()(using traits: OrderedTypeTraits[BooleanValue, BooleanDomain, BooleanVariable]) = {
            assertEq(traits, BooleanTypeTraits)
        }
        testTraits()
    }

}
