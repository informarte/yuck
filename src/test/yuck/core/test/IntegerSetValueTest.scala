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
final class IntegerSetValueTest extends UnitTest with IntegerSetValueTestData {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new OrderedValueTestHelper[IntegerSetValue](randomGenerator)

    @Test
    def testConstruction: Unit = {
        for (a <- baseData) {
            assertEq(new IntegerSetValue(a).set, a)
        }
    }

    @Test
    def testSpecialValues: Unit = {
        assert(EmptyIntegerSetValue.set.isEmpty)
        assert(CompleteIntegerSetValue.set.isComplete)
    }

    @Test
    def testEquality: Unit = {
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
    def testOrdering: Unit = {
        helper.testOrdering(testData)
    }

    @Test
    def testConfiguration: Unit = {
        import IntegerSetValue._
        assertEq(valueTraits, IntegerSetValueTraits)
        assertEq(valueOrdering, IntegerSetValueOrdering)
        assertEq(domainOrdering, IntegerSetDomainOrdering)
    }

}
