package yuck.core.test

import org.junit.*

import yuck.core.{given, *}
import yuck.test.util.{OrderingTestHelper, UnitTest}

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetValueOrderingTest extends UnitTest with IntegerSetValueTestData {

    override protected val randomGenerator = new JavaRandomGenerator

    @Test
    def testOrdering(): Unit = {
        val helper = new OrderingTestHelper[IntegerSetValue](randomGenerator)
        val ord1 = IntegerSetValueOrdering
        helper.testOrdering(testData, ord1)
        val ord2 = new Ordering[IntegerSetValue] {
            override def compare(a: IntegerSetValue, b: IntegerSetValue) = a.compare(b)
        }
        assertEq(testData.sorted(ord1), testData.sorted(ord2))
    }

}
