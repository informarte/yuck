package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.core.test.util.OrderingTestTooling
import yuck.test.util.UnitTest

final class IntegerSetValueOrderingTest
    extends UnitTest
       with OrderingTestTooling[IntegerSetValue]
       with IntegerSetValueTestData
{

    override protected val randomGenerator = new JavaRandomGenerator

    @Test
    def testOrdering(): Unit = {
        val ord1 = IntegerSetValueOrdering
        testOrdering(testData, ord1)
        val ord2 = new Ordering[IntegerSetValue] {
            override def compare(a: IntegerSetValue, b: IntegerSetValue) = a.compare(b)
        }
        assertEq(testData.sorted(using ord1), testData.sorted(using ord2))
    }

}
