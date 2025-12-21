package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.core.test.util.OrderingTestTooling
import yuck.test.util.UnitTest

final class BooleanValueOrderingTest
    extends UnitTest
       with OrderingTestTooling[BooleanValue]
       with BooleanValueTestData
{

    protected val randomGenerator = new JavaRandomGenerator

    @Test
    def testOrdering(): Unit = {
        val ord1 = BooleanValueOrdering
        testOrdering(testData, ord1)
        val ord2 = new Ordering[BooleanValue] {
            override def compare(a: BooleanValue, b: BooleanValue) = a.compare(b)
        }
        assertEq(testData.sorted(using ord1), testData.sorted(using ord2))
    }

}
