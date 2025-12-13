package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.test.util.{OrderingTestHelper, UnitTest}

final class BooleanValueOrderingTest extends UnitTest with BooleanValueTestData {

    private val randomGenerator = new JavaRandomGenerator

    @Test
    def testOrdering(): Unit = {
        val helper = new OrderingTestHelper[BooleanValue](randomGenerator)
        val ord1 = BooleanValueOrdering
        helper.testOrdering(testData, ord1)
        val ord2 = new Ordering[BooleanValue] {
            override def compare(a: BooleanValue, b: BooleanValue) = a.compare(b)
        }
        assertEq(testData.sorted(using ord1), testData.sorted(using ord2))
    }

}
