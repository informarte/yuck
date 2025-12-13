package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.test.util.{OrderingTestHelper, UnitTest}

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
        assertEq(testData.sorted(using ord1), testData.sorted(using ord2))
    }

}
