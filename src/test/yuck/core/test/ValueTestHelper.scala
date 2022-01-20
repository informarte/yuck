package yuck.core.test

import scala.collection.Seq

import yuck.core._
import yuck.test.util.{EqualityTestHelper, YuckAssert}

/**
 * @author Michael Marte
 *
 */
class ValueTestHelper[V <: Value[V]] extends YuckAssert {

    def testEquality(testData: Seq[V]): Unit = {
        val helper = new EqualityTestHelper[V]
        helper.testEquality(testData)
        for (a <- testData) {
            for (b <- testData) {
                assert(if (a.eq(b)) a == b else a != b)
            }
        }
    }

}
