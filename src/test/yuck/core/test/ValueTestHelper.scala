package yuck.core.test

import scala.collection.Seq

import yuck.core.*
import yuck.test.util.{EqualityTestHelper, YuckAssert}

class ValueTestHelper[V <: Value[V]] extends YuckAssert {

    def testEquality(testData: Seq[V]): Unit = {
        val helper = new EqualityTestHelper[V]
        helper.testEquality(testData)
        for a <- testData do {
            for b <- testData do {
                assert(if a.eq(b) then a == b else a != b)
            }
        }
    }

}
