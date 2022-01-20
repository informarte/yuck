package yuck.core.test

import scala.collection.Seq

import yuck.core.AnyValue
import yuck.test.util.{EqualityTestHelper, YuckAssert}

/**
 * @author Michael Marte
 *
 */
class ValueTestHelper[V <: AnyValue] extends YuckAssert {

    def testEquality(testData: Seq[V]): Unit = {
        val helper = new EqualityTestHelper[V]
        helper.testEquality(testData)
    }

}
