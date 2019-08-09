package yuck.core.test

import yuck.core.AnyValue
import yuck.util.testing.{EqualityTestHelper, YuckAssert}

/**
 * @author Michael Marte
 *
 */
class ValueTestHelper[Value <: AnyValue] extends YuckAssert {

    def testEquality(testData: Seq[Value]): Unit = {
        val helper = new EqualityTestHelper[Value] {}
        helper.testEquality(testData)
    }

}
