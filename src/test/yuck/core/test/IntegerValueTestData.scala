package yuck.core.test

import yuck.core.IntegerValue

/**
 * @author Michael Marte
 *
 */
trait IntegerValueTestData {

    protected val testRange = -5 to 5
    protected lazy val testData = testRange.map(IntegerValue.get)

}
