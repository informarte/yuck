package yuck.core.test

import yuck.core.BooleanValue

/**
 * @author Michael Marte
 *
 */
trait BooleanValueTestData {

    protected val testRange = 0 to 5
    protected lazy val testData = testRange.map(BooleanValue.get)

}
