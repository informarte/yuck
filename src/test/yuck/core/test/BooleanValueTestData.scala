package yuck.core.test

import yuck.core.BooleanValue

trait BooleanValueTestData {

    protected val testRange = 0 to 5
    protected lazy val testData = testRange.map(BooleanValue.apply)

}
