package yuck.core.test

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
trait IntegerSetValueTestData {

    protected val baseRange = IntegerRange(IntegerValue(-5), Five)
    protected val sampleSize = 16
    protected val randomGenerator: RandomGenerator

    protected lazy val baseData = IntegerDomainTestHelper.createTestData(baseRange, sampleSize, randomGenerator)
    protected lazy val testData = baseData.map(new IntegerSetValue(_))

}
