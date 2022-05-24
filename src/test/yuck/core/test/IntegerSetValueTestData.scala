package yuck.core.test

import yuck.core.*
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
trait IntegerSetValueTestData {

    protected val randomGenerator: RandomGenerator
    protected val logger: LazyLogger

    protected val baseRange = IntegerRange(IntegerValue(-5), Five)
    protected val sampleSize = 16

    private lazy val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    protected lazy val baseData = helper.createTestData(baseRange, sampleSize)
    protected lazy val testData = baseData.map(new IntegerSetValue(_))

}
