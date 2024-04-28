package yuck.core.test

import yuck.core.{given, *}
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
trait IntegerSetValueTestData {

    protected val randomGenerator: RandomGenerator
    protected val logger: LazyLogger

    private lazy val helper = new IntegerDomainTestHelper(randomGenerator, logger)

    protected val baseRange = IntegerRange(-5, 5)
    protected val sampleSize = 16
    protected lazy val baseData = helper.createTestData(baseRange, sampleSize)
    protected lazy val testData = baseData.map(new IntegerSetValue(_))

}
