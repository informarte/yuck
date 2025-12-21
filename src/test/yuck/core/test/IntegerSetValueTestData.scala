package yuck.core.test

import yuck.core.*

trait IntegerSetValueTestData {

    protected val randomGenerator: RandomGenerator

    private class BaseDataFactory extends IntegerDomainTestDataFactory {
        override protected val randomGenerator = IntegerSetValueTestData.this.randomGenerator
    }

    protected val baseRange = IntegerRange(-5, 5)
    protected val sampleSize = 16
    protected lazy val baseData = new BaseDataFactory().createTestData(baseRange, sampleSize)
    protected lazy val testData = baseData.map(new IntegerSetValue(_))

}
