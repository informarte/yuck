package yuck.core.test

import scala.collection.Seq

import yuck.core.*

trait IntegerSetDomainTestDataFactory {

    protected val randomGenerator: RandomGenerator

    private class BaseDataFactory extends IntegerDomainTestDataFactory {
        override protected val randomGenerator = IntegerSetDomainTestDataFactory.this.randomGenerator
    }

    def createTestData(baseRange: IntegerRange, sampleSize: Int): Seq[IntegerSetDomain] =
        new BaseDataFactory()
            .createTestData(baseRange, sampleSize)
            .flatMap(r =>
                // {{}} = P({}), so we keep only one of them to facilitate equality testing
                if r.isEmpty
                then List(new SingletonIntegerSetDomain(r))
                else List(new SingletonIntegerSetDomain(r), new IntegerPowerSetDomain(r)))

}
