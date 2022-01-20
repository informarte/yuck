package yuck.core.test

import yuck.core._
import yuck.util.logging.LazyLogger

import scala.collection.Seq

/**
 * @author Michael Marte
 *
 */
final class IntegerSetDomainTestHelper
    (randomGenerator: RandomGenerator, logger: LazyLogger)
    extends OrderedDomainTestHelper[IntegerSetValue](logger, randomGenerator)
{

    def createTestData(baseRange: IntegerRange, sampleSize: Int): Seq[IntegerSetDomain] =
        IntegerDomainTestHelper
            .createTestData(baseRange, sampleSize, randomGenerator)
            .flatMap(r =>
                // {{}} = P({}), so we keep only one of them to facilitate equality testing
                if (r.isEmpty) List(new SingletonIntegerSetDomain(r))
                else List(new SingletonIntegerSetDomain(r), new IntegerPowersetDomain(r)))

}
