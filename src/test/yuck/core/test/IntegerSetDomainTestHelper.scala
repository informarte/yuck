package yuck.core.test

import scala.collection.Seq

import yuck.core.{given, *}
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class IntegerSetDomainTestHelper
    (override protected val randomGenerator: RandomGenerator,
     override protected val logger: LazyLogger)
    extends OrderedDomainTestHelper[IntegerSetValue]
{

    def createTestData(baseRange: IntegerRange, sampleSize: Int): Seq[IntegerSetDomain] =
        new IntegerDomainTestHelper(randomGenerator, logger)
            .createTestData(baseRange, sampleSize)
            .flatMap(r =>
                // {{}} = P({}), so we keep only one of them to facilitate equality testing
                if (r.isEmpty) List(new SingletonIntegerSetDomain(r))
                else List(new SingletonIntegerSetDomain(r), new IntegerPowersetDomain(r)))

}
