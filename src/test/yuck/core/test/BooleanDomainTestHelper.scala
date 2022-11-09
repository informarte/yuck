package yuck.core.test

import scala.collection.Seq

import yuck.core.{given, *}
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class BooleanDomainTestHelper
    (override protected val randomGenerator: RandomGenerator,
     override protected val logger: LazyLogger)
    extends OrderedDomainTestHelper[BooleanValue]
