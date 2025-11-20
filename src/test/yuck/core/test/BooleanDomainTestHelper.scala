package yuck.core.test

import yuck.core.*
import yuck.util.logging.LazyLogger

final class BooleanDomainTestHelper
    (override protected val randomGenerator: RandomGenerator,
     override protected val logger: LazyLogger)
    extends OrderedDomainTestHelper[BooleanValue]
