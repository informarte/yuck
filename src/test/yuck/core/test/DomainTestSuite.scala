package yuck.core.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

@Suite
@SelectClasses(
    Array(
        classOf[BooleanDomainTest],
        classOf[BooleanDomainPrunerTest],
        classOf[EmptyIntegerSetDomainTest],
        classOf[IntegerDomainTest],
        classOf[IntegerRangeTest],
        classOf[IntegerRangeListTest],
        classOf[SixtyFourBitSetTest],
        classOf[IntegerDomainPrunerTest],
        classOf[IntegerSetDomainTest],
        classOf[IntegerPowerSetDomainTest],
        classOf[SingletonIntegerSetDomainTest]))
final class DomainTestSuite
