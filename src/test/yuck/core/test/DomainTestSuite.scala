package yuck.core.test

import org.junit.*

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[BooleanDomainTest],
        classOf[BooleanDomainPrunerTest],
        classOf[EmptyIntegerSetDomainTest],
        classOf[IntegerDomainTest],
        classOf[IntegerRangeTest],
        classOf[IntegerRangeListTest],
        classOf[IntegerDomainPrunerTest],
        classOf[IntegerSetDomainTest],
        classOf[IntegerPowersetDomainTest],
        classOf[SingletonIntegerSetDomainTest]))
final class DomainTestSuite
