package yuck.core.test

import org.junit._

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[BooleanDomainTest],
        classOf[BooleanDecisionDomainTest],
        classOf[BooleanChannelDomainTest],
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
