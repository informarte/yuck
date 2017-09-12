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
        classOf[IntegerDomainTest],
        classOf[IntegerRangeTest],
        classOf[IntegerRangeListTest],
        classOf[IntegerSetDomainTest],
        classOf[IntegerPowersetDomainTest],
        classOf[SingletonIntegerSetDomainTest]))
@Test
final class DomainTestSuite {
}
