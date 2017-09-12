package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetValueTraitsTest extends UnitTest {

    import IntegerSetValueTraits._

    @Test
    def testSpecialDomains {
        assertEx(emptyDomain, classOf[NotImplementedError])
        assertEq(completeDomain, CompleteIntegerSetDomain)
    }

    @Test
    def testDomainFactories {
        assertEx(createDomain(Set()), classOf[NotImplementedError])
        assertEx(createDomain(EmptyIntegerSetValue, CompleteIntegerSetValue), classOf[NotImplementedError])
    }

    @Test
    def testCasting {
        assertEx(safeDowncast(Zero))
        safeDowncast(new IntegerSetValue(CompleteIntegerRange))
        safeDowncast(new SingletonIntegerSetDomain(CompleteIntegerRange))
        safeDowncast(new IntegerPowersetDomain(CompleteIntegerRange))
        assertEx(safeDowncast(CompleteIntegerRange))
    }

}
