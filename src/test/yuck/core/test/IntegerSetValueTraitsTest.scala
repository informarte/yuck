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
        assertEx(emptyDomain)
        assertEq(completeDomain, CompleteIntegerSetDomain)
    }

    @Test
    def testDomainFactories {
        assertEx(createDomain(Set()))
        assertEx(createDomain(EmptyIntegerSetValue, CompleteIntegerSetValue))
    }

    @Test
    def testCasting {
        assertEx(safeDowncast(Zero))
        safeDowncast(new IntegerSetValue(CompleteIntegerRange))
        safeDowncast(new SingletonIntegerSetDomain(CompleteIntegerRange))
        safeDowncast(new IntegerPowersetDomain(CompleteIntegerRange))
        assertEx(safeDowncast(CompleteIntegerRange))
    }

    @Test
    def testConfiguration {
        assertEq(valueType, classOf[IntegerSetValue])
        assertEq(orderingCostModel, IntegerSetOrderingCostModel)
        assertEq(domainPruner, IntegerSetDomainPruner)
    }

}
