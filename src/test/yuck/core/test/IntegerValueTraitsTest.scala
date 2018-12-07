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
final class IntegerValueTraitsTest extends UnitTest {

    import IntegerValueTraits._

    @Test
    def testSpecialValues {
        assertEq(zero, Zero)
        assertEq(one, One)
    }

    @Test
    def testSpecialDomains {
        assertEq(emptyDomain, EmptyIntegerRange)
        assertEq(completeDomain, CompleteIntegerRange)
    }

    @Test
    def testDomainFactories {
        assertEq(createDomain(Set()), EmptyIntegerRange)
        assertEq(createDomain(Set(Zero)), ZeroToZeroIntegerRange)
        assertEq(createDomain(Set(Zero, One)), ZeroToOneIntegerRange)
        assert(createDomain(Zero, One).isInstanceOf[IntegerRange])
        assertEq(createDomain(null, null), CompleteIntegerRange)
        assertEq(createDomain(One, Zero), EmptyIntegerRange)
    }

    @Test
    def testCasting {
        assertEx(safeDowncast(False))
        safeDowncast(Zero)
        safeDowncast(EmptyIntegerRange)
        safeDowncast(EmptyIntegerRangeList)
        assertEx(safeDowncast(EmptyBooleanDomain))
    }

    @Test
    def testConfiguration {
        assertEq(valueType, classOf[IntegerValue])
        assertEq(orderingCostModel, IntegerOrderingCostModel)
        assertEq(domainPruner, IntegerDomainPruner)
    }

}
