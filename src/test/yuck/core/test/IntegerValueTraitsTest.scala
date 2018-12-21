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
    def testVariableFactories {
        val space = new Space(logger)
        val x = createVariable(space, "x", NonNegativeIntegerRange)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, NonNegativeIntegerRange)
        assertEq(c.domain, CompleteIntegerRange)
    }

    @Test
    def testValueCasting {
        assertEx(safeDowncast(False), classOf[ClassCastException])
        safeDowncast(Zero)
    }

    @Test
    def testDomainCasting {
        safeDowncast(EmptyIntegerRange)
        safeDowncast(EmptyIntegerRangeList)
        assertEx(safeDowncast(EmptyBooleanDomain), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting {
        val space = new Space(logger)
        val b = space.createVariable("b", CompleteBooleanDecisionDomain)
        val i = space.createVariable("i", CompleteIntegerRange)
        safeDowncast(i)
        assertEx(safeDowncast(b), classOf[ClassCastException])
    }

    @Test
    def testConfiguration {
        assertEq(valueType, classOf[IntegerValue])
        assertEq(orderingCostModel, IntegerOrderingCostModel)
        assertEq(domainPruner, IntegerDomainPruner)
    }

}
