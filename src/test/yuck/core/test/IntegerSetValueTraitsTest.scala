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
    def testVariableFactories {
        val space = new Space(logger)
        val dx = new IntegerPowersetDomain(NonNegativeIntegerRange)
        val x = createVariable(space, "x", dx)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, dx)
        assertEq(c.domain, CompleteIntegerSetDomain)
    }

    @Test
    def testValueCasting {
        assertEx(safeDowncast(Zero), classOf[ClassCastException])
        safeDowncast(new IntegerSetValue(CompleteIntegerRange))
    }

    @Test
    def testDomainCasting {
        safeDowncast(new SingletonIntegerSetDomain(CompleteIntegerRange))
        safeDowncast(new IntegerPowersetDomain(CompleteIntegerRange))
        assertEx(safeDowncast(CompleteIntegerRange), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting {
        val space = new Space(logger)
        val b = space.createVariable("b", CompleteBooleanDecisionDomain)
        val s = space.createVariable("s", new IntegerPowersetDomain(CompleteIntegerRange))
        safeDowncast(s)
        assertEx(safeDowncast(b), classOf[ClassCastException])
    }

    @Test
    def testConfiguration {
        assertEq(valueType, classOf[IntegerSetValue])
        assertEq(orderingCostModel, IntegerSetOrderingCostModel)
        assertEq(domainPruner, IntegerSetDomainPruner)
    }

}
