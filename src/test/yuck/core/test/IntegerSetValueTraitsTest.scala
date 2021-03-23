package yuck.core.test

import org.junit._

import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetValueTraitsTest extends UnitTest {

    import IntegerSetValueTraits._

    @Test
    def testSpecialDomains: Unit = {
        assertEx(emptyDomain)
        assertEq(completeDomain, CompleteIntegerSetDomain)
    }

    @Test
    def testDomainFactories: Unit = {
        assertEx(createDomain(Set()))
        assertEx(createDomain(EmptyIntegerSetValue, CompleteIntegerSetValue))
    }

    @Test
    def testVariableFactories: Unit = {
        val space = new Space(logger, sigint)
        val dx = new IntegerPowersetDomain(NonNegativeIntegerRange)
        val x = createVariable(space, "x", dx)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, dx)
        assertEq(c.domain, CompleteIntegerSetDomain)
    }

    @Test
    def testValueCasting: Unit = {
        assertEx(safeDowncast(Zero), classOf[ClassCastException])
        safeDowncast(new IntegerSetValue(CompleteIntegerRange))
    }

    @Test
    def testDomainCasting: Unit = {
        safeDowncast(new SingletonIntegerSetDomain(CompleteIntegerRange))
        safeDowncast(new IntegerPowersetDomain(CompleteIntegerRange))
        assertEx(safeDowncast(CompleteIntegerRange), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting: Unit = {
        val space = new Space(logger, sigint)
        val b = space.createVariable("b", CompleteBooleanDecisionDomain)
        val s = space.createVariable("s", new IntegerPowersetDomain(CompleteIntegerRange))
        safeDowncast(s)
        assertEx(safeDowncast(b), classOf[ClassCastException])
    }

    @Test
    def testConfiguration: Unit = {
        assertEq(valueType, classOf[IntegerSetValue])
        assertEq(valueOrdering, IntegerSetValueOrdering)
        assertEq(orderingCostModel, IntegerSetValueOrderingCostModel)
        assertEq(domainOrdering, IntegerSetDomainOrdering)
        assertEq(domainPruner, IntegerSetDomainPruner)
    }

}
