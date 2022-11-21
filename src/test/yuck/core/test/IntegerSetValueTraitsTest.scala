package yuck.core.test

import org.junit.*

import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetValueTraitsTest extends UnitTest {

    import IntegerSetValueTraits.*

    @Test
    def testValueNormalization(): Unit = {
        assertEq(normalizedValue(EmptyIntegerSetValue), EmptyIntegerSetValue)
        assertEq(normalizedValue(CompleteIntegerSetValue), CompleteIntegerSetValue)
    }

    @Test
    def testSpecialDomains(): Unit = {
        assertEq(emptyDomain, EmptyIntegerSetDomain)
        assertEq(completeDomain, CompleteIntegerSetDomain)
    }

    @Test
    def testDomainFactories(): Unit = {
        assertEq(createDomain(Set()), EmptyIntegerSetDomain)
        assertNie(createDomain(Set(EmptyIntegerSetValue)))
        assertEq(createDomain(CompleteIntegerSetValue, EmptyIntegerSetValue), EmptyIntegerSetDomain)
        assertEq(createDomain(CompleteIntegerSetValue, CompleteIntegerSetValue), new SingletonIntegerSetDomain(CompleteIntegerRange))
        assertEq(createDomain(EmptyIntegerSetValue, CompleteIntegerSetValue), CompleteIntegerSetDomain)
    }

    @Test
    def testVariableFactories(): Unit = {
        val space = new Space(logger, sigint)
        val dx = new IntegerPowersetDomain(NonNegativeIntegerRange)
        val x = createVariable(space, "x", dx)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, dx)
        assertEq(c.domain, CompleteIntegerSetDomain)
    }

    @Test
    def testValueCasting(): Unit = {
        assertEx(safeDowncast(Zero), classOf[ClassCastException])
        safeDowncast(new IntegerSetValue(CompleteIntegerRange))
    }

    @Test
    def testDomainCasting(): Unit = {
        safeDowncast(new SingletonIntegerSetDomain(CompleteIntegerRange))
        safeDowncast(new IntegerPowersetDomain(CompleteIntegerRange))
        assertEx(safeDowncast(CompleteIntegerRange), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting(): Unit = {
        val space = new Space(logger, sigint)
        val b = space.createVariable("b", CompleteBooleanDomain)
        val s = space.createVariable("s", new IntegerPowersetDomain(CompleteIntegerRange))
        safeDowncast(s)
        assertEx(safeDowncast(b), classOf[ClassCastException])
    }

    @Test
    def testConfiguration(): Unit = {
        assertEq(valueType, classOf[IntegerSetValue])
        assertEq(valueOrdering, IntegerSetValueOrdering)
        assertEq(costModel, IntegerSetValueOrderingCostModel)
        assertEq(domainOrdering, IntegerSetDomainOrdering)
        assertEq(domainPruner, IntegerSetDomainPruner)
    }

}
