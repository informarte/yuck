package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.test.util.UnitTest

final class IntegerSetTypeTraitsTest extends UnitTest {

    import IntegerSetTypeTraits.*

    @Test
    def testDomainCapabilities(): Unit = {
        assert(! domainCapabilities.createDomain)
        assert(! domainCapabilities.diff)
        assert(! domainCapabilities.randomSubdomain)
        assert(! domainCapabilities.size)
        assert(! domainCapabilities.union)
    }

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
        assertThrows(createDomain(Set(EmptyIntegerSetValue)), classOf[NotImplementedError])
        assertEq(createDomain(CompleteIntegerSetValue, EmptyIntegerSetValue), EmptyIntegerSetDomain)
        assertEq(createDomain(CompleteIntegerSetValue, CompleteIntegerSetValue), new SingletonIntegerSetDomain(CompleteIntegerRange))
        assertEq(createDomain(EmptyIntegerSetValue, CompleteIntegerSetValue), CompleteIntegerSetDomain)
    }

    @Test
    def testVariableFactories(): Unit = {
        val space = new Space(logger, sigint)
        val dx = new IntegerPowerSetDomain(NonNegativeIntegerRange)
        val x = createVariable(space, "x", dx)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, dx)
        assertEq(c.domain, CompleteIntegerSetDomain)
    }

    @Test
    def testValueCasting(): Unit = {
        assertThrows(safeDowncast(Zero), classOf[ClassCastException])
        safeDowncast(new IntegerSetValue(CompleteIntegerRange))
    }

    @Test
    def testDomainCasting(): Unit = {
        safeDowncast(new SingletonIntegerSetDomain(CompleteIntegerRange))
        safeDowncast(new IntegerPowerSetDomain(CompleteIntegerRange))
        assertThrows(safeDowncast(CompleteIntegerRange), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting(): Unit = {
        val space = new Space(logger, sigint)
        val b = space.createVariable("b", CompleteBooleanDomain)
        val s = space.createVariable("s", new IntegerPowerSetDomain(CompleteIntegerRange))
        safeDowncast(s)
        assertThrows(safeDowncast(b), classOf[ClassCastException])
    }

    @Test
    def testConfiguration(): Unit = {
        assertEq(valueClass, classOf[IntegerSetValue])
        assertEq(domainClass, classOf[IntegerSetDomain])
        assertEq(variableClass, classOf[IntegerSetVariable])
        assertEq(valueOrdering, IntegerSetValueOrdering)
        assertEq(costModel, IntegerSetValueOrderingCostModel)
        assertEq(domainOrdering, IntegerSetDomainOrdering)
        assertEq(domainPruner, IntegerSetDomainPruner)
    }

}
