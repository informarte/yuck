package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.*
import yuck.test.*
import yuck.test.util.UnitTest

final class IntegerTypeTraitsTest extends UnitTest {

    import IntegerTypeTraits.*

    @Test
    def testDomainCapabilities(): Unit = {
        assert(domainCapabilities.createDomain)
        assert(domainCapabilities.diff)
        assert(domainCapabilities.randomSubdomain)
        assert(domainCapabilities.size)
        assert(domainCapabilities.union)
    }

    @Test
    def testValueNormalization(): Unit = {
        assertEq(normalizedValue(MinusOne), MinusOne)
        assertEq(normalizedValue(Zero), Zero)
        assertEq(normalizedValue(One), One)
        assertEq(normalizedValue(Two), Two)
    }

    @Test
    def testSpecialValues(): Unit = {
        assertEq(zero, Zero)
        assertEq(one, One)
        assertEq(minValue.value, Long.MinValue)
        assertEq(maxValue.value, Long.MaxValue)
    }

    @Test
    def testSpecialDomains(): Unit = {
        assertEq(emptyDomain, EmptyIntegerRange)
        assertEq(completeDomain, CompleteIntegerRange)
    }

    @Test
    def testDomainFactories(): Unit = {
        assertEq(createDomain(Set()), EmptyIntegerRange)
        assertEq(createDomain(Set(Zero)), IntegerRange(0, 0))
        assertEq(createDomain(Set(Zero, One)), IntegerRange(0, 1))
        assert(createDomain(Zero, One).isInstanceOf[IntegerRange])
        assertEq(createDomain(null, null), CompleteIntegerRange)
        assertEq(createDomain(One, Zero), EmptyIntegerRange)
    }

    @Test
    def testVariableFactories(): Unit = {
        val space = new Space(logger, sigint)
        val x = createVariable(space, "x", NonNegativeIntegerRange)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, NonNegativeIntegerRange)
        assertEq(c.domain, CompleteIntegerRange)
    }

    @Test
    def testValueCasting(): Unit = {
        assertThrows(safeDowncast(False), classOf[ClassCastException])
        safeDowncast(Zero)
    }

    @Test
    def testDomainCasting(): Unit = {
        safeDowncast(EmptyIntegerRange)
        safeDowncast(EmptyIntegerRangeList)
        assertThrows(safeDowncast(EmptyBooleanDomain), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting(): Unit = {
        val space = new Space(logger, sigint)
        val b = space.createVariable("b", CompleteBooleanDomain)
        val i = space.createVariable("i", CompleteIntegerRange)
        safeDowncast(i)
        assertThrows(safeDowncast(b), classOf[ClassCastException])
    }

    @Test
    def testConfiguration(): Unit = {
        assertEq(valueClass, classOf[IntegerValue])
        assertEq(domainClass, classOf[IntegerDomain])
        assertEq(variableClass, classOf[IntegerVariable])
        assertEq(valueOrdering, IntegerValueOperations)
        assertEq(numericalOperations, IntegerValueOperations)
        assertEq(costModel, IntegerValueOrderingCostModel)
        assertEq(domainOrdering, IntegerDomainOrdering)
        assertEq(domainPruner, IntegerDomainPruner)
    }

}
