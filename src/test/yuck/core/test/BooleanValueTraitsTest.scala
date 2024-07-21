package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanValueTraitsTest extends UnitTest {

    import BooleanValueTraits.*

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
        assertEq(normalizedValue(True), True)
        assertEq(normalizedValue(False), False)
        assertEq(normalizedValue(False2), False)
    }

    @Test
    def testSpecialDomains(): Unit = {
        assertEq(emptyDomain, EmptyBooleanDomain)
        assertEq(completeDomain, CompleteBooleanDomain)
    }

    @Test
    def testDomainFactories(): Unit = {
        assertEq(createDomain(Set()), EmptyBooleanDomain)
        assertEq(createDomain(Set(False)), FalseDomain)
        assertEq(createDomain(Set(True)), TrueDomain)
        assertEq(createDomain(Set(False, True)), CompleteBooleanDomain)
        assertEx(createDomain(Set(False, False2, True)))
        assertEq(createDomain(False, False), FalseDomain)
        assertEq(createDomain(False, True), EmptyBooleanDomain)
        assertEq(createDomain(True, False), CompleteBooleanDomain)
        assertEq(createDomain(True, True), TrueDomain)
    }

    @Test
    def testVariableFactories(): Unit = {
        val space = new Space(logger, sigint)
        val x = createVariable(space, "x", FalseDomain)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, FalseDomain)
        assertEq(c.domain, CompleteBooleanDomain)
    }

    @Test
    def testValueCasting(): Unit = {
        safeDowncast(False)
        assertEx(safeDowncast(Zero), classOf[ClassCastException])
    }

    @Test
    def testDomainCasting(): Unit = {
        safeDowncast(CompleteBooleanDomain)
        assertEx(safeDowncast(CompleteIntegerRange), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting(): Unit = {
        val space = new Space(logger, sigint)
        val b = space.createVariable("b", CompleteBooleanDomain)
        val i = space.createVariable("i", CompleteIntegerRange)
        safeDowncast(b)
        assertEx(safeDowncast(i), classOf[ClassCastException])
    }

    @Test
    def testConfiguration(): Unit = {
        assertEq(valueType, classOf[BooleanValue])
        assertEq(valueOrdering, BooleanValueOrdering)
        assertEq(costModel, BooleanValueOrderingCostModel)
        assertEq(domainOrdering, BooleanDomainOrdering)
        assertEq(domainPruner, BooleanDomainPruner)
    }

}
