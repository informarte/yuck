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
    def testSpecialDomains(): Unit = {
        assertEq(emptyDomain, EmptyBooleanDomain)
        assertEq(completeDomain, CompleteBooleanDomain)
    }

    @Test
    def testDomainFactories(): Unit = {
        assertEq(createDomain(Set()), EmptyBooleanDomain)
        assertEq(createDomain(Set(False)), FalseDomain)
        assertEq(createDomain(Set(True)), TrueDomain)
        assertEq(createDomain(Set(False, True)), CompleteBooleanDecisionDomain)
        assertEx(createDomain(Set(False, False2, True)))
        assertEq(createDomain(False, False), FalseDomain)
        assertEq(createDomain(False, True), EmptyBooleanDomain)
        assertEq(createDomain(True, False), CompleteBooleanDecisionDomain)
        assertEq(createDomain(True, True), TrueDomain)
    }

    @Test
    def testVariableFactories(): Unit = {
        val space = new Space(logger, sigint)
        val x = createVariable(space, "x", FalseDomain)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, FalseDomain)
        assertEq(c.domain, CompleteBooleanDecisionDomain)
    }

    @Test
    def testValueCasting(): Unit = {
        safeDowncast(False)
        assertEx(safeDowncast(Zero), classOf[ClassCastException])
    }

    @Test
    def testDomainCasting(): Unit = {
        safeDowncast(CompleteBooleanDecisionDomain)
        assertEx(safeDowncast(CompleteIntegerRange), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting(): Unit = {
        val space = new Space(logger, sigint)
        val b = space.createVariable("b", CompleteBooleanDecisionDomain)
        val i = space.createVariable("i", CompleteIntegerRange)
        safeDowncast(b)
        assertEx(safeDowncast(i), classOf[ClassCastException])
    }

    @Test
    def testConfiguration(): Unit = {
        assertEq(valueType, classOf[BooleanValue])
        assertEq(valueOrdering, BooleanValueOrdering)
        assertEq(orderingCostModel, BooleanValueOrderingCostModel)
        assertEq(domainOrdering, BooleanDomainOrdering)
        assertEq(domainPruner, BooleanDomainPruner)
    }

}
