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
    def testSpecialValues: Unit = {
        assertEq(zero, Zero)
        assertEq(one, One)
    }

    @Test
    def testSpecialDomains: Unit = {
        assertEq(emptyDomain, EmptyIntegerRange)
        assertEq(completeDomain, CompleteIntegerRange)
    }

    @Test
    def testDomainFactories: Unit = {
        assertEq(createDomain(Set()), EmptyIntegerRange)
        assertEq(createDomain(Set(Zero)), ZeroToZeroIntegerRange)
        assertEq(createDomain(Set(Zero, One)), ZeroToOneIntegerRange)
        assert(createDomain(Zero, One).isInstanceOf[IntegerRange])
        assertEq(createDomain(null, null), CompleteIntegerRange)
        assertEq(createDomain(One, Zero), EmptyIntegerRange)
    }

    @Test
    def testVariableFactories: Unit = {
        val space = new Space(logger, sigint)
        val x = createVariable(space, "x", NonNegativeIntegerRange)
        val c = createChannel(space)
        assertEq(x.name, "x")
        assertEq(x.domain, NonNegativeIntegerRange)
        assertEq(c.domain, CompleteIntegerRange)
    }

    @Test
    def testValueCasting: Unit = {
        assertEx(safeDowncast(False), classOf[ClassCastException])
        safeDowncast(Zero)
    }

    @Test
    def testDomainCasting: Unit = {
        safeDowncast(EmptyIntegerRange)
        safeDowncast(EmptyIntegerRangeList)
        assertEx(safeDowncast(EmptyBooleanDomain), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting: Unit = {
        val space = new Space(logger, sigint)
        val b = space.createVariable("b", CompleteBooleanDecisionDomain)
        val i = space.createVariable("i", CompleteIntegerRange)
        safeDowncast(i)
        assertEx(safeDowncast(b), classOf[ClassCastException])
    }

    @Test
    def testConfiguration: Unit = {
        assertEq(valueType, classOf[IntegerValue])
        assertEq(orderingCostModel, IntegerOrderingCostModel)
        assertEq(domainPruner, IntegerDomainPruner)
    }

}
