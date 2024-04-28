package yuck.core.test

import org.junit.*

import yuck.core.{given, *}
import yuck.test.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerValueTraitsTest extends UnitTest {

    import IntegerValueTraits.*

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
        assertEx(safeDowncast(False), classOf[ClassCastException])
        safeDowncast(Zero)
    }

    @Test
    def testDomainCasting(): Unit = {
        safeDowncast(EmptyIntegerRange)
        safeDowncast(EmptyIntegerRangeList)
        assertEx(safeDowncast(EmptyBooleanDomain), classOf[ClassCastException])
    }

    @Test
    def testVariableCasting(): Unit = {
        val space = new Space(logger, sigint)
        val b = space.createVariable("b", CompleteBooleanDomain)
        val i = space.createVariable("i", CompleteIntegerRange)
        safeDowncast(i)
        assertEx(safeDowncast(b), classOf[ClassCastException])
    }

    @Test
    def testConfiguration(): Unit = {
        assertEq(valueType, classOf[IntegerValue])
        assertEq(valueOrdering, IntegerValueOperations)
        assertEq(numericalOperations, IntegerValueOperations)
        assertEq(costModel, IntegerValueOrderingCostModel)
        assertEq(domainOrdering, IntegerDomainOrdering)
        assertEq(domainPruner, IntegerDomainPruner)
    }

}
