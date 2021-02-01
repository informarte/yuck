package yuck.constraints.test

import org.junit._

import yuck.constraints.Eq
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class DomainPropagationTestToolingTest extends UnitTest with DomainPropagationTestTooling {

    private val space = new Space(logger, sigint)
    private val x = new IntegerVariable(space.nextVariableId, "x", CompleteIntegerRange)
    private val y = new IntegerVariable(space.nextVariableId, "y", CompleteIntegerRange)
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)

    @Test
    def testSetup: Unit = {
        var applied = false
        new Setup(() => {applied = true}).apply()
        assert(applied)
    }

    @Test
    def testCheck: Unit = {
        var applied = false
        new Check(() => {applied = true}).apply()
        assert(applied)
    }

    @Test
    def testImplicitConversionsToSetup: Unit = {
        def setup(setup: Setup) = setup()
        setup(() => {x.pruneDomain(PositiveIntegerRange); ()})
        assertEq(x.domain, PositiveIntegerRange)
        x.relaxDomain(CompleteIntegerRange)
        setup((x, PositiveIntegerRange))
        assertEq(x.domain, PositiveIntegerRange)
        x.relaxDomain(CompleteIntegerRange)
        setup(List((x, PositiveIntegerRange), (y, NegativeIntegerRange)))
        assertEq(x.domain, PositiveIntegerRange)
        assertEq(y.domain, NegativeIntegerRange)
        setup(List[AnyDomainReduction]((x, ZeroToOneIntegerRange), (costs, TrueDomain)))
        assertEq(x.domain, OneToOneIntegerRange)
        assertEq(costs.domain, TrueDomain)
    }

    @Test
    def testImplicitConversionsToCheck: Unit = {
        def check(check: Check) = check()
        assertEx(check(() => assertEq(x.domain, PositiveIntegerRange)), classOf[AssertionError])
        assertEx(check((x, PositiveIntegerRange)), classOf[AssertionError])
        x.pruneDomain(PositiveIntegerRange)
        check((x, PositiveIntegerRange))
        x.relaxDomain(CompleteIntegerRange)
        assertEx(check(List((x, PositiveIntegerRange), (y, NegativeIntegerRange))), classOf[AssertionError])
        x.pruneDomain(PositiveIntegerRange)
        assertEx(check(List((x, PositiveIntegerRange), (y, NegativeIntegerRange))), classOf[AssertionError])
        y.pruneDomain(NegativeIntegerRange)
        check(List((x, PositiveIntegerRange), (y, NegativeIntegerRange)))
        check(List[AnyDomainReduction]((x, PositiveIntegerRange), (costs, CompleteBooleanDomain)))
    }

    @Test
    def testPropagate: Unit = {
        space.post(new Eq(space.nextConstraintId, None, x, y, costs))
        var checked = false
        val scenario =
            TestScenario(
                space,
                Propagate(
                    "Propagate x >= 0 and y <= 0 and expect that the resulting domains of x and y are different",
                    List[AnyDomainReduction](
                        (x, NonNegativeIntegerRange), (y, NonPositiveIntegerRange), (costs, TrueDomain)),
                    () => {
                        assertEq(x.domain, y.domain)
                        assert(x.domain.isSingleton)
                        assertEq(x.domain.singleValue, Zero)
                        checked = true
                    }))
        runScenario(scenario)
        assert(checked)
        assertEq(x.domain, y.domain)
        assert(x.domain.isSingleton)
        assertEq(x.domain.singleValue, Zero)
    }

    @Test
    def testPropagateAndRollback: Unit = {
        space.post(new Eq(space.nextConstraintId, None, x, y, costs))
        var checked = false
        val scenario =
            TestScenario(
                space,
                PropagateAndRollback(
                    "Propagate x >= 0 and y <= 0 and expect that the resulting domains of x and y are different",
                    List[AnyDomainReduction](
                        (x, NonNegativeIntegerRange), (y, NonPositiveIntegerRange), (costs, TrueDomain)),
                    () => {
                        assertEq(x.domain, y.domain)
                        assert(x.domain.isSingleton)
                        assertEq(x.domain.singleValue, Zero)
                        checked = true
                    }))
        runScenario(scenario)
        assert(checked)
        assertEq(x.domain, CompleteIntegerRange)
        assertEq(y.domain, CompleteIntegerRange)
        assertEq(costs.domain, CompleteBooleanDomain)
    }

}
