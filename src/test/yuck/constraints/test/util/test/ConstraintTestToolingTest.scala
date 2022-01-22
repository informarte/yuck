package yuck.constraints.test.util.test

import org.junit._

import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class ConstraintTestToolingTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)
    private val now = space.searchState
    private val x = new IntegerVariable(space.nextVariableId, "x", CompleteIntegerRange)
    private val y = new IntegerVariable(space.nextVariableId, "y", CompleteIntegerRange)
    private val z = new BooleanVariable(space.nextVariableId, "z", CompleteBooleanDomain)

    private class ConstraintMock1 extends Constraint(space.nextConstraintId) {
        override def inVariables = List(x, y)
        override def outVariables = Nil
        override def propagate = {
            val i = x.domain.intersect(y.domain)
            NoPropagationOccurred.pruneDomain(x, i).pruneDomain(y, i)
        }
        override def initialize(now: SearchState) = Nil
        override def consult(before: SearchState, after: SearchState, move: Move) = Nil
    }

    private def checkState1: Unit = {
        assertEq(space.searchState.mappedVariables, Set())
        assertGe(space.numberOfPropagations, 1)
        assertEq(space.numberOfInitializations, 0)
        assertEq(space.numberOfConsultations, 0)
        assertEq(space.numberOfCommitments, 0)
    }

    private class ConstraintMock2
        (initializeResult: BooleanValue, consultResult: BooleanValue, commitResult: BooleanValue)
        extends Constraint(space.nextConstraintId)
    {
        override def inVariables = List(x)
        override def outVariables = List(z)
        override def initialize(now: SearchState) =
            new ImmutableMoveEffect(z, initializeResult)
        override def consult(before: SearchState, after: SearchState, move: Move) =
            new ImmutableMoveEffect(z, consultResult)
        override def commit(before: SearchState, after: SearchState, move: Move) =
            new ImmutableMoveEffect(z, commitResult)
    }

    private def checkState2
        (numberOfInitializations: Int, numberOfConsultations: Int, numberOfCommitments: Int,
         a: IntegerValue, b: BooleanValue): Unit =
    {
        assertEq(space.numberOfPropagations, 0)
        assertEq(space.numberOfInitializations, numberOfInitializations)
        assertEq(space.numberOfConsultations, numberOfConsultations)
        assertEq(space.numberOfCommitments, numberOfCommitments)
        assertEq(now.value(x), a)
        assertEq(now.value(z), b)
    }

    @Test
    def testImplicitConversionsToDomainReduction: Unit = {
        def unpack(reduction: AnyDomainReduction) = (reduction.x, reduction.dx)
        assertEq(unpack((x, IntegerRange(0, 9))), (x, IntegerRange(0, 9)))
        assertEq(unpack((x, (3, 5))), (x, IntegerRange(3, 5)))
        assertEq(unpack((x, List(3, 5))), (x, IntegerDomain(3, 5)))
        assertEq(unpack((z, TrueDomain)), (z, TrueDomain))
        assertEq(unpack(x << IntegerRange(0, 9)), (x, IntegerRange(0, 9)))
        assertEq(unpack(x << (3, 5)), (x, IntegerRange(3, 5)))
        assertEq(unpack(x << List(3, 5)), (x, IntegerDomain(3, 5)))
        assertEq(unpack(x << List(Three, Five)), (x, IntegerDomain(3, 5)))
        assertEq(unpack(z << TrueDomain), (z, TrueDomain))
        assertEq(unpack(z << List(True)), (z, TrueDomain))
    }

    @Test
    def testThatPropagatePassesWhenExpectationIsMet: Unit = {
        space.post(new ConstraintMock1())
        val scenario =
            TestScenario(
                space,
                Propagate(
                    "x >= 0 and y <= 0 -> d(x) = d(y) = {0}",
                    List(x << NonNegativeIntegerRange, y << NonPositiveIntegerRange),
                    List(x << ZeroToZeroIntegerRange, y << ZeroToZeroIntegerRange)))
        runScenario(scenario)
        assertEq(x.domain, ZeroToZeroIntegerRange)
        assertEq(y.domain, ZeroToZeroIntegerRange)
        checkState1
    }

    @Test
    def testThatPropagateThrowsWhenExpectationIsNotMet: Unit = {
        space.post(new ConstraintMock1())
        val scenario =
            TestScenario(
                space,
                Propagate(
                    "x >= 0 and y <= 0 -> d(x) = {0} and d(y) = {0, 1}",
                    List(x << NonNegativeIntegerRange, y << NonPositiveIntegerRange),
                    List(x << ZeroToZeroIntegerRange, y << ZeroToOneIntegerRange)))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState1
    }

    @Test
    def testThatPropgateThrowsWhenUntestedVariableGotItsDomainReduced: Unit = {
        space.post(new ConstraintMock1())
        val scenario =
            TestScenario(
                space,
                Propagate(
                    "x = 0 and y <= 0",
                    List(x << ZeroToZeroIntegerRange, y << NonPositiveIntegerRange),
                    Nil))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState1
    }

    @Test
    def testThatPropagateAndRollbackPassesWhenExpectationIsMet: Unit = {
        space.post(new ConstraintMock1())
        val scenario =
            TestScenario(
                space,
                PropagateAndRollback(
                    "x >= 0 and y <= 0 -> d(x) = d(y) = {0}",
                    List(x << NonNegativeIntegerRange, y << NonPositiveIntegerRange),
                    List(x << ZeroToZeroIntegerRange, y << ZeroToZeroIntegerRange)))
        runScenario(scenario)
        assertEq(x.domain, CompleteIntegerRange)
        assertEq(y.domain, CompleteIntegerRange)
        checkState1
    }

    @Test
    def testThatPropagateAndRollbackThrowsWhenExpectationIsNotMet: Unit = {
        space.post(new ConstraintMock1())
        val scenario =
            TestScenario(
                space,
                PropagateAndRollback(
                    "x >= 0 and y <= 0 -> d(x) = {0} and d(y) = {0, 1}",
                    List(x << NonNegativeIntegerRange, y << NonPositiveIntegerRange),
                    List(x << ZeroToZeroIntegerRange, y << ZeroToOneIntegerRange)))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState1
    }

    @Test
    def testThatPropgateAndRollbackThrowsWhenUntestedVariableGotItsDomainReduced: Unit = {
        space.post(new ConstraintMock1())
        val scenario =
            TestScenario(
                space,
                PropagateAndRollback(
                    "x = 0 and y <= 0",
                    List(x << ZeroToZeroIntegerRange, y << NonPositiveIntegerRange),
                    Nil))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState1
    }

    @Test
    def testImplicitConversionsToMoveEffect: Unit = {
        def unpack(effect: AnyMoveEffect) = (effect.x, effect.a)
        assertEq(unpack((x, Three)), (x, Three))
        assertEq(unpack((z, False7)), (z, False7))
        assertEq(unpack(x << Three), (x, Three))
        assertEq(unpack(x << 3), (x, Three))
        assertEq(unpack(z << False7), (z, False7))
    }

    @Test
    def testImplicitConversionToInitialize: Unit = {
        val step = Initialize("x = 0 -> y = False", x << 0, z << False)
        assertEq(step.preconditions.size, 1)
        assertEq(step.preconditions(0).x, x)
        assertEq(step.preconditions(0).a, Zero)
        assertEq(step.postconditions(0).x, z)
        assertEq(step.postconditions(0).a, False)
    }

    @Test
    def testImplicitConversionToConsult: Unit = {
        val step = Consult("x = 0 -> y = False", x << 0, z << False)
        assertEq(step.preconditions.size, 1)
        assertEq(step.preconditions(0).x, x)
        assertEq(step.preconditions(0).a, Zero)
        assertEq(step.postconditions(0).x, z)
        assertEq(step.postconditions(0).a, False)
    }

    @Test
    def testImplicitConversionToConsultAndCommit: Unit = {
        val step = ConsultAndCommit("x = 0 -> y = False", x << 0, z << False)
        assertEq(step.preconditions.size, 1)
        assertEq(step.preconditions(0).x, x)
        assertEq(step.preconditions(0).a, Zero)
        assertEq(step.postconditions(0).x, z)
        assertEq(step.postconditions(0).a, False)
    }

    @Test
    def testThatInitializePassesWhenExpectationIsMet: Unit = {
        space.post(new ConstraintMock2(False, False, False))
        val scenario = TestScenario(space, Initialize("x = 0 -> y = False", List(x << Zero), List(z << False)))
        runScenario(scenario)
        checkState2(1, 0, 0, Zero, False)
    }

    @Test
    def testThatInitializeThrowsWhenExpectationIsNotMet: Unit = {
        space.post(new ConstraintMock2(False, False, False))
        val scenario = TestScenario(space, Initialize("x = 0 -> y = True", List(x << Zero), List(z << True)))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState2(1, 0, 0, Zero, False)
    }

    @Test
    def testThatInitializeThrowsWhenUntestedVariableGotItsValueChanged: Unit = {
        space.post(new ConstraintMock2(False, False, False)).setValue(z, True)
        val scenario = TestScenario(space, Initialize("x = 0", List(x << Zero), Nil))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState2(1, 0, 0, Zero, False)
    }

    @Test
    def testThatConsultPassesWhenExpectationIsMet: Unit = {
        space.post(new ConstraintMock2(False, True, True)).setValue(x, Zero).initialize()
        assertEq(now.value(z), False)
        val scenario = TestScenario(space, Consult("x = 1 -> y = True", List(x << One), List(z << True)))
        runScenario(scenario)
        checkState2(1, 1, 0, Zero, False)
    }

    @Test
    def testThatConsultThrowsWhenExpectationIsNotMet: Unit = {
        space.post(new ConstraintMock2(False, True, True)).setValue(x, Zero).initialize()
        assertEq(now.value(z), False)
        val scenario = TestScenario(space, Consult("x = 1 -> y = False", List(x << One), List(z << False)))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState2(1, 1, 0, Zero, False)
    }

    @Test
    def testThatConsultThrowsWhenUntestedVariableGotItsValueChanged: Unit = {
        space.post(new ConstraintMock2(False, True, True)).setValue(x, Zero).initialize()
        assertEq(now.value(z), False)
        val scenario = TestScenario(space, Consult("x = 1", List(x << One), Nil))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState2(1, 1, 0, Zero, False)
    }

    @Test
    def testThatConsultAndCommitPassesWhenExpectationsAreMet: Unit = {
        space.post(new ConstraintMock2(True, False, False)).setValue(x, Zero).initialize()
        assertEq(now.value(z), True)
        val scenario = TestScenario(space, ConsultAndCommit("x = 1 -> y = False", List(x << One), List(z << False)))
        runScenario(scenario)
        checkState2(1, 1, 1, One, False)
    }

    @Test
    def testThatConsultAndCommitThrowsWhenExpectationIsNotMetInConsultPhase: Unit = {
        space.post(new ConstraintMock2(True, True, False)).setValue(x, Zero).initialize()
        assertEq(now.value(z), True)
        val scenario = TestScenario(space, ConsultAndCommit("x = 1 -> y = False", List(x << One), List(z << False)))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState2(1, 1, 0, Zero, True)
    }

    @Test
    def testThatConsultAndCommitThrowsWhenExpectationIsNotMetInCommitPhase: Unit = {
        space.post(new ConstraintMock2(True, False, True)).setValue(x, Zero).initialize()
        assertEq(now.value(z), True)
        val scenario = TestScenario(space, ConsultAndCommit("x = 1 -> y = False", List(x << One), List(z << False)))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState2(1, 1, 1, One, True)
    }

    @Test
    def testThatConsultAndCommitThrowsWhenUntestedVariableGotItsValueChangedInConsultPhase: Unit = {
        space.post(new ConstraintMock2(True, False, False)).setValue(x, Zero).initialize()
        assertEq(now.value(z), True)
        val scenario = TestScenario(space, ConsultAndCommit("x = 1", List(x << One), Nil))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState2(1, 1, 0, Zero, True)
    }

    @Test
    def testThatConsultAndCommitThrowsWhenUntestedVariableGotItsValueChangedInCommitPhase: Unit = {
        space.post(new ConstraintMock2(True, True, False)).setValue(x, Zero).initialize()
        assertEq(now.value(z), True)
        val scenario = TestScenario(space, ConsultAndCommit("x = 1", List(x << One), Nil))
        assertEx(runScenario(scenario), classOf[AssertionError])
        checkState2(1, 1, 1, One, False)
    }

}
