package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.annealing._
import yuck.constraints._
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class AlldistinctTest extends UnitTest with ConstraintTestTooling {

    @Test
    def testAlldistinct: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Alldistinct(space.nextConstraintId, null, Vector(s, t, u), costs))
        assertEq(space.searchVariables, Set(s, t, u))
        runScenario(
            TestScenario(
                space,
                Initialize("init", (s, One), (t, One), (u, One), (costs, False2)),
                ConsultAndCommit("1", (s, Two), (costs, False))))
    }

    @Test
    def testAlldistinctWithAVariableOccuringTwice: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Alldistinct(space.nextConstraintId, null, Vector(s, t, t), costs))
        assertEq(space.searchVariables, Set(s, t))
        runScenario(
            TestScenario(
                space,
                Initialize("init", (s, One), (t, One), (costs, False2)),
                ConsultAndCommit("1", (s, Two), (costs, False)),
                ConsultAndCommit("2", (t, Two), (costs, False2))))
    }

    @Test
    def testAlldistinctWithImplicitSolving: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Two)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val constraint = new Alldistinct(space.nextConstraintId, null, Vector(s, t, u), costs)
        space.post(constraint)
        val now = space.searchState
        val maybeNeighbourhood =
            constraint.createNeighbourhood(
                space, new JavaRandomGenerator, DefaultMoveSizeDistribution, logger, sigint)
        assert(maybeNeighbourhood.isDefined)
        val neighbourhood = maybeNeighbourhood.get
        assertNe(now.value(s), now.value(t))
        assertNe(now.value(s), now.value(u))
        assertNe(now.value(t), now.value(u))
        assertEq(now.value(costs), True)
        space.initialize()
        val sampleSize = 1000
        for (i <- 1 to sampleSize) {
            val move = neighbourhood.nextMove
            val after = space.consult(move)
            assert(List(s, t, u).exists(x => now.value(x) != after.value(x)))
            assertNe(after.value(s), after.value(t))
            assertNe(after.value(s), after.value(u))
            assertNe(after.value(t), after.value(u))
            space.commit(move)
        }
    }

    @Test
    def testAlldistinctExceptZero: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new AlldistinctExceptZero(space.nextConstraintId, null, List(s, t, u), costs))
        assertEq(space.searchVariables, Set(s, t, u))
        runScenario(
            TestScenario(
                space,
                Initialize("init", (s, One), (t, One), (u, One), (costs, False2)),
                ConsultAndCommit("1", (s, Zero), (costs, False)),
                ConsultAndCommit("2", (t, Zero), (costs, True)),
                ConsultAndCommit("3", (u, Zero), (costs, True))))
    }

    @Test
    def testAlldistinctExceptZeroWithAVariableOccuringTwice: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new AlldistinctExceptZero(space.nextConstraintId, null, List(s, t, t), costs))
        assertEq(space.searchVariables, Set(s, t))
        runScenario(
            TestScenario(
                space,
                Initialize("init", (s, One), (t, One), (costs, False2)),
                ConsultAndCommit("1", (s, Zero), (costs, False)),
                ConsultAndCommit("2", (t, Zero), (costs, True)),
                ConsultAndCommit("3", (s, One), (costs, True)),
                ConsultAndCommit("4", (t, Two), (costs, False))))
    }

}
