package yuck.constraints.test

import org.junit._

import scala.collection._
import scala.language.existentials

import yuck.constraints._
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CumulativeTest extends UnitTest with AssignmentPropagationTestTooling {

    private def createTask(space: Space, i: Int, d: IntegerDomain): CumulativeTask =
        new CumulativeTask(
            new IntegerVariable(space.nextVariableId, "s%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "d%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "d%d".format(i), d))

    @Test
    def testSearchVariables: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        assertEq(space.searchVariables, Set(t1.s, t1.d, t1.c, t2.s, t2.d, t2.c, ub))
    }

    @Test
    def testTaskMovement: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: two tasks with same start time exceed capacity",
                    (t1.s, Zero), (t1.d, Three), (t1.c, Two), (t2.s, Zero), (t2.d, Four), (t2.c, Three), (ub, Three),
                    (costs, False6)),
                ConsultAndCommit("move t2 to reduce conflict", (t2.s, One), (costs, False4)),
                ConsultAndCommit("move t2 to resolve conflict", (t2.s, Three), (costs, True)),
                ConsultAndCommit("move both tasks in one move", (t1.s, One), (t2.s, Two), (costs, False4))))
    }

    @Test
    def testTaskResizing: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: two tasks with same start time exceed capacity",
                    (t1.s, Zero), (t1.d, Three), (t1.c, Two), (t2.s, Zero), (t2.d, Four), (t2.c, Three), (ub, Three),
                    (costs, False6)),
                ConsultAndCommit("reduce conflict by reducing the the duration of t1", (t1.d, One), (costs, False2)),
                ConsultAndCommit("resolve conflict by setting the duration of t1 to zero", (t1.d, Zero), (costs, True)),
                ConsultAndCommit(
                    "restore duration of t1 and, instead, set its consumption to zero",
                    (t1.d, Three), (t1.c, Zero),
                    (costs, True)),
                ConsultAndCommit("increase consumption of t2 beyond capacity", (t2.c, Four), (costs, False4)),
                ConsultAndCommit(
                    "change duration and resource consumption of both tasks in one move",
                    (t1.d, Four), (t1.c, Two), (t2.d, Five), (t2.c, Three),
                    (costs, False8))))
    }

    @Test
    def testCapacityChanges: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val tasks = (1 to 1).map(createTask(space, _, d))
        val Vector(t1) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: consumption exceeds capacity",
                    (t1.s, Zero), (t1.d, Three), (t1.c, Four), (ub, Three),
                    (costs, False3)),
                ConsultAndCommit("resolve conflict by increasing capacity", (ub, Four), (costs, True)),
                ConsultAndCommit("increase consumption to cause conflict", (t1.c, Five), (costs, False3))))
    }

    @Test
    def testHandlingOfDuplicateVariables: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val s2 = new IntegerVariable(space.nextVariableId, "s2", d)
        val d2 = new IntegerVariable(space.nextVariableId, "d2", d)
        val c2 = new IntegerVariable(space.nextVariableId, "c2", d)
        val t1 = new CumulativeTask(ub, ub, ub)
        val t2 = new CumulativeTask(ub, ub, ub)
        val t3 = new CumulativeTask(s2, d2, c2)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, Vector(t1, t2, t3), ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "baseline: capacity is zero, so t1 and t2 are empty and t3 exceeds capacity",
                    (ub, Zero), (s2, Three), (d2, One), (c2, One),
                    (costs, False)),
                ConsultAndCommit("increase capacity and hence duration and consumption of t1 and t2", (ub, One), (costs, False)),
                ConsultAndCommit("increasing capacity once more makes t1 and t2 overlap t3", (ub, Two), (costs, False5))))
    }

    @Test
    def testConsultWithoutCommit: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val tasks = (1 to 1).map(createTask(space, _, d))
        val Vector(t1) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: consumption is too high",
                    (t1.s, Zero), (t1.d, Three), (t1.c, Two),  (ub, One),
                    (costs, False3)),
                Consult("resolve conflict by reducing consumption", (t1.c, One), (costs, True)),
                Consult("resolve conflict by increasing capacity", (ub, Two), (costs, True))))
    }

    @Test
    def testComplexMoves: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "no initial conflict",
                    (t1.s, Zero), (t1.d, Three), (t1.c, One), (t2.s, Three), (t2.d, Four), (t2.c, Three), (ub, Three),
                    (costs, True)),
                ConsultAndCommit("move and resize t1 in one move", (t1.s, Two), (t1.d, Seven), (t1.c, Two), (costs, False8)),
                ConsultAndCommit(
                    "change consumption of both tasks and capacity in one move",
                    (t1.d, Six), (t1.c, Four), (t2.s, Seven), (t2.c, Two), (ub, Four),
                    (costs, False2))))
    }

    @Test
    def testHandlingOfNegativeDurationAndConsumption: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict",
                    (t1.s, Zero), (t1.d, One), (t1.c, One), (t2.s, Zero), (t2.d, One), (t2.c, One), (ub, Zero),
                    (costs, False2)),
                ConsultAndCommit("reduce conflict by setting duration of t1 to -1", (t1.d, MinusOne), (costs, False)),
                ConsultAndCommit("resolve conflict by setting consumption of t2 to -1", (t2.c, MinusOne), (costs, True))))
    }

}
