package yuck.constraints.test

import org.junit._

import scala.collection._
import scala.language.existentials

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CumulativeTest extends UnitTest with StandardConstraintTestTooling[BooleanValue] {

    private def createTask(space: Space, i: Int, d: IntegerDomain): CumulativeTask =
        new CumulativeTask(
            new IntegerVariable(space.nextVariableId, "s%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "d%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "d%d".format(i), d))

    @Test
    def testSearchVariables: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
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
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                costs,
                Initialize(
                    "initial conflict: two tasks with same start time exceed capacity",
                    False6,
                    (t1.s, Zero), (t1.d, Three), (t1.c, Two), (t2.s, Zero), (t2.d, Four), (t2.c, Three), (ub, Three)),
                ConsultAndCommit("move t2 to reduce conflict", False4, (t2.s, One)),
                ConsultAndCommit("move t2 to resolve conflict", True, (t2.s, Three)),
                ConsultAndCommit("move both tasks in one move", False4, (t1.s, One), (t2.s, Two))))
    }

    @Test
    def testTaskResizing: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                costs,
                Initialize(
                    "initial conflict: two tasks with same start time exceed capacity",
                    False6,
                    (t1.s, Zero), (t1.d, Three), (t1.c, Two), (t2.s, Zero), (t2.d, Four), (t2.c, Three), (ub, Three)),
                ConsultAndCommit("reduce conflict by reducing the the duration of t1", False2, (t1.d, One)),
                ConsultAndCommit("resolve conflict by setting the duration of t1 to zero", True, (t1.d, Zero)),
                ConsultAndCommit(
                    "restore duration of t1 and, instead, set its consumption to zero",
                    True,
                    (t1.d, Three), (t1.c, Zero)),
                ConsultAndCommit("increase consumption of t2 beyond capacity", False4, (t2.c, Four)),
                ConsultAndCommit(
                    "change duration and resource consumption of both tasks in one move",
                    False8,
                    (t1.d, Four), (t1.c, Two), (t2.d, Five), (t2.c, Three))))
    }

    @Test
    def testCapacityChanges: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 1).map(createTask(space, _, d))
        val Vector(t1) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                costs,
                Initialize(
                    "initial conflict: consumption exceeds capacity",
                    False3,
                    (t1.s, Zero), (t1.d, Three), (t1.c, Four), (ub, Three)),
                ConsultAndCommit("resolve conflict by increasing capacity", True, (ub, Four)),
                ConsultAndCommit("increase consumption to cause conflict", False3, (t1.c, Five))))
    }

    @Test
    def testHandlingOfDuplicateVariables: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
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
                costs,
                Initialize(
                    "baseline: capacity is zero, so t1 and t2 are empty and t3 exceeds capacity",
                    False,
                    (ub, Zero), (s2, Three), (d2, One), (c2, One)),
                ConsultAndCommit("increase capacity and hence duration and consumption of t1 and t2", False, (ub, One)),
                ConsultAndCommit("increasing capacity once more makes t1 and t2 overlap t3", False5, (ub, Two))))
    }

    @Test
    def testConsultWithoutCommit: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 1).map(createTask(space, _, d))
        val Vector(t1) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                costs,
                Initialize(
                    "initial conflict: consumption is too high",
                    False3,
                    (t1.s, Zero), (t1.d, Three), (t1.c, Two),  (ub, One)),
                Consult("resolve conflict by reducing consumption", True, (t1.c, One)),
                Consult("resolve conflict by increasing capacity", True, (ub, Two))))
    }

    @Test
    def testComplexMoves: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                costs,
                Initialize(
                    "no initial conflict",
                    True,
                    (t1.s, Zero), (t1.d, Three), (t1.c, One), (t2.s, Three), (t2.d, Four), (t2.c, Three), (ub, Three)),
                ConsultAndCommit("move and resize t1 in one move", False8, (t1.s, Two), (t1.d, Seven), (t1.c, Two)),
                ConsultAndCommit(
                    "change consumption of both tasks and capacity in one move",
                    False2,
                    (t1.d, Six), (t1.c, Four), (t2.s, Seven), (t2.c, Two), (ub, Four))))
    }

    @Test
    def testHandlingOfNegativeDurationAndConsumption: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                costs,
                Initialize(
                    "initial conflict",
                    False2,
                    (t1.s, Zero), (t1.d, One), (t1.c, One), (t2.s, Zero), (t2.d, One), (t2.c, One), (ub, Zero)),
                ConsultAndCommit("reduce conflict by setting duration of t1 to -1", False, (t1.d, MinusOne)),
                ConsultAndCommit("resolve conflict by setting consumption of t2 to -1", True, (t2.c, MinusOne))))
    }

}
