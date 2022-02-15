package yuck.constraints.test

import org.junit.*

import yuck.constraints.*
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CumulativeTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val ub = new IntegerVariable(space.nextVariableId, "ub", baseDomain)
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)

    private def createTask(i: Int): CumulativeTask =
        new CumulativeTask(
            new IntegerVariable(space.nextVariableId, "s%d".format(i), baseDomain),
            new IntegerVariable(space.nextVariableId, "d%d".format(i), baseDomain),
            new IntegerVariable(space.nextVariableId, "c%d".format(i), baseDomain))

    @Test
    def testBasics(): Unit = {
        val tasks = (1 to 2).map(createTask)
        val Seq(t1, t2) = tasks
        val constraint = new Cumulative(space.nextConstraintId, null, tasks, ub, costs)
        assertEq(constraint.toString, "cumulative([(s1, d1, c1), (s2, d2, c2)], ub, costs)")
        assertEq(constraint.inVariables.size, 7)
        assertEq(constraint.inVariables.toSet, Set(t1.s, t1.d, t1.c, t2.s, t2.d, t2.c, ub))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testTaskMovement(): Unit = {
        val tasks = (1 to 2).map(createTask)
        val Seq(t1, t2) = tasks
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: two tasks with same start time exceed capacity",
                    t1.s << 0, t1.d << 3, t1.c << 2, t2.s << 0, t2.d << 4, t2.c << 3, ub << 3,
                    costs << False6),
                ConsultAndCommit("move t2 to reduce conflict", t2.s << 1, costs << False4),
                ConsultAndCommit("move t2 to resolve conflict", t2.s << 3, costs << True),
                ConsultAndCommit("move both tasks in one move", t1.s << 1, t2.s << 2, costs << False4)))
    }

    @Test
    def testTaskResizing(): Unit = {
        val tasks = (1 to 2).map(createTask)
        val Seq(t1, t2) = tasks
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: two tasks with same start time exceed capacity",
                    t1.s << 0, t1.d << 3, t1.c << 2, t2.s << 0, t2.d << 4, t2.c << 3, ub << 3,
                    costs << False6),
                ConsultAndCommit("reduce conflict by reducing the the duration of t1", t1.d << 1, costs << False2),
                ConsultAndCommit("resolve conflict by setting the duration of t1 to zero", t1.d << 0, costs << True),
                ConsultAndCommit(
                    "restore duration of t1 and, instead, set its consumption to zero",
                    t1.d << 3, t1.c << 0,
                    costs << True),
                ConsultAndCommit("increase consumption of t2 beyond capacity", t2.c << 4, costs << False4),
                ConsultAndCommit(
                    "change duration and resource consumption of both tasks in one move",
                    t1.d << 4, t1.c << 2, t2.d << 5, t2.c << 3,
                    costs << False8)))
    }

    @Test
    def testCapacityChanges(): Unit = {
        val tasks = (1 to 1).map(createTask)
        val Seq(t1) = tasks
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: consumption exceeds capacity",
                    t1.s << 0, t1.d << 3, t1.c << 4, ub << 3,
                    costs << False3),
                ConsultAndCommit("resolve conflict by increasing capacity", ub << 4, costs << True),
                ConsultAndCommit("increase consumption to cause conflict", t1.c << 5, costs << False3)))
    }

    @Test
    def testHandlingOfDuplicateVariables(): Unit = {
        val s2 = new IntegerVariable(space.nextVariableId, "s2", baseDomain)
        val d2 = new IntegerVariable(space.nextVariableId, "d2", baseDomain)
        val c2 = new IntegerVariable(space.nextVariableId, "c2", baseDomain)
        val t1 = new CumulativeTask(ub, ub, ub)
        val t2 = new CumulativeTask(ub, ub, ub)
        val t3 = new CumulativeTask(s2, d2, c2)
        space.post(new Cumulative(space.nextConstraintId, null, Vector(t1, t2, t3), ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "baseline: capacity is zero, so t1 and t2 are empty and t3 exceeds capacity",
                    ub << 0, s2 << 3, d2 << 1, c2 << 1,
                    costs << False),
                ConsultAndCommit("increase capacity and hence duration and consumption of t1 and t2", ub << 1, costs << False),
                ConsultAndCommit("increasing capacity once more makes t1 and t2 overlap t3", ub << 2, costs << False5)))
    }

    @Test
    def testConsultWithoutCommit(): Unit = {
        val tasks = (1 to 1).map(createTask)
        val Seq(t1) = tasks
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: consumption is too high",
                    t1.s << 0, t1.d << 3, t1.c << 2,  ub << 1,
                    costs << False3),
                Consult("resolve conflict by reducing consumption", t1.c << 1, costs << True),
                Consult("resolve conflict by increasing capacity", ub << 2, costs << True)))
    }

    @Test
    def testComplexMoves(): Unit = {
        val tasks = (1 to 2).map(createTask)
        val Seq(t1, t2) = tasks
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "no initial conflict",
                    t1.s << 0, t1.d << 3, t1.c << 1, t2.s << 3, t2.d << 4, t2.c << 3, ub << 3,
                    costs << True),
                ConsultAndCommit("move and resize t1 in one move", t1.s << 2, t1.d << 7, t1.c << 2, costs << False8),
                ConsultAndCommit(
                    "change consumption of both tasks and capacity in one move",
                    t1.d << 6, t1.c << 4, t2.s << 7, t2.c << 2, ub << 4,
                    costs << False2)))
    }

    @Test
    def testHandlingOfNegativeDurationAndConsumption(): Unit = {
        val tasks = (1 to 2).map(createTask)
        val Seq(t1, t2) = tasks
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict",
                    t1.s << 0, t1.d << 1, t1.c << 1, t2.s << 0, t2.d << 1, t2.c << 1, ub << 0,
                    costs << False2),
                ConsultAndCommit("reduce conflict by setting duration of t1 to -1", t1.d << -1, costs << False),
                ConsultAndCommit("resolve conflict by setting consumption of t2 to -1", t2.c << -1, costs << True)))
    }

}
