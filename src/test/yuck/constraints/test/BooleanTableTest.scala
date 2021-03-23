package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.constraints._
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanTableTest
    extends UnitTest
    with AssignmentPropagationTestTooling
    with DomainPropagationTestTooling
{

    private def createTable(m: Int)(elems: BooleanValue*): immutable.IndexedSeq[immutable.IndexedSeq[BooleanValue]] =
        elems.toIndexedSeq.grouped(m).toIndexedSeq

    @Test
    def testConsultAndCommit: Unit = {
        val space = new Space(logger, sigint)
        val xs = Vector("s", "t", "u", "v").map(new BooleanVariable(space.nextVariableId, _, CompleteBooleanDecisionDomain))
        val Vector(s, t, u, v) = xs
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val rows =
            createTable(4)(
                False, False, False, False,
                True,  True,  True,  True)
        space.post(new BooleanTable(space.nextConstraintId, null, xs, rows, costs))
        assertEq(space.searchVariables, xs.toSet)
        runScenario(
            TestScenario(
                space,
                Initialize("initial conflict", (s, True), (t, False), (u, False), (v, False), (costs, False)),
                ConsultAndCommit("move away from the first row and approach the second row", (t, True), (costs, False2)),
                ConsultAndCommit("resolve conflict by changing two values at once", (u, True), (v, True), (costs, True))))
    }

    @Test
    def testConsultAndCommitWithDuplicateVariables: Unit = {
        val space = new Space(logger, sigint)
        val Vector(s, t, u) =
            Vector("s", "t", "u").map(new BooleanVariable(space.nextVariableId, _, CompleteBooleanDecisionDomain))
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val rows =
            createTable(4)(
                False, False, False, False,
                False, False, True,  True)
        space.post(new BooleanTable(space.nextConstraintId, null, Vector(s, s, t, u), rows, costs))
        assertEq(space.searchVariables, Set(s, t, u))
        runScenario(
            TestScenario(
                space,
                Initialize("initial conflict", (s, True), (t, True), (u, False), (costs, False3)),
                ConsultAndCommit("fix first two columns", (s, False), (costs, False)),
                ConsultAndCommit("fix third column", (t, False), (costs, True)),
                ConsultAndCommit("change two values at once", (s, True), (t, True), (costs, False3))))
    }

    @Test
    def testPropagation: Unit = {
        val space = new Space(logger, sigint)
        val xs = Vector("s", "t", "u", "v").map(new BooleanVariable(space.nextVariableId, _, CompleteBooleanDecisionDomain))
        val Vector(s, t, u, v) = xs
        val costs = new BooleanVariable(space.nextVariableId, "costs", TrueDomain)
        val rows =
            createTable(4)(
                True,  False, False, False,
                True,  True,  False, False,
                False, False, True,  True,
                False, True,  True,  True)
        space.post(new BooleanTable(space.nextConstraintId, null, xs, rows, costs))
        runScenario(
            TestScenario(
                space,
                Propagate(
                    "1",
                    Nil,
                    List((s, CompleteBooleanDecisionDomain), (t, CompleteBooleanDecisionDomain),
                         (u, CompleteBooleanDecisionDomain), (v, CompleteBooleanDecisionDomain))),
                Propagate(
                    "2",
                    (v, TrueDomain),
                    List((s, FalseDomain), (t, CompleteBooleanDecisionDomain), (u, TrueDomain), (v, TrueDomain)))))
    }

    @Test
    def testPropagationWithDuplicateVariables: Unit = {
        val space = new Space(logger, sigint)
        val Vector(s, t, u) =
            Vector("s", "t", "u").map(new BooleanVariable(space.nextVariableId, _, CompleteBooleanDecisionDomain))
        val costs = new BooleanVariable(space.nextVariableId, "costs", TrueDomain)
        val rows =
            createTable(4)(
                True,  False, True,  False,
                True,  True,  False, True,
                False, False, True,  False,
                True,  True,  False, True)
        space.post(new BooleanTable(space.nextConstraintId, null, Vector(s, s, t, u), rows, costs))
        runScenario(
            TestScenario(
                space,
                Propagate(
                    "1",
                    Nil,
                    List((s, CompleteBooleanDecisionDomain), (t, CompleteBooleanDecisionDomain),
                         (u, CompleteBooleanDecisionDomain))),
                Propagate(
                    "2",
                    (s, TrueDomain),
                    List((t, FalseDomain), (u, TrueDomain)))))
    }

}
