package yuck.constraints.test

import org.junit._
import scala.collection._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanTableTest
    extends UnitTest
    with CostComputationTestTooling[BooleanValue]
    with PropagationTestTooling
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
            CostComputationTestScenario(
                space,
                costs,
                Initialize("initial conflict", False, (s, True), (t, False), (u, False), (v, False)),
                ConsultAndCommit("move away from the first row and approach the second row", False2, (t, True)),
                ConsultAndCommit("resolve conflict by changing two values at once", True, (u, True), (v, True))))
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
            CostComputationTestScenario(
                space,
                costs,
                Initialize("initial conflict", False3, (s, True), (t, True), (u, False)),
                ConsultAndCommit("fix first two columns", False, (s, False)),
                ConsultAndCommit("fix third column", True, (t, False)),
                ConsultAndCommit("change two values at once", False3, (s, True), (t, True))))
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
            PropagationTestScenario(
                space,
                Propagate(
                    "1",
                    Nil,
                    () => {
                        assertEq(s.domain, CompleteBooleanDecisionDomain)
                        assertEq(t.domain, CompleteBooleanDecisionDomain)
                        assertEq(u.domain, CompleteBooleanDecisionDomain)
                        assertEq(v.domain, CompleteBooleanDecisionDomain)
                    }),
                Propagate(
                    "2",
                    (v, TrueDomain),
                    () => {
                        assertEq(s.domain, FalseDomain)
                        assertEq(t.domain, CompleteBooleanDecisionDomain)
                        assertEq(u.domain, TrueDomain)
                        assertEq(v.domain, TrueDomain)
                    })))
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
            PropagationTestScenario(
                space,
                Propagate(
                    "1",
                    Nil,
                    () => {
                        assertEq(s.domain, CompleteBooleanDecisionDomain)
                        assertEq(t.domain, CompleteBooleanDecisionDomain)
                        assertEq(u.domain, CompleteBooleanDecisionDomain)
                    }),
                Propagate(
                    "2",
                    (s, TrueDomain),
                    () => {
                        assertEq(t.domain, FalseDomain)
                        assertEq(u.domain, TrueDomain)
                    })))
    }

}
