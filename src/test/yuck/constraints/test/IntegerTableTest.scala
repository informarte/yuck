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
final class IntegerTableTest
    extends UnitTest
    with CostComputationTestTooling[BooleanValue]
    with PropagationTestTooling
{

    private def createTable(m: Int)(elems: Int*) =
        elems.toIndexedSeq.map(IntegerValue.get).grouped(m).toIndexedSeq

    @Test
    def testConsultAndCommit: Unit = {
        val space = new Space(logger, sigint)
        val xs = Vector("s", "t", "u").map(new IntegerVariable(space.nextVariableId, _, new IntegerRange(Zero, Nine)))
        val Vector(s, t, u) = xs
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val rows = createTable(3)(0, 0, 0, 1, 2, 3)
        space.post(new IntegerTable(space.nextConstraintId, null, xs, rows, costs))
        assertEq(space.searchVariables, xs.toSet)
        runScenario(
            CostComputationTestScenario(
                space,
                costs,
                Initialize("initial conflict", False3, (s, One), (t, One), (u, One)),
                ConsultAndCommit("move away from the first row and approach the second row", False2, (t, Two)),
                ConsultAndCommit("change two values at once", False, (s, Zero), (u, Three))))
    }

    @Test
    def testConsultAndCommitWithDuplicateVariables: Unit = {
        val space = new Space(logger, sigint)
        val Vector(s, t) = Vector("s", "t").map(new IntegerVariable(space.nextVariableId, _, new IntegerRange(Zero, Nine)))
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val rows = createTable(3)(0, 0, 0, 1, 2, 3, 2, 2, 3)
        space.post(new IntegerTable(space.nextConstraintId, null, Vector(s, s, t), rows, costs))
        assertEq(space.searchVariables, Set(s, t))
        runScenario(
            CostComputationTestScenario(
                space,
                costs,
                Initialize("initial conflict", False3, (s, One), (t, One)),
                ConsultAndCommit("fix first two columns", False2, (s, Two)),
                ConsultAndCommit("fix last column", True, (t, Three)),
                ConsultAndCommit("change two values at once", False2, (s, Zero), (t, Two))))
    }

    @Test
    def testPropagation: Unit = {
        val space = new Space(logger, sigint)
        val s = new IntegerVariable(space.nextVariableId, "s", new IntegerRange(Two, Five))
        val t = new IntegerVariable(space.nextVariableId, "t", new IntegerRange(Two, Three))
        val costs = new BooleanVariable(space.nextVariableId, "costs", TrueDomain)
        val rows =
            createTable(2)(
                0, 0,
                1, 0, 1, 1, 1, 4,
                2, 0, 2, 2,
                3, 0, 3, 3,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
        space.post(new IntegerTable(space.nextConstraintId, null, Vector(s, t), rows, costs))
        runScenario(
            PropagationTestScenario(
                space,
                Propagate(
                    "1",
                    Nil,
                    List[AnyDomainReduction](
                        (s, IntegerDomain.createDomain(List(Two, Three, Five))),
                        (t, IntegerDomain.createDomain(List(Two, Three))))),
                Propagate(
                    "2",
                    (t, new IntegerRange(Two, Two)),
                    (s, IntegerDomain.createDomain(List(Two, Five))))))
    }

    @Test
    def testPropagationWithDuplicateVariables: Unit = {
        val space = new Space(logger, sigint)
        val s = new IntegerVariable(space.nextVariableId, "s", new IntegerRange(Two, Five))
        val costs = new BooleanVariable(space.nextVariableId, "costs", TrueDomain)
        val rows =
            createTable(2)(
                0, 0,
                1, 0, 1, 4,
                2, 0, 2, 2,
                3, 0,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
        space.post(new IntegerTable(space.nextConstraintId, null, Vector(s, s), rows, costs))
        runScenario(
            PropagationTestScenario(
                space,
                Propagate(
                    "1",
                    Nil,
                    (s, IntegerDomain.createDomain(List(Two, Four))))))
    }

}
