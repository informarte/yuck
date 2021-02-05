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
final class InverseTest extends UnitTest with AssignmentPropagationTestTooling {

    @Test
    def testInverseWithIdenticalOffsets: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(One, Three)
        val f1 = new IntegerVariable(space.nextVariableId, "f1", d)
        val f2 = new IntegerVariable(space.nextVariableId, "f2", d)
        val f3 = new IntegerVariable(space.nextVariableId, "f3", d)
        val g1 = new IntegerVariable(space.nextVariableId, "g1", d)
        val g2 = new IntegerVariable(space.nextVariableId, "g2", d)
        val g3 = new IntegerVariable(space.nextVariableId, "g3", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val f = immutable.IndexedSeq(f1, f2, f3)
        val g = immutable.IndexedSeq(g1, g2, g3)
        space.post(new Inverse(space.nextConstraintId, null, new InverseFunction(f, 1), new InverseFunction(g, 1), costs))
        assertEq(space.searchVariables, Set(f1, f2, f3, g1, g2, g3))
        runScenario(
            TestScenario(
                space,
                // 3 1 2
                // 3 1 2
                Initialize("setup", (f1, Three), (f2, One), (f3, Two), (g1, Three), (g2, One), (g3, Two), (costs, False8)),
                // swap values of g1 and g3:
                // 3 1 2
                // 2 1 3
                // This move checks the functioning of the logic that avoids revisiting the same node twice
                // when computing the cost delta.
                // Here f2 is the node in question because it is referenced by before(g3) and after(g1).
                ConsultAndCommit("1", (g1, Two), (g3, Three), (costs, False6)),
                // swap values of f1 and f3:
                // 2 1 3
                // 2 1 3
                ConsultAndCommit("2", (f1, Two), (f3, Three), (costs, True)),
                // swap values of f2 and g1:
                // 2 2 3
                // 1 1 3
                ConsultAndCommit("3", (f2, Two), (g1, One), (costs, False2)),
                // reverting previous move in two steps, this is step one:
                // 2 1 3
                // 1 1 3
                ConsultAndCommit("4a", (f2, One), (costs, False2)),
                // ... this is step two:
                // 2 1 3
                // 2 1 3
                ConsultAndCommit("4b", (g1, Two), (costs, True))))
    }

    @Test
    def testInverseWithDifferentOffsets: Unit = {
        val space = new Space(logger, sigint)
        val fd = IntegerRange(Zero, Two)
        val gd = IntegerRange(One, Three)
        val f1 = new IntegerVariable(space.nextVariableId, "f1", fd)
        val f2 = new IntegerVariable(space.nextVariableId, "f2", fd)
        val f3 = new IntegerVariable(space.nextVariableId, "f3", fd)
        val g1 = new IntegerVariable(space.nextVariableId, "g1", gd)
        val g2 = new IntegerVariable(space.nextVariableId, "g2", gd)
        val g3 = new IntegerVariable(space.nextVariableId, "g3", gd)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val f = immutable.IndexedSeq(f1, f2, f3)
        val g = immutable.IndexedSeq(g1, g2, g3)
        space.post(new Inverse(space.nextConstraintId, null, new InverseFunction(f, 1), new InverseFunction(g, 0), costs))
        assertEq(space.searchVariables, Set(f1, f2, f3, g1, g2, g3))
        runScenario(
            TestScenario(
                space,
                // 2 0 1
                // 3 1 2
                Initialize("setup", (f1, Two), (f2, Zero), (f3, One), (g1, Three), (g2, One), (g3, Two), (costs, False8)),
                // swap values of g1 and g3:
                // 2 0 1
                // 2 1 3
                // This move checks the functioning of the logic that avoids revisiting the same node twice
                // when computing the cost delta.
                // Here f2 is the node in question because it is referenced by before(g3) and after(g1).
                ConsultAndCommit("1", (g1, Two), (g3, Three), (costs, False6)),
                // swap values of f1 and f3:
                // 1 0 2
                // 2 1 3
                ConsultAndCommit("2", (f1, One), (f3, Two), (costs, True))))
    }

}
