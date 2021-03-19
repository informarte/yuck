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
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class RegularTest extends UnitTest with AssignmentPropagationTestTooling {

    @Test
    def testRegular: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(One, Two)
        val xs = for (i <- 1 to 10) yield new IntegerVariable(space.nextVariableId, "x[%d]".format(i), d)
        val Q = 6
        val S = 2
        val delta = immutable.IndexedSeq(1, 2, 3, 0, 3, 4, 0, 5, 0, 6, 6, 0).grouped(2).toIndexedSeq
        val q0 = 1
        val F = IntegerRange(Six, Six)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Regular(space.nextConstraintId, null, xs, Q, S, delta, q0, F, costs))
        assertEq(space.searchVariables, xs.toSet)
        runScenario(
            TestScenario(
                space,
                // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
                Initialize(
                    "setup",
                    (xs(0), One), (xs(1), One), (xs(2), One), (xs(3), Two), (xs(4), One),
                    (xs(5), Two), (xs(6), Two), (xs(7), Two), (xs(8), One), (xs(9), One),
                    (costs, True)),
                // input:  1, 1, 1, 2, 2, 2, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 0, 0, 0, 0, 0, 0
                ConsultAndCommit("1", (xs(4), Two), (costs, False6)),
                // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 2
                // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 0
                ConsultAndCommit("2", (xs(4), One), (xs(9), Two), (costs, False)),
                // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
                ConsultAndCommit("3", (xs(9), One), (costs, True))))
    }

}
