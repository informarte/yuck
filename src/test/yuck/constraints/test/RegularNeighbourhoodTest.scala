package yuck.constraints.test

import scala.jdk.CollectionConverters.*
import yuck.constraints.{Regular, RegularDfa, RegularNeighbourhood}
import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class RegularNeighbourhoodTest extends SpecialNeighbourhoodTest {

    private val xs = for (i <- 1 to 10) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), IntegerRange(1, 3))
    private val Seq(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = xs
    private val Q = 6
    private val S = 3
    // To test the handling of multiple links between pairs of states, the automaton handles input 3 like 2.
    private val delta = Vector(1, 2, 2, 3, 0, 0, 3, 4, 4, 0, 5, 5, 0, 6, 6, 6, 0, 0).grouped(S).toVector
    private val q0 = 1
    private val F = IntegerRange(6, 6)
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    override protected def createConstraint() =
        new Regular(space.nextConstraintId(), null, new RegularDfa(xs, Q, S, delta, q0, F), costs, logger)

    override protected def checkSearchState(searchState: SearchState) = {
        assert(xs.forall(_.hasValidValue(searchState)))
        val finalState = xs.foldLeft(q0)((q, x) => if q == 0 then 0 else delta(q - 1)(searchState.value(x).toInt - 1))
        assert(F.contains(IntegerValue(finalState)))
        assertEq(searchState.value(costs), True)
    }

    override protected val expectedNeighbourhoodClass = classOf[RegularNeighbourhood]

}
