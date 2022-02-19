package yuck.constraints.test

import org.junit.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.{Alldistinct, AlldistinctNeighbourhood}
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class AlldistinctNeighbourhoodTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    @Test
    def testMoveGeneration(): Unit = {
        val numberOfVariables = 100
        val xs = for (i <- 0 until numberOfVariables) yield {
            val dx = IntegerRange(1, 2 * numberOfVariables).diff(IntegerDomain(List(i)))
            new IntegerVariable(space.nextVariableId(), "x%d".format(i + 1), dx)
        }
        val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)
        val constraint = new Alldistinct(space.nextConstraintId(), null, xs, costs)
        space.post(constraint)
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[AlldistinctNeighbourhood[_]])
        assertEq(xs.map(now.value(_)).toSet.size, xs.size)
        assertEq(now.value(costs), True)
        space.initialize()
        val sampleSize = 1000
        for (i <- 1 to sampleSize) {
            val move = neighbourhood.nextMove
            val after = space.consult(move)
            assert(xs.exists(x => now.value(x) != after.value(x)))
            assertEq(xs.map(after.value(_)).toSet.size, xs.size)
            if (randomGenerator.nextDecision()) {
                space.commit(move)
                neighbourhood.commit(move)
            }
        }
    }

}
