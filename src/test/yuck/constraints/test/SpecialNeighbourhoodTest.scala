package yuck.constraints.test

import org.junit.Test

import scala.collection.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
abstract class SpecialNeighbourhoodTest extends UnitTest {

    protected val randomGenerator = new JavaRandomGenerator
    protected val space = new Space(logger, sigint)

    protected val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    protected def createConstraint(): Constraint

    protected val propagate = false

    protected val expectedNeighbourhoodClass: Class[_ <: Neighbourhood]

    protected def checkSearchState(searchState: SearchState): Unit

    protected val sampleSize: Int = 1000

    @Test
    protected final def testMoveGeneration(): Unit = {
        val constraint = createConstraint()
        space.post(constraint)
        if (propagate) {
            space.propagate()
        }
        val xs = space.searchVariables
        xs.foreach(space.registerObjectiveVariable)
        space.registerObjectiveVariable(costs)
        val neighbourhood = constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution).get
        assertEq(neighbourhood.getClass, expectedNeighbourhoodClass)
        assertEq(neighbourhood.searchVariables, xs)
        val now = space.searchState
        checkSearchState(now)
        space.registerImplicitConstraint(constraint)
        space.initialize()
        for (i <- 1 to sampleSize) {
            val move = neighbourhood.nextMove
            assert(! move.involves(costs))
            val after = space.consult(move)
            assert(xs.exists(x => now.value(x) != after.value(x)))
            checkSearchState(after)
            if (randomGenerator.nextDecision()) {
                space.commit(move)
                neighbourhood.commit(move)
            }
        }
    }

}
