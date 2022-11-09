package yuck.constraints.test

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.{Inverse, InverseFunction, InverseNeighbourhood}
import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
abstract class InverseNeighbourhoodTest extends UnitTest {

    protected val randomGenerator = new JavaRandomGenerator
    protected val space = new Space(logger, sigint)

    protected def testMoveGeneration
        (f: InverseFunction, g: InverseFunction,
         sampleSize: Int,
         expectedNeighbourhoodClass: Class[_ <: InverseNeighbourhood]):
        Unit =
    {
        val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)
        val constraint = new Inverse(space.nextConstraintId(), null, f, g, costs)
        space.post(constraint)
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, expectedNeighbourhoodClass)
        val now = space.searchState
        assert(Inverse.areInverseFunctionsOfEachOther(f, g, now))
        assertEq(now.value(costs), True)
        space.initialize()
        for (i <- 1 to sampleSize) {
            val move = neighbourhood.nextMove
            val after = space.consult(move)
            assert(f.xs.exists(x => now.value(x) != after.value(x)))
            assert(g.xs.exists(x => now.value(x) != after.value(x)))
            assert(Inverse.areInverseFunctionsOfEachOther(f, g, after))
            if (randomGenerator.nextDecision()) {
                space.commit(move)
                neighbourhood.commit(move)
            }
        }
    }

}
