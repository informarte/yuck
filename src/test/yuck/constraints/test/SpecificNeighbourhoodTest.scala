package yuck.constraints.test

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}

import yuck.core.*
import yuck.test.util.UnitTest

@Execution(ExecutionMode.CONCURRENT)
abstract class SpecificNeighbourhoodTest extends UnitTest {

    protected val randomGenerator = new JavaRandomGenerator
    protected val space = new Space(logger, sigint)
    private val now = space.searchState

    protected val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    protected lazy val constraint: Constraint

    protected val propagate = false

    protected val expectedNeighbourhoodClass: Class[? <: Neighbourhood]

    protected def checkSearchState(searchState: SearchState): Unit

    protected val sampleSize: Int = 1000
    protected val moveSizeDistribution: Distribution = Distribution(1, List(90, 10))

    @Test
    protected final def testMoveGeneration(): Unit = {
        val neighbourhood = createNeighbourhood()
        val xs = neighbourhood.searchVariables
        for i <- 1 to sampleSize do {
            val move = neighbourhood.nextMove()
            assert(! move.involves(costs))
            val after = space.consult(move)
            assert(xs.exists(x => now.value(x) != after.value(x)))
            checkSearchState(after)
            if randomGenerator.nextDecision() then {
                space.commit(move)
                neighbourhood.commit(move)
            }
        }
    }

    protected val numberOfPerturbations: Int = 1000
    protected val perturbationProbability: Probability = Probability(0.5)

    @Test
    protected final def testPerturbation(): Unit = {
        val neighbourhood = createNeighbourhood()
        val xs = neighbourhood.searchVariables
        for i <- 1 to numberOfPerturbations do {
            val before = now.clone()
            neighbourhood.perturb(perturbationProbability)
            assert(xs.exists(x => now.value(x) != before.value(x)))
            checkSearchState(now)
        }
    }

    private def createNeighbourhood(): Neighbourhood = {
        space.post(constraint)
        if propagate then {
            space.propagate()
        }
        val xs = space.searchVariables
        xs.foreach(space.registerObjectiveVariable)
        space.registerObjectiveVariable(costs)
        val neighbourhood = constraint.createNeighbourhood(space, randomGenerator, logger, sigint, moveSizeDistribution).get
        assertEq(neighbourhood.getClass, expectedNeighbourhoodClass)
        assertEq(neighbourhood.searchVariables, xs)
        checkSearchState(now)
        space.registerImplicitConstraint(constraint)
        space.initialize()
        neighbourhood
    }

}
