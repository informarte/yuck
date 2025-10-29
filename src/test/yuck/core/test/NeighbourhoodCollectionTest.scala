package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
class NeighbourhoodCollectionTest
    (randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     maybeHotSpotDistribution: Option[Distribution],
     maybeFairChoiceRate: Option[Probability],
     numberOfVariables: Int)
    extends UnitTest
{

    final class CommitChecker
       (override protected val space: Space, neighbourhood: Neighbourhood)
        extends Neighbourhood
    {
        var lastMove: Move = null
        override def searchVariables = neighbourhood.searchVariables
        override def children = neighbourhood.children
        override def nextMove() = {
            lastMove = neighbourhood.nextMove()
            lastMove
        }
        override def commit(move: Move) = {
            assert(lastMove.ne(null))
            assertEq(lastMove, move)
            neighbourhood.commit(move)
            lastMove = null
        }
        override def perturb(perturbationProbability: Probability) = {
            neighbourhood.perturb(perturbationProbability)
        }
    }

    private val domains = for (i <- 0 until numberOfVariables) yield IntegerRange(0, numberOfVariables - 1)
    private val (space, xs) = NeighbourhoodTestHelper.createSpace(logger, sigint, randomGenerator, domains)
    val neighbourhoods =
        for (i <- 0 until numberOfVariables) yield
            new CommitChecker(space, new SimpleRandomReassignmentGenerator(space, Vector(xs(i)), randomGenerator))
    val neighbourhood =
        new NeighbourhoodCollection(
            space, neighbourhoods, randomGenerator, Some(moveSizeDistribution), maybeHotSpotDistribution, maybeFairChoiceRate)
    private val helper =
        new NeighbourhoodTestHelper(
            space, neighbourhood, xs, moveSizeDistribution, maybeHotSpotDistribution, maybeFairChoiceRate, logger)

    @Test
    def testMoveGeneration(): Unit = {
        val result = helper.testMoveGeneration()
        helper.checkMoveSizeFrequencies(result, 0.1, 0)
        helper.checkVariableFrequencies(result, 0.2, 0)
    }

    @Test
    def testPerturbation(): Unit = {
        helper.testPerturbation()
    }

}

/**
 * @author Michael Marte
 *
 */
object NeighbourhoodCollectionTest extends NeighbourhoodTestGenerator {

    override protected val moveSizeDistributions =
        List(List(100), List(90, 10), List(50, 35, 15), List(50, 25, 15, 10)).map(Distribution(1, _))

}
