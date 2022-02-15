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

    private val domains =
        for (i <- 0 until numberOfVariables) yield IntegerRange(Zero, IntegerValue(numberOfVariables - 1))
    private val (space, xs) =
        NeighbourhoodTestHelper.createSpace(logger, sigint, randomGenerator, domains)
    private val helper =
        new NeighbourhoodTestHelper(logger, xs, moveSizeDistribution, maybeHotSpotDistribution, maybeFairChoiceRate)

    @Test
    def testMoveGeneration(): Unit = {
        val neighbourhoods =
            for (i <- 0 until numberOfVariables) yield
                new SimpleRandomReassignmentGenerator(space, Vector(xs(i)), randomGenerator)
        val neighbourhood =
            new NeighbourhoodCollection(
                neighbourhoods, randomGenerator, maybeHotSpotDistribution, maybeFairChoiceRate)
        val result = helper.measure(neighbourhood)
        helper.checkVariableFrequencies(result, 0.1, 0.1)
    }

    @Test
    def testCommitForwarding(): Unit = {
        final class CommitChecker(neighbourhood: Neighbourhood) extends Neighbourhood {
            var lastMove: Move = null
            override def searchVariables = neighbourhood.searchVariables
            override def children = neighbourhood.children
            override def nextMove = {
                assert(lastMove.eq(null))
                lastMove = neighbourhood.nextMove
                lastMove
            }
            override def commit(move: Move) = {
                assert(lastMove.ne(null))
                assertEq(lastMove, move)
                neighbourhood.commit(move)
                lastMove = null
            }
        }
        val neighbourhoods =
            for (i <- 0 until numberOfVariables) yield
                new CommitChecker(new SimpleRandomReassignmentGenerator(space, Vector(xs(i)), randomGenerator))
        val neighbourhood =
            new NeighbourhoodCollection(
                neighbourhoods, randomGenerator, maybeHotSpotDistribution, maybeFairChoiceRate)
        val numberOfTrials = 1000
        for (i <- 0 until numberOfTrials) {
            val move = neighbourhood.nextMove
            neighbourhood.commit(move)
        }
        for (neighbourhood <- neighbourhoods) {
            assert(neighbourhood.lastMove.eq(null))
        }
    }

}

/**
 * @author Michael Marte
 *
 */
object NeighbourhoodCollectionTest extends NeighbourhoodTestGenerator {

    protected override val moveSizeDistributions = List(List(100)).map(Distribution(1, _))

}
