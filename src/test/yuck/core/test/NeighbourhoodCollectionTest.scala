package yuck.core.test

import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.core.*

@ParameterizedClass
@MethodSource(Array("parameters"))
@Execution(ExecutionMode.CONCURRENT)
class NeighbourhoodCollectionTest
    (override protected val moveSizeDistribution: Distribution,
     override protected val maybeHotSpotDistribution: Option[Distribution],
     override protected val maybeFairChoiceRate: Option[Probability],
     numberOfVariables: Int)
    extends GeneralNeighbourhoodTest[IntegerValue, IntegerDomain, IntegerVariable]
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

    override protected val typeTraits = IntegerTypeTraits

    override protected val xs =
        for i <- 0 until numberOfVariables yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i), IntegerRange(0, numberOfVariables - 1))

    private lazy val neighbourhoods =
        for i <- 0 until numberOfVariables yield
            new CommitChecker(space, new RandomReassignmentGenerator(space, Vector(xs(i)), randomGenerator))

    override protected lazy val neighbourhood =
        new NeighbourhoodCollection(
            space, neighbourhoods, randomGenerator, Some(moveSizeDistribution), maybeHotSpotDistribution, maybeFairChoiceRate)

    override protected val acceptableMoveSizeFrequencyDeviation = new AcceptableDeviation(0.1, 0)
    override protected val acceptableVariableFrequencyDeviation = new AcceptableDeviation(0.2, 1)

}

object NeighbourhoodCollectionTest extends GeneralNeighbourhoodTestParameterFactory {

    override protected val moveSizeDistributions =
        List(List(100), List(90, 10), List(50, 35, 15), List(50, 25, 15, 10)).map(Distribution(1, _))

}
