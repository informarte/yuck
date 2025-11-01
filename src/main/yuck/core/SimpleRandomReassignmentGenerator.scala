package yuck.core

import scala.collection.*

/**
 * Generates random moves involving one variable.
 *
 * @author Michael Marte
 */
final class SimpleRandomReassignmentGenerator
    (override protected val space: Space,
     xs: immutable.IndexedSeq[AnyVariable],
     randomGenerator: RandomGenerator)
    extends Neighbourhood
{

    require(! xs.isEmpty)
    require(xs.size == xs.toSet.size)
    require(xs.forall(space.isSearchVariable))
    require(xs.forall(_.domain.isFinite))

    override def searchVariables = xs.toSet

    override def children = Nil

    override def nextMove() =
        xs
        .apply(randomGenerator.nextInt(xs.length))
        .nextMove(space, randomGenerator)

    override def perturb(perturbationProbability: Probability) = {
        val move = new BulkMove(space.nextMoveId())
        for (x <- xs) {
            if (randomGenerator.nextDecision(perturbationProbability)) {
                move += x.nextRandomMoveEffect(space, randomGenerator)
            }
        }
        space.consult(move)
        space.commit(move)
    }

}
