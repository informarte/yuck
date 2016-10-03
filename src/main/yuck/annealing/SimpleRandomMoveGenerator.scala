package yuck.annealing

import scala.collection._

import yuck.core._

/**
 * Generates random moves involving one variable.
 *
 * @author Michael Marte
 */
final class SimpleRandomMoveGenerator
    (space: Space,
     override val xs: immutable.IndexedSeq[AnyVariable],
     randomGenerator: RandomGenerator)
    extends MoveGenerator
{
    require(! xs.isEmpty)
    override def nextMove =
        xs
        .apply(randomGenerator.nextInt(xs.length))
        .nextMove(space, randomGenerator)
}
