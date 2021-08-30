package yuck.constraints

import scala.collection.immutable

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class TableNeighbourhood
    [Value <: AnyValue]
    (space: Space,
     xs: immutable.IndexedSeq[Variable[Value]],
     rows: immutable.IndexedSeq[immutable.IndexedSeq[Value]],
     randomGenerator: RandomGenerator)
    extends Neighbourhood
{

    require(xs.size == xs.toSet.size)
    require(rows.size > 1)
    require(rows.forall(_.size == xs.size))

    private val effects = xs.map(_.reuseableEffect)

    override def searchVariables = xs.iterator.filterNot(_.domain.isSingleton).toSet

    override def children = Nil

    private var currentRowIndex = -1
    private var nextRowIndex = -1

    override def nextMove = {
        if (currentRowIndex == -1) {
            currentRowIndex = rows.indexOf(xs.map(space.searchState.value(_)))
            assert(currentRowIndex >= 0)
        }
        nextRowIndex = {
            val i = randomGenerator.nextInt(rows.size - 1)
            if (i < currentRowIndex) i else i + 1
        }
        val row = rows(nextRowIndex)
        var colIndex = xs.size - 1
        while (colIndex >= 0) {
            effects(colIndex).a = row(colIndex)
            colIndex -= 1
        }
        new ChangeValues(space.nextMoveId, effects)
    }

    override def commit(move: Move) = {
        currentRowIndex = nextRowIndex
    }

}
