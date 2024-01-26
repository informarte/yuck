package yuck.constraints

import com.conversantmedia.util.collection.spatial.{HyperRect, RectBuilder, SpatialSearch, SpatialSearches}

import java.util.function.Consumer

import scala.collection.*

import yuck.core.*
import yuck.util.alg.rtree.RTreeTransaction

/**
 * Constrains n k-dimensional rectangles to be non-overlapping.
 *
 * Uses an R tree to keep track of rectangle placements in order to provide the total overlap
 * as measure of constraint violation.
 *
 * Details like the representation of rectangles and the cost model have to be provided
 * by subclassing.
 *
 * @author Michael Marte
 */
abstract class Disjoint
    (id: Id[Constraint], n: Int, costs: BooleanVariable)
    extends Constraint(id)
{

    protected def variablesIterator(i: Int): Iterator[IntegerVariable]
    final override def inVariables = (0 until n).view.flatMap(variablesIterator)
    final override def outVariables = List(costs)

    protected type BBox <: HyperRect[_]
    protected trait HasBBox {
        def bbox: BBox
    }
    protected type RTreeEntry <: HasBBox
    protected val rectBuilder: RectBuilder[RTreeEntry]
    protected def createRTreeEntry(i: Int, searchState: SearchState): RTreeEntry
    private var rTree: SpatialSearch[RTreeEntry] = null
    private var rTreeTransaction: RTreeTransaction[RTreeEntry] = null

    private var currentCosts = 0L
    private var futureCosts = 0L
    protected def computeOverlap(e1: RTreeEntry, e2: RTreeEntry): Long

    private val x2is =
        (0 until n)
        .iterator
        .flatMap(i => variablesIterator(i).map((_, i)))
        .foldLeft(new mutable.HashMap[AnyVariable, mutable.Buffer[Int]]) {
            case (map, (x, i)) =>
                val buf = map.getOrElseUpdate(x, new mutable.ArrayBuffer[Int])
                buf += i
                map
        }
        .map{case (x, buf) => (x, buf.toVector)}
        .toMap

    private val effect = costs.reuseableEffect

    final override def initialize(now: SearchState) = {
        rTree = SpatialSearches.rTree[RTreeEntry](rectBuilder)
        rTreeTransaction = new RTreeTransaction[RTreeEntry](rTree, rectBuilder)
        currentCosts = 0L
        for (i <- 0 until n) {
            val newEntry = createRTreeEntry(i, now)
            rTree.intersects(
                newEntry.bbox,
                new Consumer[RTreeEntry] {
                    override def accept(intersectingEntry: RTreeEntry) = {
                        currentCosts =
                            safeAdd(currentCosts, computeOverlap(newEntry, intersectingEntry))
                    }
                }
            )
            rTree.add(newEntry)
        }
        assert(rTree.getEntryCount == n)
        effect.a = BooleanValue(currentCosts)
        effect
    }

    final override def consult(before: SearchState, after: SearchState, move: Move) = {
        rTreeTransaction.rollback()
        futureCosts = currentCosts
        val is =
            if (move.size == 1) x2is(move.effects.head.x)
            else move.involvedVariablesIterator.flatMap(x2is).to(mutable.Set)
        for (i <- is) {
            val beforeEntry = createRTreeEntry(i, before)
            rTreeTransaction.remove(beforeEntry)
            rTreeTransaction.intersects(
                beforeEntry.bbox,
                new Consumer[RTreeEntry] {
                    override def accept(intersectingEntry: RTreeEntry) = {
                        futureCosts -= computeOverlap(beforeEntry, intersectingEntry)
                    }
                }
            )
            val afterEntry = createRTreeEntry(i, after)
            rTreeTransaction.intersects(
                afterEntry.bbox,
                new Consumer[RTreeEntry] {
                    override def accept(intersectingEntry: RTreeEntry) = {
                        futureCosts =
                            safeAdd(futureCosts, computeOverlap(afterEntry, intersectingEntry))
                    }
                }
            )
            rTreeTransaction.add(afterEntry)
        }
        effect.a = BooleanValue(futureCosts)
        effect
    }

    final override def commit(before: SearchState, after: SearchState, move: Move) = {
        rTreeTransaction.commit()
        assert(rTree.getEntryCount == n)
        currentCosts = futureCosts
        effect
    }

}
