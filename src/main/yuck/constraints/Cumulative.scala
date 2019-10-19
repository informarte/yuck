package yuck.constraints

import com.conversantmedia.util.collection.geometry.{Point2d, Rect2d}
import com.conversantmedia.util.collection.spatial.{HyperPoint, HyperRect, RectBuilder, SpatialSearch, SpatialSearches}

import java.util.function.Consumer

import scala.collection._

import yuck.core._
import yuck.util.alg.rtree.{RTreeTransaction, RicherRect2d}


/**
 * A data structure to provide a single task to a [[yuck.constraints.Cumulative Cumulative]] constraint.
 *
 * @param s is the task start.
 * @param d is the task duration.
 * @param c is the task's resource consumption.
 *
 * @author Michael Marte
 */
final class CumulativeTask
    (val s: IntegerVariable, val d: IntegerVariable, val c: IntegerVariable)
{
    override def toString = "(%s, %s, %s)".format(s, d, c)
}

/**
 * Implements the ''cumulative'' constraint as specified by MiniZinc.
 *
 * Keeps track of resource consumption for each time slot in order to provide the amount
 * of unsatisfied requirements (summed up over time) as measure of constraint violation.
 *
 * Uses an R tree to track task placements and sweeping to compute costs and cost deltas.
 *
 * Ignores tasks with negative duration or consumption.
 *
 * @author Michael Marte
 */
final class Cumulative
    (id: Id[Constraint], goal: Goal,
     tasks: immutable.IndexedSeq[CumulativeTask], capacity: IntegerVariable,
     costs: BooleanVariable)
    extends Constraint(id, goal)
{

    private val n = tasks.size

    override def toString = "cumulative([%s], %s, %s)".format(tasks.mkString(", "), capacity, costs)

    private def variablesIterator(i: Int) =
        new Iterator[IntegerVariable] {
            private val t = tasks(i)
            private var j = 0
            override def hasNext = j < 3
            override def next = {
                j += 1
                j match {
                    case 1 => t.s
                    case 2 => t.d
                    case 3 => t.c
                }
            }
        }

    override def inVariables = (0 until n).flatMap(variablesIterator) :+ capacity
    override def outVariables = List(costs)

    // Rectangles may be identical, so we use the index of originating task to distinguish them.
    private class RTreeEntry(val i: Int, val bbox: Rect2d) {
        override def hashCode = i
        override def equals(that: Any) = that match {
            case rhs: RTreeEntry => {
                val lhs = this
                lhs.i == rhs.i && lhs.bbox == rhs.bbox
            }
            case _ => false
        }
        override def toString = "%d -> %s".format(i, bbox)
    }

    private val rectBuilder =
        new RectBuilder[RTreeEntry] {
            override def getBBox(entry: RTreeEntry) =
                entry.bbox
            override def getMbr(p1: HyperPoint, p2: HyperPoint) =
                new Rect2d(p1.getCoord(0), p1.getCoord(1), p2.getCoord(0), p2.getCoord(1))
        }

    private def createRTreeEntry(i: Int, searchState: SearchState) = {
        val t = tasks(i)
        val s = searchState.value(t.s).value
        val d = max(0, searchState.value(t.d).value)
        val c = max(0, searchState.value(t.c).value)
        val entry = new RTreeEntry(i, new Rect2d(s, 0, safeAdd(s, d), c))
        entry
    }

    private var rTree: SpatialSearch[RTreeEntry] = null
    private var rTreeTransaction: RTreeTransaction[RTreeEntry] = null

    private val x2is =
        (0 until n)
        .iterator
        .flatMap{case i => variablesIterator(i).map((_, i))}
        .foldLeft(new mutable.HashMap[AnyVariable, mutable.Buffer[Int]]) {
            case (map, (x, i)) =>
                val buf = map.getOrElseUpdate(x, new mutable.ArrayBuffer[Int])
                buf += i
                map
        }
        .map{case (x, buf) => (x, buf.toIndexedSeq)}
        .toMap

    private val effects = List(new ReusableMoveEffectWithFixedVariable[BooleanValue](costs))
    private val effect = effects.head

    private var currentCosts = 0L
    private var futureCosts = 0L

    private final class EventPoint(val x: Int, val bbox: Rect2d, val isBBoxStart: Boolean)
    private object EventPointOrdering extends Ordering[EventPoint] {
        override def compare(p1: EventPoint, p2: EventPoint) = java.lang.Integer.compare(p1.x, p2.x)
    }

    private def computeCosts(rTree: SpatialSearch[RTreeEntry], capacity: Int): Long = {
        require(capacity >= 0)
        val eventPoints = new java.util.ArrayList[EventPoint]
        rTree.forEach(
            new Consumer[RTreeEntry] {
                override def accept(entry: RTreeEntry) = {
                    val bbox = entry.bbox
                    eventPoints.add(new EventPoint(bbox.x1, bbox, true))
                    eventPoints.add(new EventPoint(bbox.x2, bbox, false))
                }
            }
        )
        eventPoints.sort(EventPointOrdering) // sort in place without copying (infeasible with Scala facilities)
        val n = eventPoints.size
        var costs = 0L
        if (n > 0) {
            assert(n > 1)
            var sweepLinePos = eventPoints.get(0).x
            var i = 0
            var consumption = 0
            do {
                do {
                    val p = eventPoints.get(i)
                    if (p.isBBoxStart) {
                        consumption = safeAdd(consumption, p.bbox.h)
                    } else {
                        consumption -= p.bbox.h
                    }
                    assert(consumption >= 0)
                    i += 1
                } while (i < n && eventPoints.get(i).x == sweepLinePos)
                if (i < n) {
                    val nextSweepLinePos = eventPoints.get(i).x
                    val segmentWidth = nextSweepLinePos - sweepLinePos
                    sweepLinePos = nextSweepLinePos
                    if (consumption > capacity) {
                        costs = safeAdd(costs, safeMul(segmentWidth.toLong, (consumption - capacity).toLong))
                    }
                }
            } while (i < n)
        }
        costs
    }

    private def computeCostDelta(rTree: SpatialSearch[RTreeEntry], x1: Int, x2: Int, consumptionDelta: Int, capacity: Int): Long = {
        require(x1 < x2)
        require(consumptionDelta != 0)
        require(capacity >= 0)
        val eventPoints = new java.util.ArrayList[EventPoint]
        rTree.intersects(
            new Rect2d(x1, 0, x2, 1),
            new Consumer[RTreeEntry] {
                override def accept(entry: RTreeEntry) = {
                    val bbox = entry.bbox
                    val p1 = new EventPoint(max(x1, bbox.x1), bbox, true)
                    val p2 = new EventPoint(min(x2, bbox.x2), bbox, false)
                    if (p1.x < x2 && p2.x > x1) {
                        // entry really intersects with [x1, x2]
                        eventPoints.add(p1)
                        eventPoints.add(p2)
                    }
                }
            }
        )
        if (consumptionDelta > 0) {
            // add event points for the case that [x1, x2] is not (yet) fully covered by tasks
            val bbox = new Rect2d(x1, 0, x2, 0)
            eventPoints.add(new EventPoint(x1, bbox, true))
            eventPoints.add(new EventPoint(x2, bbox, false))
        }
        eventPoints.sort(EventPointOrdering) // sort in place without copying (infeasible with Scala facilities)
        val n = eventPoints.size
        assert(n > 1)
        assert(eventPoints.get(0).x == x1)
        assert(eventPoints.get(n - 1).x == x2)
        var sweepLinePos = eventPoints.get(0).x
        var i = 0
        var consumption = 0
        var costDelta = 0L
        do {
            do {
                val p = eventPoints.get(i)
                if (p.isBBoxStart) {
                    consumption = safeAdd(consumption, p.bbox.h)
                } else {
                    consumption -= p.bbox.h
                }
                assert(consumption >= 0)
                i += 1
            } while (i < n && eventPoints.get(i).x == sweepLinePos)
            if (i < n) {
                val nextSweepLinePos = eventPoints.get(i).x
                val segmentWidth = nextSweepLinePos - sweepLinePos
                if (consumptionDelta > 0) {
                    if (consumption >= capacity) {
                        costDelta = safeAdd(costDelta, safeMul(segmentWidth.toLong, consumptionDelta.toLong))
                    } else {
                        val futureConsumption = safeAdd(consumption, consumptionDelta)
                        if (futureConsumption > capacity) {
                            costDelta = safeAdd(costDelta, safeMul(segmentWidth.toLong, (futureConsumption - capacity).toLong))
                        }
                    }
                } else if (consumption > capacity) {
                    val futureConsumption = safeAdd(consumption, consumptionDelta)
                    assert(futureConsumption >= 0)
                    if (futureConsumption >= capacity) {
                        costDelta = safeAdd(costDelta, safeMul(segmentWidth.toLong, consumptionDelta.toLong))
                    } else {
                        costDelta = safeSub(costDelta, safeMul(segmentWidth.toLong, (consumption - capacity).toLong))
                    }
                }
                sweepLinePos = nextSweepLinePos
            }
        } while (i < n)
        costDelta
    }

    override def initialize(now: SearchState) = {
        rTree = SpatialSearches.rTree[RTreeEntry](rectBuilder)
        rTreeTransaction = new RTreeTransaction[RTreeEntry](rTree, rectBuilder)
        currentCosts = 0
        for (i <- 0 until n) {
            val entry = createRTreeEntry(i, now)
            rTree.add(entry)
        }
        currentCosts = computeCosts(rTree, now.value(capacity).value)
        assert(currentCosts >= 0)
        effect.a = BooleanValue.get(currentCosts)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        rTreeTransaction.rollback
        futureCosts = currentCosts
        val beforeCapacity = before.value(capacity).value
        val capacityChanged = move.involves(capacity)
        val is = move.involvedVariablesIterator.flatMap(x2is.getOrElse(_, Nil)).to(mutable.Set)
        for (i <- is) {
            val beforeEntry = createRTreeEntry(i, before)
            val beforeBbox = beforeEntry.bbox
            if (! capacityChanged && ! beforeBbox.isEmpty) {
                futureCosts = safeAdd(futureCosts, computeCostDelta(rTreeTransaction, beforeBbox.x1, beforeBbox.x2, -beforeBbox.h, beforeCapacity))
            }
            rTreeTransaction.remove(beforeEntry)
            val afterEntry = createRTreeEntry(i, after)
            val afterBbox = afterEntry.bbox
            if (! capacityChanged && ! afterBbox.isEmpty) {
                futureCosts = safeAdd(futureCosts, computeCostDelta(rTreeTransaction, afterBbox.x1, afterBbox.x2, afterBbox.h, beforeCapacity))
            }
            rTreeTransaction.add(afterEntry)
        }
        if (capacityChanged) {
            val afterCapacity = after.value(capacity).value
            futureCosts = computeCosts(rTreeTransaction, afterCapacity)
        }
        assert(futureCosts >= 0)
        effect.a = BooleanValue.get(futureCosts)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        rTreeTransaction.commit
        assert(rTree.getEntryCount == n)
        currentCosts = futureCosts
        effects
    }

}
