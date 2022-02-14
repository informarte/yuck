package yuck.constraints

import com.conversantmedia.util.collection.geometry.{Point2d, Rect2d}
import com.conversantmedia.util.collection.spatial.{HyperPoint, HyperRect, RectBuilder}

import scala.collection.*

import yuck.core.*
import yuck.util.alg.rtree.RicherPoint2d

/**
 * A data structure to provide a single rectangle to a [[yuck.constraints.Disjoint2 Disjoint2]] constraint.
 *
 * @author Michael Marte
 */
final class Disjoint2Rect
    (val x: IntegerVariable, val y: IntegerVariable, val w: IntegerVariable, val h: IntegerVariable)
{
    override def toString = "(%s, %s, %s, %s)".format(x, y, w, h)
}

/**
 * Constrains n 2-dimensional rectangles to be non-overlapping.
 *
 * Uses an R tree to keep track of rectangle placements in order to provide the total overlap
 * as measure of constraint violation.
 *
 * Ignores rectangles with negative width or height.
 *
 * @author Michael Marte
 */
final class Disjoint2
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     rects: immutable.IndexedSeq[Disjoint2Rect],
     isStrict: Boolean,
     costs: BooleanVariable)
    extends Disjoint(id, rects.size, costs)
{

    override def toString = "disjoint2([%s], %s)".format(rects.mkString(", "), costs)

    override protected def variablesIterator(i: Int) =
        new Iterator[IntegerVariable] {
            private val r = rects(i)
            private var j = 0
            override def hasNext = j < 4
            override def next() = {
                j += 1
                j match {
                    case 1 => r.x
                    case 2 => r.y
                    case 3 => r.w
                    case 4 => r.h
                }
            }
        }

    override protected type BBox = Rect2d

    // Rectangles may be identical, so we use the index of originating Disjoint2Rect instance
    // to distinguish them.
    protected class Disjoint2Entry
        (val i: Int, val hasZeroWidth: Boolean, val hasZeroHeight: Boolean, override val bbox: Rect2d)
        extends HasBBox
    {
        override def hashCode = i
        override def equals(that: Any) = that match {
            case rhs: Disjoint2Entry =>
                val lhs = this
                lhs.i == rhs.i &&
                lhs.hasZeroWidth == rhs.hasZeroWidth && lhs.hasZeroHeight == rhs.hasZeroHeight &&
                lhs.bbox == rhs.bbox
            case _ => false
        }
        override def toString =
            "%d -> %s (%s)".format(i, bbox, if (hasZeroWidth || hasZeroHeight) "empty" else "non-empty")
    }

    override protected type RTreeEntry = Disjoint2Entry

    protected override val rectBuilder =
        new RectBuilder[RTreeEntry] {
            override def getBBox(entry: RTreeEntry) =
                entry.bbox
            override def getMbr(p1: HyperPoint, p2: HyperPoint) =
                new Rect2d(p1.getCoord(0), p1.getCoord(1), p2.getCoord(0), p2.getCoord(1))
        }

    protected override def createRTreeEntry(i: Int, searchState: SearchState) = {
        val r = rects(i)
        val x1 = searchState.value(r.x).value
        val y1 = searchState.value(r.y).value
        val w = searchState.value(r.w).value
        val h = searchState.value(r.h).value
        val hasZeroWidth = w == 0
        val hasZeroHeight = h == 0
        val x2 = safeAdd(x1, if (hasZeroWidth && isStrict) 1 else max(w, 0))
        val y2 = safeAdd(y1, if (hasZeroHeight && isStrict) 1 else max(h, 0))
        val entry = new RTreeEntry(i, hasZeroWidth, hasZeroHeight, new Rect2d(x1, y1, x2, y2))
        entry
    }

    protected override def computeOverlap(e1: Disjoint2Entry, e2: Disjoint2Entry) = {
        val r1 = e1.bbox
        val r2 = e2.bbox
        val min1 = r1.getMin.asInstanceOf[Point2d]
        val max1 = r1.getMax.asInstanceOf[Point2d]
        val min2 = r2.getMin.asInstanceOf[Point2d]
        val max2 = r2.getMax.asInstanceOf[Point2d]
        val r1x1 = min1.x
        val r1y1 = min1.y
        val r1x2 = max1.x
        val r1y2 = max1.y
        val r2x1 = min2.x
        val r2y1 = min2.y
        val r2x2 = max2.x
        val r2y2 = max2.y
        val xOverlap = max(0, min(r1x2, r2x2) - max(r1x1, r2x1))
        val yOverlap = max(0, min(r1y2, r2y2) - max(r1y1, r2y1))
        val overlap: Long = safeMul(xOverlap.toLong, yOverlap.toLong)
        if (isStrict &&
            overlap > 0 &&
            ((e1.hasZeroWidth && (r1x1 == r2x1 || r1x1 == r2x2)) ||
             (e2.hasZeroWidth && (r2x1 == r1x1 || r2x1 == r1x2)) ||
             (e1.hasZeroHeight && (r1y1 == r2y1 || r1y1 == r2y2)) ||
             (e2.hasZeroHeight && (r2y1 == r1y1 || r2y1 == r1y2))))
        {
            // e1 and e2 are adjacent
            0
        } else {
            overlap
        }
    }

}
