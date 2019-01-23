package yuck.util.alg

import com.conversantmedia.util.collection.geometry.{Point2d, Rect2d}

/**
 * @author Michael Marte
 *
 */
package object rtree {

    /** Decorates Point2d with convenience methods under the assumption that coordinates are integers. */
    implicit final class RicherPoint2d(val point: Point2d) extends AnyVal {
        def x: Int = point.getCoord(Point2d.X).toInt
        def y: Int = point.getCoord(Point2d.Y).toInt
    }

    /** Decorates Rect2d with convenience methods under the assumption that coordinates are integers. */
    implicit final class RicherRect2d(val rect: Rect2d) extends AnyVal {
        def x1: Int = rect.getMin.asInstanceOf[Point2d].x
        def y1: Int = rect.getMin.asInstanceOf[Point2d].y
        def x2: Int = rect.getMax.asInstanceOf[Point2d].x
        def y2: Int = rect.getMax.asInstanceOf[Point2d].y
        def w: Int = rect.getRange(Point2d.X).toInt
        def h: Int = rect.getRange(Point2d.Y).toInt
        def isEmpty: Boolean = w == 0 || h == 0
    }

}
