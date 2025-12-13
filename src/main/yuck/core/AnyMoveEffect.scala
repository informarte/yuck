package yuck.core

import scala.collection.mutable.Cloneable

/**
 * Describes the effect of a move on the value of a variable.
 *
 * Notice that AnyMoveEffect is an iterable of itself.
 * (This helps to avoid object allocations.)
 * Therefore, by default, the debugger of IntelliJ IDEA will not show x and a
 * but the elements of the iterable, i.e. the effect itself.
 * To inspect x and a, select the effect and then View as > Scala object.
 */
abstract class AnyMoveEffect extends Cloneable[AnyMoveEffect] with Iterable[AnyMoveEffect] {

    /** Returns the affected variable. */
    def x: AnyVariable

    /** Returns the value assigned to the variable by the move. */
    def a: AnyValue

    final override def toString = "(%s, %s)".format(x, a)

    inline final override def iterator = Iterator.single(this)
    inline final override def foreach[U](f: AnyMoveEffect => U) = f(this)
    final override def size = 1

    /** Changes the given space's assignment such that it maps the variable to the value. */
    def affect(space: Space): Unit

}
