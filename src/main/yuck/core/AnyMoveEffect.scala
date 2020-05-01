package yuck.core

import scala.collection.mutable.Cloneable

/**
 * Describes the effect of a move on the value of a variable.
 *
 * @author Michael Marte
 */
abstract class AnyMoveEffect extends Cloneable[AnyMoveEffect] with Iterable[AnyMoveEffect] {

    /** Returns the affected variable. */
    def x: AnyVariable

    /** Returns the value assigned to the variable by the move. */
    def a: AnyValue

    final override def toString = (x -> a).toString

    final override def iterator = Iterator.single(this)
    final override def foreach[U](f: AnyMoveEffect => U) = f(this)
    final override def size = 1

    /** Changes the given space's assignment such that it maps the variable to the value. */
    def affect(space: Space): Unit

}
