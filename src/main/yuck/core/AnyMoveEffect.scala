package yuck.core

import scala.collection.mutable.Cloneable

/**
 * Describes the effect of a move on the value of a variable.
 *
 * @author Michael Marte
 */
abstract class AnyMoveEffect extends Cloneable[AnyMoveEffect] {

    /** Returns the affected variable. */
    def anyVariable: AnyVariable

    /** Returns the value assigned to the variable by the move. */
    def anyValue: AnyValue

    override def toString = (anyVariable -> anyValue).toString

    /** Changes the given space's assignment such that it maps the variable to the value. */
    def affect(space: Space): Unit

}
