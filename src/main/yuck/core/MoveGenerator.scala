package yuck.core

import scala.collection._

/**
 * Provides move generation.
 *
 * @author Michael Marte
 */
abstract class MoveGenerator {

    /** Returns the variables that may occur in the moves produced by this generator. */
    val xs: immutable.IndexedSeq[AnyVariable]

    /** Creates a move. */
    def nextMove: Move

}
