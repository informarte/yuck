package yuck.core

import scala.collection._

/**
 * Defines a neighbourhood for local search.
 *
 * @author Michael Marte
 */
abstract class Neighbourhood {

    /** Returns the variables that may occur in the moves produced by this neighbourhood. */
    def searchVariables: Set[AnyVariable]

    /** Returns the children of this neighbourhood. */
    def children: IterableOnce[Neighbourhood]

    /** Creates a move. */
    def nextMove: Move

}
