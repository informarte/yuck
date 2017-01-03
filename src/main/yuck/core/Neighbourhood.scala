package yuck.core

import scala.collection._

/**
 * Defines a neighbourhood for local search.
 *
 * @author Michael Marte
 */
abstract class Neighbourhood {

    /** Returns the variables that may occur in the moves produced by this generator. */
    def searchVariables: TraversableOnce[AnyVariable]

    /** Creates a move. */
    def nextMove: Move

}
