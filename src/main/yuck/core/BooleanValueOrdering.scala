package yuck.core

import yuck.util.OrderingFromOrdered

/**
 * A total ordering on Boolean values.
 *
 * @author Michael Marte
 */
implicit object BooleanValueOrdering extends OrderingFromOrdered[BooleanValue]
