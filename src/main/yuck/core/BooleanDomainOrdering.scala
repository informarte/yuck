package yuck.core

import yuck.util.OrderingFromOrdered

/**
 * A total ordering on Boolean domains.
 *
 * @author Michael Marte
 */
implicit object BooleanDomainOrdering extends OrderingFromOrdered[OrderedDomain[BooleanValue]]
