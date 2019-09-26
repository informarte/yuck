package yuck.core

import yuck.util.OrderingFromOrdered

/**
 * A total ordering on Boolean domains.
 *
 * @author Michael Marte
 */
final object BooleanDomainOrdering extends OrderingFromOrdered[OrderedDomain[BooleanValue]]
