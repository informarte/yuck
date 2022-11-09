package yuck.core

import yuck.util.OrderingFromOrdered

/**
 * A total ordering on integer domains.
 *
 * @author Michael Marte
 */
object IntegerDomainOrdering extends OrderingFromOrdered[OrderedDomain[IntegerValue]]
