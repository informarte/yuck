package yuck.core

import yuck.util.OrderingFromOrdered

/**
 * A total ordering on integer domains.
 *
 * @author Michael Marte
 */
implicit object IntegerDomainOrdering extends OrderingFromOrdered[OrderedDomain[IntegerValue]]
