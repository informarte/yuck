package yuck.core

import yuck.util.OrderingFromOrdered

/**
 * A lexicographic ordering on the boundaries of integer-set domains.
 *
 * (There is currently no implementation of IntegerSetDomain that allows for holes.)
 *
 * @author Michael Marte
 */
object IntegerSetDomainOrdering extends OrderingFromOrdered[OrderedDomain[IntegerSetValue]]
