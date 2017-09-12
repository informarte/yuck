package yuck.core

import IntegerDomain.createRange

/**
 * Provides methods for pruning integer domains.
 *
 * @author Michael Marte
 */
final object IntegerDomainPruner {
    def eq(lhs: IntegerDomain, rhs: IntegerValue): IntegerDomain =
        lhs.intersect(createRange(rhs, rhs))
    def eq(lhs: IntegerDomain, rhs: IntegerDomain): IntegerDomain =
        lhs.intersect(rhs)
    def ne(lhs: IntegerDomain, rhs: IntegerValue): IntegerDomain =
        lhs.diff(createRange(rhs, rhs))
    def le(lhs: IntegerDomain, rhs: IntegerValue): IntegerDomain =
        lhs.intersect(createRange(null, rhs))
    def le(lhs: IntegerValue, rhs: IntegerDomain): IntegerDomain =
        rhs.intersect(createRange(lhs, null))
    def le(lhs: IntegerDomain, rhs: IntegerDomain): (IntegerDomain, IntegerDomain) =
        if (lhs.isEmpty || rhs.isEmpty)
            (EmptyIntegerRange, EmptyIntegerRange)
        else
            (lhs.intersect(createRange(null, rhs.ub)),
             rhs.intersect(createRange(lhs.lb, null)))
    def lt(lhs: IntegerDomain, rhs: IntegerValue): IntegerDomain =
        lhs.intersect(createRange(null, rhs - One))
    def lt(lhs: IntegerValue, rhs: IntegerDomain): IntegerDomain =
        rhs.intersect(createRange(lhs + One, null))
    def lt(lhs: IntegerDomain, rhs: IntegerDomain): (IntegerDomain, IntegerDomain) =
        if (lhs.isEmpty || rhs.isEmpty)
            (EmptyIntegerRange, EmptyIntegerRange)
        else
            (lhs.intersect(createRange(null, if (! rhs.hasUb) null else rhs.ub - One)),
             rhs.intersect(createRange(if (! lhs.hasLb) null else lhs.lb + One, null)))
}
