package yuck.core

import scala.collection._

/**
 * Provides methods for pruning integer domains.
 *
 * @author Michael Marte
 */
final object IntegerDomainPruner {
    def eq(lhs: IntegerDomain, rhs: IntegerValue): IntegerDomain =
        lhs.intersect(new IntegerDomain(rhs))
    def eq(lhs: IntegerDomain, rhs: IntegerDomain): IntegerDomain =
        lhs.intersect(rhs)
    def ne(lhs: IntegerDomain, rhs: IntegerValue): IntegerDomain =
        lhs.subtract(new IntegerDomain(rhs))
    def ne(lhs: IntegerDomain, rhs: IntegerDomain): IntegerDomain =
        lhs.subtract(rhs)
    def le(lhs: IntegerDomain, rhs: IntegerValue): IntegerDomain =
        lhs.intersect(new IntegerDomain(null, rhs))
    def le(lhs: IntegerValue, rhs: IntegerDomain): IntegerDomain =
        rhs.intersect(new IntegerDomain(lhs, null))
    def le(lhs: IntegerDomain, rhs: IntegerDomain): (IntegerDomain, IntegerDomain) =
        if (lhs.isEmpty || rhs.isEmpty)
            (EmptyIntegerDomain, EmptyIntegerDomain)
        else
            (lhs.intersect(new IntegerDomain(null, rhs.ub)),
             rhs.intersect(new IntegerDomain(lhs.lb, null)))
    def lt(lhs: IntegerDomain, rhs: IntegerValue): IntegerDomain =
        lhs.intersect(new IntegerDomain(null, rhs - One))
    def lt(lhs: IntegerValue, rhs: IntegerDomain): IntegerDomain =
        rhs.intersect(new IntegerDomain(lhs + One, null))
    def lt(lhs: IntegerDomain, rhs: IntegerDomain): (IntegerDomain, IntegerDomain) =
        if (lhs.isEmpty || rhs.isEmpty)
            (EmptyIntegerDomain, EmptyIntegerDomain)
        else
            (lhs.intersect(new IntegerDomain(null, if (rhs.ub == null) null else rhs.ub - One)),
             rhs.intersect(new IntegerDomain(if (lhs.lb == null) null else lhs.lb + One, null)))
}
