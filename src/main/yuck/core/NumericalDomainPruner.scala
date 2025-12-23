package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 */
abstract class NumericalDomainPruner[A <: NumericalValue[A], D <: NumericalDomain[A, D]] extends OrderedDomainPruner[A, D] {

    override protected val typeTraits: NumericalTypeTraits[A, D, ?]

    def absRule(lhs: D, rhs: D): (D, D) = (lhs, rhs)

    def linEqRule(lhs: Iterable[(A, D)], rhs: D): (Iterator[D], D) = (lhs.iterator.map(_._2), rhs)

    def timesRule(dx: D, dy: D, dz: D): (D, D, D) = (dx, dy, dz)

}
