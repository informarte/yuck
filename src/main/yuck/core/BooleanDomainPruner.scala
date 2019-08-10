package yuck.core

/**
 * Provides methods for pruning Boolean domains.
 *
 * To simplify the implementation, channel domains are sometimes translated to decision domains.
 * This is correct because, for pruning, there is no need to distinguish different levels
 * of violation.
 *
 * @author Michael Marte
 */
final object BooleanDomainPruner extends NumericalDomainPruner[BooleanValue] {

    import BooleanDecisionDomain.createDomain
    import BooleanDomain.ensureDecisionDomain

    type DomainImpl = BooleanDomain

    // iff
    override def eq
        [Domain >: DomainImpl <: yuck.core.Domain[BooleanValue]]
        (lhs0: Domain, rhs0: Domain):
        (Domain, Domain) =
    {
        val lhs1 = ensureDecisionDomain(lhs0)
        val rhs1 = ensureDecisionDomain(rhs0)
        val intersection = lhs1.intersect(rhs1)
        (intersection, intersection)
    }

    // negation
    override def ne
        [Domain >: DomainImpl <: yuck.core.Domain[BooleanValue]]
        (lhs0: Domain, rhs0: Domain):
        (Domain, Domain) =
    {
        val lhs1 = ensureDecisionDomain(lhs0)
        val rhs1 = ensureDecisionDomain(rhs0)
        (if (rhs1.isSingleton) lhs1.diff(rhs1) else lhs1,
         if (lhs1.isSingleton) rhs1.diff(lhs1) else rhs1)
    }

    override def lt
        [Domain >: BooleanDomain <: OrderedDomain[BooleanValue]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
    {
        (FalseDomain.intersect(lhs), TrueDomain.intersect(rhs))
    }

    // implication
    override def le
        [Domain >: BooleanDomain <: OrderedDomain[BooleanValue]]
        (lhs0: Domain, rhs0: Domain):
        (Domain, Domain) =
    {
        val lhs1 = ensureDecisionDomain(lhs0)
        val rhs1 = ensureDecisionDomain(rhs0)
        (if (rhs1.isSingleton && rhs1.singleValue == False) createDomain(lhs1.contains(False), false) else lhs1,
         if (lhs1.isSingleton && lhs1.singleValue == True) createDomain(false, rhs1.contains(True)) else rhs1)
    }

    // conjunction
    override def linEq
        [Domain >: DomainImpl <: NumericalDomain[BooleanValue]]
        (lhs0: Iterable[(BooleanValue, Domain)], rhs0: Domain):
        (Iterator[Domain], Domain) =
    {
        def lhs1 = lhs0.iterator.map(_._2)
        if (rhs0.isEmpty || lhs0.exists{case (_, d) => d.isEmpty}) {
            (for (_ <- lhs0.iterator) yield EmptyBooleanDomain, EmptyBooleanDomain)
        } else if (rhs0 == TrueDomain) {
            val lhs1 = for ((a, d) <- lhs0.iterator) yield if (a == True) d else TrueDomain.intersect(d)
            (lhs1, rhs0)
        } else if (! rhs0.contains(True)) {
            // Necessary: lhs0 contains at least one (a, d) with a != True && d != {True}.
            if (! lhs0.exists{case (a, d) => a != True && d != TrueDomain}) {
                (lhs1, EmptyBooleanDomain)
            } else if (lhs0.count{case (a, d) => a != True} == 1) {
                // When there is exactly one (a, d) in lhs0 with a != True, then d must not contain True.
                val lhs1 =
                    for ((a, d) <- lhs0.iterator) yield
                        if (a == True) d else ensureDecisionDomain(d).diff(TrueDomain)
                (lhs1, rhs0)
            } else {
                (lhs1, rhs0)
            }
        } else if (lhs0.forall{case (a, d) => a == True || d == TrueDomain}) {
            (lhs1, TrueDomain.intersect(rhs0))
        } else if (lhs0.exists{case (a, d) => a != True && ! d.contains(True)}) {
            (lhs1, FalseDomain.intersect(rhs0))
        } else {
            (lhs1, rhs0)
        }
    }

}
