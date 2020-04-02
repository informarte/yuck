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
object BooleanDomainPruner extends OrderedDomainPruner[BooleanValue] {

    import BooleanDecisionDomain.createDomain
    import BooleanDomain.ensureDecisionDomain

    override protected val valueTraits = BooleanValueTraits

    // iff
    override def eqRule
        (lhs0: Domain[BooleanValue], rhs0: Domain[BooleanValue]):
        (BooleanDomain, BooleanDomain) =
    {
        val lhs1 = ensureDecisionDomain(lhs0)
        val rhs1 = ensureDecisionDomain(rhs0)
        val intersection = lhs1.intersect(rhs1)
        (intersection, intersection)
    }

    // negation
    override def neRule
        (lhs0: Domain[BooleanValue], rhs0: Domain[BooleanValue]):
        (BooleanDomain, BooleanDomain) =
    {
        val lhs1 = ensureDecisionDomain(lhs0)
        val rhs1 = ensureDecisionDomain(rhs0)
        (if (rhs1.isSingleton) lhs1.diff(rhs1) else lhs1,
         if (lhs1.isSingleton) rhs1.diff(lhs1) else rhs1)
    }

    override def ltRule
        (lhs: OrderedDomain[BooleanValue], rhs: OrderedDomain[BooleanValue]):
        (BooleanDomain, BooleanDomain) =
    {
        (FalseDomain.intersect(lhs), TrueDomain.intersect(rhs))
    }

    // implication
    override def leRule
        (lhs0: OrderedDomain[BooleanValue], rhs0: OrderedDomain[BooleanValue]):
        (BooleanDomain, BooleanDomain) =
    {
        val lhs1 = ensureDecisionDomain(lhs0)
        val rhs1 = ensureDecisionDomain(rhs0)
        (if (rhs1.isSingleton && rhs1.singleValue == False) createDomain(lhs1.contains(False), false) else lhs1,
         if (lhs1.isSingleton && lhs1.singleValue == True) createDomain(false, rhs1.contains(True)) else rhs1)
    }

    def conjunctionRule
        (lhs0: Iterable[OrderedDomain[BooleanValue]], rhs0: OrderedDomain[BooleanValue]):
        (Iterator[BooleanDomain], BooleanDomain) =
    {
        val lhs1 = lhs0.view.map(ensureDecisionDomain)
        val rhs1 = ensureDecisionDomain(rhs0)
        val (lhs2, rhs2) = conjunctionRule(lhs1, rhs1)
        (lhs2, rhs2)
    }

    private def conjunctionRule
        (lhs0: Iterable[BooleanDecisionDomain], rhs0: BooleanDecisionDomain):
        (Iterator[BooleanDecisionDomain], BooleanDecisionDomain) =
    {
        def lhs1 = lhs0.iterator
        if (rhs0.isEmpty || lhs0.exists(_.isEmpty)) {
            (for (_ <- lhs0.iterator) yield EmptyBooleanDomain, EmptyBooleanDomain)
        } else if (rhs0 == TrueDomain) {
            (for (d <- lhs0.iterator) yield TrueDomain.intersect(d), rhs0)
        } else if (rhs0 == FalseDomain) {
            if (lhs0.forall(_ == TrueDomain)) {
                (lhs1, EmptyBooleanDomain)
            } else if (lhs0.size == 1) {
                (for (d <- lhs0.iterator) yield d.diff(TrueDomain), rhs0)
            } else {
                (lhs1, rhs0)
            }
        } else if (lhs0.forall(_ == TrueDomain)) {
            (lhs1, TrueDomain.intersect(rhs0))
        } else if (lhs0.exists(_ == FalseDomain)) {
            (lhs1, FalseDomain.intersect(rhs0))
        } else {
            (lhs1, rhs0)
        }
    }

    def disjunctionRule
        (lhs0: Iterable[OrderedDomain[BooleanValue]], rhs0: OrderedDomain[BooleanValue]):
        (Iterator[BooleanDomain], BooleanDomain) =
    {
        val lhs1 = lhs0.view.map(ensureDecisionDomain)
        val rhs1 = ensureDecisionDomain(rhs0)
        val (lhs2, rhs2) = disjunctionRule(lhs1, rhs1)
        (lhs2, rhs2)
    }

    private def disjunctionRule
        (lhs0: Iterable[BooleanDecisionDomain], rhs0: BooleanDecisionDomain):
        (Iterator[BooleanDecisionDomain], BooleanDecisionDomain) =
    {
        def lhs1 = lhs0.iterator
        if (rhs0.isEmpty || lhs0.exists(_.isEmpty)) {
            (for (_ <- lhs0.iterator) yield EmptyBooleanDomain, EmptyBooleanDomain)
        } else if (rhs0 == TrueDomain) {
            if (lhs0.forall(_ == FalseDomain)) {
                (lhs1, EmptyBooleanDomain)
            } else if (lhs0.count(_.contains(True)) == 1) {
                (for (d <- lhs0.iterator) yield if (d.contains(True)) TrueDomain else d, rhs0)
            } else {
                (lhs1, rhs0)
            }
        } else if (rhs0 == FalseDomain) {
            (for (d <- lhs0.iterator) yield FalseDomain.intersect(d), rhs0)
        } else if (lhs0.exists(_ == TrueDomain)) {
            (lhs1, TrueDomain.intersect(rhs0))
        } else if (lhs0.forall(_ == FalseDomain)) {
            (lhs1, FalseDomain.intersect(rhs0))
        } else {
            (lhs1, rhs0)
        }
    }

}
