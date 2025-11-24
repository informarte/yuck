package yuck.core

/**
 * Provides methods for pruning Boolean domains.
 *
 * To simplify the implementation, channel domains are sometimes translated to decision domains.
 * This is correct because, for pruning, there is no need to distinguish different levels
 * of violation.
 */
object BooleanDomainPruner extends OrderedDomainPruner[BooleanValue] {

    override protected val valueTraits = BooleanValueTraits

    // iff
    override def eqRule
        (lhs0: Domain[BooleanValue], rhs0: Domain[BooleanValue]):
        (BooleanDomain, BooleanDomain) =
    {
        val lhs1 = lhs0.asInstanceOf[BooleanDomain]
        val rhs1 = rhs0.asInstanceOf[BooleanDomain]
        val intersection = lhs1.intersect(rhs1)
        (intersection, intersection)
    }

    // negation
    override def neRule
        (lhs0: Domain[BooleanValue], rhs0: Domain[BooleanValue]):
        (BooleanDomain, BooleanDomain) =
    {
        val lhs1 = lhs0.asInstanceOf[BooleanDomain]
        val rhs1 = rhs0.asInstanceOf[BooleanDomain]
        (if rhs1.isSingleton then lhs1.diff(rhs1) else lhs1,
         if lhs1.isSingleton then rhs1.diff(lhs1) else rhs1)
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
        val lhs1 = lhs0.asInstanceOf[BooleanDomain]
        val rhs1 = rhs0.asInstanceOf[BooleanDomain]
        (if rhs1.isSingleton && rhs1.singleValue == False then BooleanDomain(lhs1.contains(False), false) else lhs1,
         if lhs1.isSingleton && lhs1.singleValue == True then BooleanDomain(false, rhs1.contains(True)) else rhs1)
    }

    def conjunctionRule
        (lhs0: Iterable[OrderedDomain[BooleanValue]], rhs0: OrderedDomain[BooleanValue]):
        (Iterator[BooleanDomain], BooleanDomain) =
    {
        val lhs1 = lhs0.view.map(_.asInstanceOf[BooleanDomain])
        val rhs1 = rhs0.asInstanceOf[BooleanDomain]
        val (lhs2, rhs2) = conjunctionRule(lhs1, rhs1)
        (lhs2, rhs2)
    }

    private def conjunctionRule
        (lhs0: Iterable[BooleanDomain], rhs0: BooleanDomain):
        (Iterator[BooleanDomain], BooleanDomain) =
    {
        def lhs1 = lhs0.iterator
        if rhs0.isEmpty || lhs0.exists(_.isEmpty) then {
            (for _ <- lhs0.iterator yield EmptyBooleanDomain, EmptyBooleanDomain)
        } else if rhs0 == TrueDomain then {
            (for d <- lhs0.iterator yield TrueDomain.intersect(d), rhs0)
        } else if rhs0 == FalseDomain then {
            if lhs0.forall(_ == TrueDomain) then {
                (lhs1, EmptyBooleanDomain)
            } else if lhs0.count(_ == FalseDomain) == 0 && lhs0.count(_ == CompleteBooleanDomain) == 1 then {
                (for d <- lhs0.iterator yield if d == CompleteBooleanDomain then d.diff(TrueDomain) else d, rhs0)
            } else {
                (lhs1, rhs0)
            }
        } else if lhs0.forall(_ == TrueDomain) then {
            (lhs1, TrueDomain.intersect(rhs0))
        } else if lhs0.exists(_ == FalseDomain) then {
            (lhs1, FalseDomain.intersect(rhs0))
        } else {
            (lhs1, rhs0)
        }
    }

    def disjunctionRule
        (lhs0: Iterable[OrderedDomain[BooleanValue]], rhs0: OrderedDomain[BooleanValue]):
        (Iterator[BooleanDomain], BooleanDomain) =
    {
        val lhs1 = lhs0.view.map(_.asInstanceOf[BooleanDomain])
        val rhs1 = rhs0.asInstanceOf[BooleanDomain]
        val (lhs2, rhs2) = disjunctionRule(lhs1, rhs1)
        (lhs2, rhs2)
    }

    private def disjunctionRule
        (lhs0: Iterable[BooleanDomain], rhs0: BooleanDomain):
        (Iterator[BooleanDomain], BooleanDomain) =
    {
        def lhs1 = lhs0.iterator
        if rhs0.isEmpty || lhs0.exists(_.isEmpty) then {
            (for _ <- lhs0.iterator yield EmptyBooleanDomain, EmptyBooleanDomain)
        } else if rhs0 == TrueDomain then {
            if lhs0.forall(_ == FalseDomain) then {
                (lhs1, EmptyBooleanDomain)
            } else if lhs0.count(_.contains(True)) == 1 then {
                (for d <- lhs0.iterator yield if d.contains(True) then TrueDomain else d, rhs0)
            } else {
                (lhs1, rhs0)
            }
        } else if rhs0 == FalseDomain then {
            (for d <- lhs0.iterator yield FalseDomain.intersect(d), rhs0)
        } else if lhs0.exists(_ == TrueDomain) then {
            (lhs1, TrueDomain.intersect(rhs0))
        } else if lhs0.forall(_ == FalseDomain) then {
            (lhs1, FalseDomain.intersect(rhs0))
        } else {
            (lhs1, rhs0)
        }
    }

}
