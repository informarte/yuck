package yuck.core

/**
 * Provides methods for pruning Boolean domains.
 *
 * To simplify the implementation, channel domains are sometimes translated to decision domains.
 * This is correct because, for pruning, there is no need to distinguish different levels
 * of violation.
 */
object BooleanDomainPruner extends OrderedDomainPruner[BooleanValue, BooleanDomain] {

    override protected val typeTraits = BooleanTypeTraits

    // iff
    override def eqRule
        (lhs: BooleanDomain, rhs: BooleanDomain):
        (BooleanDomain, BooleanDomain) =
    {
        val intersection = lhs.intersect(rhs)
        (intersection, intersection)
    }

    // negation
    override def neRule
        (lhs: BooleanDomain, rhs: BooleanDomain):
        (BooleanDomain, BooleanDomain) =
    {
        (if rhs.isSingleton then lhs.diff(rhs) else lhs,
         if lhs.isSingleton then rhs.diff(lhs) else rhs)
    }

    override def ltRule
        (lhs: BooleanDomain, rhs: BooleanDomain):
        (BooleanDomain, BooleanDomain) =
    {
        (FalseDomain.intersect(lhs), TrueDomain.intersect(rhs))
    }

    // implication
    override def leRule
        (lhs: BooleanDomain, rhs: BooleanDomain):
        (BooleanDomain, BooleanDomain) =
    {
        (if rhs.isSingleton && rhs.singleValue == False then BooleanDomain(lhs.contains(False), false) else lhs,
         if lhs.isSingleton && lhs.singleValue == True then BooleanDomain(false, rhs.contains(True)) else rhs)
    }

    def conjunctionRule
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
