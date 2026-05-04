package yuck.constraints

import scala.annotation.tailrec
import scala.collection.*

import yuck.core.*

/**
 * Used to implement the family of fzn_if_then_else_* constraints.
 *
 * Ignores the value of the last element of cs and assumes it to be true.
 */
final class IfThenElse
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Constraint], cs: immutable.IndexedSeq[BooleanVariable], xs: immutable.IndexedSeq[X], y: X)
    (using typeTraits: TypeTraits[A, D, X])
    extends Constraint(id)
{

    require(cs.size == xs.size)
    require(cs.size >= 2)

    private val n = cs.size

    override def toString = "if_then_else([%s], [%s], %s)".format(cs.mkString(", "), xs.mkString(", "), y)

    override def inVariables = cs.view ++ xs
    override def outVariables = List(y)

    private val effect = new ReusableMoveEffectWithFixedVariable(y)

    // propagate from the x[i] and y to the c[i]
    private def propagate1(effects: PropagationEffects): PropagationEffects = {
        // identify impossible cases
        def identifyImpossibleCase(i: Int): BooleanDomain =
            if xs(i).domain.intersects(y.domain) then cs(i).domain else cs(i).domain.intersect(FalseDomain)
        val cds = (0 until n).iterator.map(identifyImpossibleCase).toBuffer
        // make sure we have a default case
        @tailrec
        def findDefaultCase(i: Int): Int =
            if i < 0
            then i
            else {
                cds.update(i, cds(i).intersect(TrueDomain))
                if cds(i).isSingleton then i else findDefaultCase(i - 1)
            }
        findDefaultCase(n - 1)
        // prune the domains of the c[i] up to the first c[j] which could become true
        @tailrec
        def findFeasibleCase(i: Int): Int =
            if i == n
            then i
            else {
                effects.pruneDomain(cs(i), cds(i))
                if cs(i).domain.contains(True) then i else findFeasibleCase(i + 1)
            }
        findFeasibleCase(0)
        effects
    }

    // propagate from the c[i] to the x[i] and y
    @tailrec
    private def propagate2(effects: PropagationEffects, i: Int): PropagationEffects = {
        if i == n then {
            effects
        } else if cs(i).domain.isSingleton then {
            if cs(i).domain.singleValue.truthValue then {
                // y = xs(i)
                effects.pruneDomains(xs(i), y.domain, y, xs(i).domain)
            } else {
                // skip impossible case
                propagate2(effects, i + 1)
            }
        } else if typeTraits.domainCapabilities.union then {
            // constructive disjunction: propagate the union of the x[j] domains, j > i, to y
            effects.pruneDomain(
                y,
                (i until n)
                    .iterator
                    .filter(i => cs(i).domain != FalseDomain)
                    .foldLeft(typeTraits.emptyDomain)((u, i) => u.union(xs(i).domain)))
        } else {
            effects
        }
    }

    override def propagate() = {
        propagate2(propagate1(NoPropagationOccurred), 0)
    }

    override def initialize(now: SearchState) = {
        var i = 0
        while i < n && ! (i == n - 1 || now.value(cs(i)).truthValue) do {
            i += 1
        }
        effect.a = now.value(xs(i))
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)

    override def commit(before: SearchState, after: SearchState, move: Move) =
        effect

}
