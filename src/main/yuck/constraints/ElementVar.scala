package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Used to implement the family of array_*_element constraints as required by FlatZinc.
 *
 * When the index is out-of-bounds, some value from the array is returned.
 * (When such a case may happen, an additional constraint is required that forces the index variable
 * to take a valid value.)
 */
final class ElementVar
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Constraint], val xs: immutable.IndexedSeq[X], val i: IntegerVariable, val y: X, val offset: Int)
    (using typeTraits: TypeTraits[A, D, X])
    extends Constraint(id)
{

    require(! xs.isEmpty)

    override def toString = "%s = element([%s], %s, %d)".format(y, xs.mkString(", "), i, offset)

    override def inVariables = xs.view :+ i
    override def outVariables = List(y)

    private val effect = new ReusableMoveEffectWithFixedVariable(y)

    inline private def computeEffect(searchState: SearchState): Unit = {
        // When i is a channel variable, the value of i may be out-of-bounds!
        // Nevertheless, we have to provide some value for y.
        val j = min(max(0, safeSub(searchState.value(i).toInt, offset)), xs.size - 1)
        effect.a = searchState.value(xs(j))
    }

    override def propagate() = {
        if typeTraits.domainCapabilities.union then {
            val di1 =
                i.domain.intersect(IntegerRange(offset, safeDec(safeAdd(xs.size, offset))))
            val dy1 =
                y.domain.intersect(
                    di1.valuesIterator.foldLeft(typeTraits.emptyDomain)((u, i) => u.union(xs(i.toInt - offset).domain)))
            val di2 =
                IntegerDomain(
                    di1.valuesIterator.filter(i => xs(i.toInt - offset).domain.intersects(dy1)).toSet)
            NoPropagationOccurred.pruneDomains(i, di2, y, dy1)
        } else {
            NoPropagationOccurred
        }
    }

    override def initialize(now: SearchState) = {
        computeEffect(now)
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        computeEffect(after)
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) =
        effect

}
