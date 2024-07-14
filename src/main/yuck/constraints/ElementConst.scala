package yuck.constraints

import scala.collection.*

import yuck.core.{given, *}

/**
 * Used to implement the family of array_*_element constraints as required by FlatZinc.
 *
 * When the index is out-of-bounds, some value from the array is returned.
 * (When such a case may happen, an additional constraint is required that forces the index variable
 * to take a valid value.)
 *
 * @author Michael Marte
 *
 */
final class ElementConst
    [V <: Value[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     as: immutable.IndexedSeq[V], i: IntegerVariable, y: Variable[V], offset: Int)
    (using valueTraits: ValueTraits[V])
    extends Constraint(id)
{

    require(! as.isEmpty)

    override def toString = "%s = element(%s, [%s], %d)".format(y, i, as.mkString(", "), offset)

    override def inVariables = List(i)
    override def outVariables = List(y)

    private val effect = y.reuseableEffect

    private def computeEffect(searchState: SearchState): Unit = {
        // When i is a channel variable, the value of i may be out-of-bounds!
        // Nevertheless, we have to provide some value for y.
        val j = min(max(0, safeSub(searchState.value(i).toInt, offset)), as.size - 1)
        effect.a = as(j)
    }

    override def propagate() = {
        if (valueTraits.domainCapabilities.createDomain && valueTraits.domainCapabilities.union) {
            val di1 =
                i.domain.intersect(IntegerRange(offset, safeDec(safeAdd(as.size, offset))))
            val dy1 =
                y.domain.intersect(
                    di1.valuesIterator
                        .foldLeft(valueTraits.emptyDomain)((u, i) => u.union(valueTraits.createDomain(Set(as(i.toInt - offset))))))
            val di2 =
                IntegerDomain(
                    di1.valuesIterator.filter(i => dy1.contains(as(i.toInt - offset))).toSet)
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
