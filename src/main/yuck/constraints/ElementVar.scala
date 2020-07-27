package yuck.constraints

import scala.collection._

import yuck.core._

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
final class ElementVar
    [Value <: AnyValue]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.IndexedSeq[Variable[Value]], i: IntegerVariable, y: Variable[Value], offset: Int)
    (implicit valueTraits: ValueTraits[Value])
    extends Constraint(id)
{

    override def toString = "%s = element(%s, [%s], %d)".format(y, i, xs.mkString(", "), offset)

    override def inVariables = xs.view :+ i
    override def outVariables = List(y)

    private val effect = new ReusableMoveEffectWithFixedVariable(y)

    private def computeEffect(searchState: SearchState): Unit = {
        // When i is a channel variable, the value of i may be out-of-bounds!
        // Nevertheless, we have to provide some value for y.
        val j = min(max(0, safeSub(searchState.value(i).value, offset)), xs.size - 1)
        effect.a = searchState.value(xs(j))
    }

    override def propagate = {
        if (valueTraits == IntegerSetValueTraits) {
            // Integer-set domains do not support the union operation, so the below code does not work.
            NoPropagationOccurred
        } else {
            val di1 =
                i.domain.intersect(
                    IntegerDomain.createRange(IntegerValue.get(offset), IntegerValue.get(safeDec(safeAdd(xs.size, offset)))))
            val dy1 =
                y.domain.intersect(
                    di1.valuesIterator.foldLeft(valueTraits.emptyDomain){case (u, i) => u.union(xs(i.value - offset).domain)})
            val di2 =
                IntegerDomain.createDomain(
                    di1.valuesIterator.filter(i => xs(i.value - offset).domain.intersects(dy1)).toSet)
            NoPropagationOccurred.pruneDomains(i, di2, y, dy1)
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
