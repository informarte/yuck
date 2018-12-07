package yuck.constraints

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
final class Element
    [Value <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     xs: IndexedSeq[Variable[Value]], i: Variable[IntegerValue], y: Variable[Value], indexBase: Int)
    (implicit valueTraits: AnyValueTraits[Value])
    extends Constraint(id, goal)
{

    require(indexBase >= 0)

    override def toString = "%s = element(%s, [%s])".format(y, i, xs.mkString(", "))
    override def inVariables = xs.toIterator ++ List(i).toIterator
    override def outVariables = List(y)

    private val effects = List(new ReusableEffectWithFixedVariable[Value](y))
    private val effect = effects.head

    private def computeEffect(searchState: SearchState) {
        // When i is a channel variable, the value of i may be out-of-bounds!
        // Nevertheless, we have to provide some value for y.
        val j = scala.math.min(scala.math.max(0, searchState.value(i).value - indexBase), xs.size - 1)
        effect.a = searchState.value(xs(j))
    }

    override def propagate = {
        if (valueTraits == IntegerSetValueTraits) {
            // Integer-set domains do not support the union operation, so the below code does not work.
            false
        } else {
            val di1 =
                i.domain.intersect(
                    IntegerDomain.createRange(IntegerValue.get(indexBase), IntegerValue.get(safeAdd(xs.size, indexBase) - 1)))
            val dy1 =
                y.domain.intersect(
                    di1.values.foldLeft(valueTraits.emptyDomain){case (u, i) => u.union(xs(i.value - indexBase).domain)})
            val di2 =
                IntegerDomain.createDomain(
                    di1.values.toIterator.filter(i => xs(i.value - indexBase).domain.intersects(dy1)).toSet)
            Variable.pruneDomains(i, di2, y, dy1)
        }
    }

    override def initialize(now: SearchState) = {
        computeEffect(now)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        computeEffect(after)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) =
        effects

}
