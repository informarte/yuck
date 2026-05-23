package yuck.constraints

import scala.collection.*

import yuck.core.*

final class CountVar
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Constraint],
     override val xs: immutable.Seq[X],
     val y: X,
     override val result: IntegerVariable)
    (using override protected val typeTraits: TypeTraits[A, D, X])
    extends ValueFrequencyTracker[A, D, X, IntegerValue, IntegerDomain, IntegerVariable](id)
{
    override def toString = "%s = count(%s, [%s])".format(result, y, xs.mkString(", "))
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new CountVar(id, xs, y, replacements.getOrElse(result, result).asInstanceOf[IntegerVariable])
    override def inVariables = xs.view :+ y
    override def todo(move: Move) = super.todo(move).filter(x => x != y)
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue(valueRegistry.getOrElse(typeTraits.normalizedValue(searchState.value(y)), 0))

}
