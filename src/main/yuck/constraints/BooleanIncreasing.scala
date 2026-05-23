package yuck.constraints

import scala.collection.*

import yuck.constraints.Increasing.deduplicated
import yuck.core.*

/**
 * Implements Boolean ''increasing'' constraints as specified by MiniZinc.
 */
final class BooleanIncreasing
    (id: Id[Constraint],
     override val xs: immutable.IndexedSeq[BooleanVariable],
     override val costs: BooleanVariable)
    extends Increasing(id)(using BooleanTypeTraits)
{

    override val strict = false

    override def toString = "increasing([%s], %s)".format(xs.mkString(", "), costs)
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new BooleanIncreasing(id, xs, replacements.getOrElse(costs, costs).asInstanceOf[BooleanVariable])

    override protected def maybeSmallestFeasibleValue(x: BooleanVariable, maybePreviousValue: Option[BooleanValue]) = {
        if maybePreviousValue.isDefined then {
            val a = maybePreviousValue.get
            val d = x.domain.diff(if a == True then FalseDomain else EmptyBooleanDomain)
            if d.isEmpty then None else Some(d.ub)
        } else {
            Some(x.domain.ub)
        }
    }

    override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        createHotSpotDistribution: IndexedSeq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability]) =
    {
        if solve(space) then {
            val xs1 = deduplicated(xs).toVector
            Some(new BooleanIncreasingNeighbourhood(space, xs1, randomGenerator, moveSizeDistribution))
        } else {
            None
        }
    }

}
