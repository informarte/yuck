package yuck.constraints

import scala.collection.*

import yuck.constraints.Increasing.deduplicated
import yuck.core.*

/**
 * Implements Boolean ''increasing'' constraints as specified by MiniZinc..
 *
 * @author Michael Marte
 */
final class BooleanIncreasing
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     override protected val xs: immutable.IndexedSeq[BooleanVariable],
     override protected val costs: BooleanVariable)
    extends Increasing[BooleanValue, BooleanVariable](id) {

    override protected val strict = false

    override def toString = "increasing([%s], %s)".format(xs.mkString(", "), costs)

    override protected def maybeSmallestFeasibleValue(x: BooleanVariable, maybePreviousValue: Option[BooleanValue]) = {
        if (maybePreviousValue.isDefined) {
            val a = maybePreviousValue.get
            val d = x.domain.diff(if a == True then FalseDomain else EmptyBooleanDomain)
            if (d.isEmpty) None else Some(d.ub)
        } else {
            Some(x.domain.ub)
        }
    }

    override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        createHotSpotDistribution: Seq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability]) =
    {
        if (solve(space)) {
            val xs1 = deduplicated(xs).toVector
            Some(new BooleanIncreasingNeighbourhood(space, xs1, randomGenerator, moveSizeDistribution))
        } else {
            None
        }
    }

}
