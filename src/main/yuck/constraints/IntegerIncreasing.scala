package yuck.constraints

import scala.collection.*

import yuck.constraints.Increasing.deduplicated
import yuck.core.*

/**
 * Implements integer ''increasing'' and ''strictly_increasing'' constraints as specified by MiniZinc.
 *
 * @author Michael Marte
 */
final class IntegerIncreasing
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     override protected val xs: immutable.IndexedSeq[IntegerVariable],
     override protected val strict: Boolean,
     override protected val costs: BooleanVariable)
    extends Increasing[IntegerValue, IntegerVariable](id) {

    override def toString = "increasing([%s], %s, %s)".format(xs.mkString(", "), strict, costs)

    override protected def maybeSmallestFeasibleValue(x: IntegerVariable, maybePreviousValue: Option[IntegerValue]) = {
        if (maybePreviousValue.isDefined) {
            val a = maybePreviousValue.get
            val d = x.domain.boundFromBelow(if strict then a + One else a)
            if (d.isEmpty) None else Some(d.lb)
        } else {
            Some(x.domain.lb)
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
            Some(new IntegerIncreasingNeighbourhood(
                space, xs1, strict, randomGenerator,
                moveSizeDistribution, createHotSpotDistribution(xs1), maybeFairVariableChoiceRate))
        } else {
            None
        }
    }

}
