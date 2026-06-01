package yuck.constraints

import scala.collection.*

import yuck.constraints.Increasing.deduplicated
import yuck.core.*
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * Implements integer ''increasing'' and ''strictly_increasing'' constraints as specified by MiniZinc.
 */
final class IntegerIncreasing
    (id: Id[Constraint],
     override val xs: immutable.IndexedSeq[IntegerVariable],
     override val strict: Boolean,
     override val costs: BooleanVariable)
    extends Increasing(id)(using IntegerTypeTraits)
{

    override def toString = "increasing([%s], %s, %s)".format(xs.mkString(", "), strict, costs)
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new IntegerIncreasing(id, xs, strict, replacements.getOrElse(costs, costs).asInstanceOf[BooleanVariable])

    override protected def maybeSmallestFeasibleValue(x: IntegerVariable, maybePreviousValue: Option[IntegerValue]) = {
        if maybePreviousValue.isDefined then {
            val a = maybePreviousValue.get
            val d = x.domain.boundFromBelow(if strict then a + One else a)
            if d.isEmpty then None else Some(d.lb)
        } else {
            Some(x.domain.lb)
        }
    }

    override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        logger: LazyLogger,
        sigint: Sigint,
        moveSizeDistribution: Distribution,
        createHotSpotDistribution: IndexedSeq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability]) =
    {
        if solve(space) then {
            val xs1 = deduplicated(xs).toVector
            Some(new IntegerIncreasingNeighbourhood(
                space, xs1, strict, randomGenerator,
                moveSizeDistribution, createHotSpotDistribution(xs1), maybeFairVariableChoiceRate))
        } else {
            None
        }
    }

}
