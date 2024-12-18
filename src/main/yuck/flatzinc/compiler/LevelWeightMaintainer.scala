package yuck.flatzinc.compiler

import scala.collection.*

import yuck.core.*

/**
 * Maintains a weight for each level of a hierarchical objective.
 *
 * If the top-level objective is satisfied, then its weight is set to zero and all other weights are set to one,
 * and vice versa.
 *
 * @author Michael Marte
 */
final class LevelWeightMaintainer
    (id: Id[yuck.core.Constraint],
     objectives: immutable.IndexedSeq[PrimitiveObjective],
     distribution: Distribution)
    extends Constraint(id)
{

    require(objectives.size == distribution.size)

    override def toString = "level_weight_maintainer([%s])".format(objectives.mkString(", "))

    override def inVariables = objectives.map(_.x)
    override def outVariables = Nil

    override def initialize(now: SearchState) = {
        val solved = objectives(0).isSolution(now)
        for (i <- objectives.indices) {
            distribution.setFrequency(i, if (i == 0) (if (solved) 0 else 1) else (if (solved) 1 else 0))
        }
        Nil
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)

}
