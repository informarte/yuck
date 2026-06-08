package yuck.flatzinc.compiler

import scala.collection.{Set, mutable}

import yuck.constraints.{OptimizationGoalTracker, SatisfactionGoalTracker}
import yuck.core.{AnyVariable, Constraint}

/**
 * Retracts useless constraints from the constraint network.
 *
 * The implementation assumes that objective variables have already been registered.
 */
final class ConstraintNetworkPruner
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    private def findObjectiveVariables: Set[AnyVariable] =
        cc.space.channelVariables.filter(cc.space.isObjectiveVariable)

    private def isUseless(isImportant: AnyVariable => Boolean, constraint: Constraint): Boolean = {
        constraint match {
            case _: LevelWeightMaintainer => false
            case _: OptimizationGoalTracker[?, ?, ?] => false
            case _: SatisfactionGoalTracker => false
            case _ =>
                ! cc.space.isImplicitConstraint(constraint) &&
                constraint.outVariables.forall(x =>
                    ! isImportant(x) &&
                    cc.space.directlyAffectedConstraints(x).isEmpty)
        }
    }

    override def run() = {
        val importantVars = new mutable.HashSet[AnyVariable]
        importantVars
            .addAll(cc.costVars)
            .addAll(cc.outputVars.values)
            .addAll(cc.outputArrays.values.flatten)
            .addAll(findObjectiveVariables)
        cc.danglingVars.addAll(
            cc.space
                .retractUselessConstraints(isUseless(importantVars.contains, _))
                .iterator
                .flatMap(_.inVariables)
                .filter(cc.space.isDanglingVariable))
    }

}
