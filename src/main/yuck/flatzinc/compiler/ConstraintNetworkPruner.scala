package yuck.flatzinc.compiler

import scala.collection.mutable
import scala.collection.Set

import yuck.constraints.{OptimizationGoalTracker, SatisfactionGoalTracker}
import yuck.core.{AnyVariable, Constraint}
import yuck.flatzinc.ast.{Annotation, Term}

/**
 * Retracts useless constraints from the constraint network.
 *
 * The implementation assumes that objective variables have already been registered.
 *
 * @author Michael Marte
 */
final class ConstraintNetworkPruner
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    private def findOutputVariables: Set[AnyVariable] = {
        val result = new mutable.HashSet[AnyVariable]
        for (decl <- cc.ast.varDecls) {
            for (annotation <- decl.annotations) {
                annotation match {
                    case Annotation(Term("output_var", _)) =>
                        result.add(compileAnyExpr(Term(decl.id, Nil)))
                    case Annotation(Term("output_array", _)) =>
                        for (x <- compileAnyArray(Term(decl.id, Nil))) {
                            result.add(x)
                        }
                    case _ =>
                }
            }
        }
        result
    }

    private def findObjectiveVariables: Set[AnyVariable] =
        cc.space.channelVariables.filter(cc.space.isObjectiveVariable)

    private def isUseless(isImportant: AnyVariable => Boolean, constraint: Constraint): Boolean = {
        constraint match {
            case _: LevelWeightMaintainer => false
            case _: OptimizationGoalTracker[_] => false
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
            .addAll(findOutputVariables)
            .addAll(findObjectiveVariables)
        cc.space.retractUselessConstraints(isUseless(importantVars.contains, _))
    }

}
