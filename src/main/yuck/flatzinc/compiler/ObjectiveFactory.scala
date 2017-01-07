package yuck.flatzinc.compiler

import yuck.constraints.Sum
import yuck.core._
import yuck.flatzinc.ast._

/**
 * Creates an objective from the FlatZinc solve goal.
 *
 * @author Michael Marte
 */
final class ObjectiveFactory
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    private val cfg = cc.cfg

    override def run {
        cc.costVar = createNonNegativeChannel[IntegerValue]
        cc.space.post(new Sum(nextConstraintId, null, cc.costVars.toIndexedSeq, cc.costVar))
        cc.objective = new MinimizationObjective(cc.costVar, Zero)
        cc.ast.solveGoal match {
            case Satisfy(_) =>
            case Minimize(a, _) =>
                val lb =
                    cfg.maybeOptimum
                    .orElse(cc.domains(a).asInstanceOf[IntegerDomain].maybeLb.map(_.value))
                    .getOrElse(Int.MinValue)
                    .+(cfg.maybeQualityTolerance.getOrElse(0))
                cc.logger.log("Target objective value for minimization: %s".format(lb))
                cc.objective =
                    new HierarchicalObjective(
                        List(cc.objective, new MinimizationObjective[IntegerValue](a, IntegerValue.get(lb))),
                        cfg.stopOnFirstSolution)
            case Maximize(a, _) =>
                val ub =
                    cfg.maybeOptimum
                    .orElse(cc.domains(a).asInstanceOf[IntegerDomain].maybeUb.map(_.value))
                    .getOrElse(Int.MaxValue)
                    .-(cfg.maybeQualityTolerance.getOrElse(0))
                cc.logger.log("Target objective value for maximization: %s".format(ub))
                cc.objective =
                    new HierarchicalObjective(
                        List(cc.objective, new MaximizationObjective[IntegerValue](a, IntegerValue.get(ub))),
                        cfg.stopOnFirstSolution)
        }
    }

}
