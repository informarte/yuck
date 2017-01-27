package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.{NumLe, Sum}
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
    private val domains = cc.domains
    private val logger = cc.logger
    private val space = cc.space
    private val implicitlyConstrainedVars = cc.implicitlyConstrainedVars
    private val costVars = cc.costVars

    override def run {
        val objectives = new mutable.ArrayBuffer[AnyObjective]
        cc.ast.solveGoal match {
            case Satisfy(_) =>
            case Minimize(a, _) =>
                val x = compileExpr[IntegerValue](a)
                val dx = domains(a).asInstanceOf[IntegerDomain]
                val lb =
                    cfg.maybeOptimum
                    .orElse(dx.maybeLb.map(_.value))
                    .getOrElse(Int.MinValue)
                    .+(cfg.maybeQualityTolerance.getOrElse(0))
                logger.log("Target objective value for minimization: %s".format(lb))
                val maybeObjectiveVar =
                    if (space.isDanglingVariable(x)) {
                        logger.log("Objective variable %s is dangling, assigning %s to it".format(x, lb))
                        space.setValue(x, IntegerValue.get(lb))
                        None
                    }
                    else if (
                        cfg.useProgressiveTightening &&
                        space.isChannelVariable(x) &&
                        dx.maybeUb.isDefined)
                    {
                        logger.log("Objective variable %s is a channel variable with upper bound, setting up for progressive tightening".format(x))
                        val y = space.createVariable("_YUCK_UB", dx)
                        space.setValue(y, dx.ub)
                        implicitlyConstrainedVars += y
                        val z = createNonNegativeChannel[IntegerValue]
                        costVars += z
                        space.post(new NumLe(nextConstraintId, null, x, y, z))
                        Some(y)
                    } else {
                        Some(x)
                    }
                if (maybeObjectiveVar.isDefined) {
                    objectives += new MinimizationObjective[IntegerValue](maybeObjectiveVar.get, IntegerValue.get(lb), Some(MinusOne))
                }
            case Maximize(a, _) =>
                val x = compileExpr[IntegerValue](a)
                val dx = domains(a).asInstanceOf[IntegerDomain]
                val ub =
                    cfg.maybeOptimum
                    .orElse(dx.maybeUb.map(_.value))
                    .getOrElse(Int.MaxValue)
                    .-(cfg.maybeQualityTolerance.getOrElse(0))
                logger.log("Target objective value for maximization: %s".format(ub))
                val maybeObjectiveVar =
                    if (space.isDanglingVariable(x)) {
                        logger.log("Objective variable %s is dangling, assigning %s to it".format(x, ub))
                        space.setValue(x, IntegerValue.get(ub))
                        None
                    }
                    else if (
                        cfg.useProgressiveTightening &&
                        space.isChannelVariable(x) &&
                        dx.maybeLb.isDefined)
                    {
                        logger.log("Objective variable %s is a channel variable with lower bound, setting up for progressive tightening".format(x))
                        val y = space.createVariable("_YUCK_LB", dx)
                        space.setValue(y, dx.lb)
                        implicitlyConstrainedVars += y
                        val z = createNonNegativeChannel[IntegerValue]
                        costVars += z
                        space.post(new NumLe(nextConstraintId, null, y, x, z))
                        Some(y)
                    } else {
                        Some(x)
                    }
                if (maybeObjectiveVar.isDefined) {
                    objectives += new MaximizationObjective[IntegerValue](maybeObjectiveVar.get, IntegerValue.get(ub), Some(One))
                }
        }
        cc.costVar = createNonNegativeChannel[IntegerValue]
        space.post(new Sum(nextConstraintId, null, costVars.toIndexedSeq, cc.costVar))
        objectives += new MinimizationObjective(cc.costVar, Zero, None)
        cc.objective =
            if (objectives.size == 1) objectives.head
            else new HierarchicalObjective(objectives.toList.reverse, cfg.stopOnFirstSolution)
    }

}
