package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.{Le, Sum}
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
    private val logger = cc.logger
    private val space = cc.space
    private val implicitlyConstrainedVars = cc.implicitlyConstrainedVars
    private val costVars = cc.costVars

    import HighPriorityImplicits._

    override def run = {
        val objectives = new mutable.ArrayBuffer[AnyObjective]
        cc.ast.solveGoal match {
            case Satisfy(_) =>
            case Minimize(a, _) =>
                val x = compileIntExpr(a)
                val dx = x.domain
                val maybeObjectiveVar =
                    if (space.isDanglingVariable(x)) {
                        val lb =
                            dx.maybeLb.getOrElse(
                                cfg.maybeTargetObjectiveValue.map(IntegerValue.get).getOrElse(
                                    IntegerValueTraits.minValue))
                        logger.log("Objective variable %s is dangling, assigning %s to it".format(x, lb))
                        space.setValue(x, lb)
                        None
                    }
                    else if (
                        cfg.useProgressiveTightening &&
                        space.isChannelVariable(x) &&
                        dx.maybeUb.isDefined)
                    {
                        logger.log("Objective variable %s is a channel variable with upper bound, setting up for progressive tightening".format(x))
                        val y = new IntegerVariable(space.nextVariableId, "_YUCK_UB", dx)
                        space.setValue(y, dx.ub)
                        implicitlyConstrainedVars += y
                        val z = createBoolChannel
                        costVars += z
                        space.post(new Le(nextConstraintId, null, x, y, z))
                        Some(y)
                    } else {
                        Some(x)
                    }
                if (maybeObjectiveVar.isDefined) {
                    objectives +=
                        new MinimizationObjective[IntegerValue](
                            maybeObjectiveVar.get, cfg.maybeTargetObjectiveValue.map(IntegerValue.get), Some(MinusOne))
                    cc.objectiveVar = maybeObjectiveVar.get
                }
            case Maximize(a, _) =>
                val x = compileIntExpr(a)
                val dx = x.domain
                val maybeObjectiveVar =
                    if (space.isDanglingVariable(x)) {
                        val ub =
                            dx.maybeUb.getOrElse(
                                cfg.maybeTargetObjectiveValue.map(IntegerValue.get).getOrElse(
                                    IntegerValueTraits.maxValue))
                        logger.log("Objective variable %s is dangling, assigning %s to it".format(x, ub))
                        space.setValue(x, ub)
                        None
                    }
                    else if (
                        cfg.useProgressiveTightening &&
                        space.isChannelVariable(x) &&
                        dx.maybeLb.isDefined)
                    {
                        logger.log("Objective variable %s is a channel variable with lower bound, setting up for progressive tightening".format(x))
                        val y = new IntegerVariable(space.nextVariableId, "_YUCK_LB", dx)
                        space.setValue(y, dx.lb)
                        implicitlyConstrainedVars += y
                        val z = createBoolChannel
                        costVars += z
                        space.post(new Le(nextConstraintId, null, y, x, z))
                        Some(y)
                    } else {
                        Some(x)
                    }
                if (maybeObjectiveVar.isDefined) {
                    objectives +=
                        new MaximizationObjective[IntegerValue](
                            maybeObjectiveVar.get, cfg.maybeTargetObjectiveValue.map(IntegerValue.get), Some(One))
                    cc.objectiveVar = maybeObjectiveVar.get
                }
        }
        cc.costVar = createBoolChannel
        space.post(new Sum(nextConstraintId, null, costVars.toIndexedSeq, cc.costVar))
        objectives += new MinimizationObjective(cc.costVar, Some(True), None)
        cc.objective =
            if (objectives.size == 1) objectives.head
            else new HierarchicalObjective(objectives.toList.reverse, cfg.stopOnFirstSolution)
    }

}
