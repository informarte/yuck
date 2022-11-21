package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.{Conjunction, Lt}
import yuck.core.{given, *}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.*

/**
 * Creates an objective from the FlatZinc solve goal.
 *
 * @author Michael Marte
 */
final class ObjectiveFactory
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    import HighPriorityImplicits.*

    override def run() = {
        val objectives = new mutable.ArrayBuffer[PrimitiveObjective]
        cc.ast.solveGoal match {
            case Satisfy(_) =>
                val maybeGoalHierarchy = cc.ast.solveGoal.annotations.find(_.term.id == "goal_hierarchy")
                if (maybeGoalHierarchy.isDefined) {
                    val List(ArrayConst(goals)) = maybeGoalHierarchy.get.term.params: @unchecked
                    for ((goal, i) <- goals.zipWithIndex) {
                        val goalCfg =
                            cc.cfg.copy(
                                useProgressiveTightening = cc.cfg.useProgressiveTightening && goals.size < 2,
                                maybeTargetObjectiveValue = if (i == 0) cc.cfg.maybeTargetObjectiveValue else None)
                        goal match {
                            case Term("sat_goal", Seq(a)) =>
                                objectives.append(createSatisfactionObjective(goalCfg, compileBoolExpr(a)))
                            case Term("int_min_goal", Seq(a)) =>
                                objectives.append(createMinimizationObjective(goalCfg, a))
                            case Term("int_max_goal", Seq(a)) =>
                                objectives.append(createMaximizationObjective(goalCfg, a))
                            case _ => ???
                        }
                    }
                }
            case Minimize(a, _) =>
                objectives.append(createMinimizationObjective(cc.cfg, a))
            case Maximize(a, _) =>
                objectives.append(createMaximizationObjective(cc.cfg, a))
        }
        objectives.prepend(createSatisfactionObjective(cc.cfg, cc.costVars))
        cc.objective =
            if (objectives.size == 1) objectives.head
            else new HierarchicalObjective(objectives.toList, cc.cfg.focusOnTopObjective, cc.cfg.stopOnFirstSolution)
    }

    private def createSatisfactionObjective
        (cfg: FlatZincSolverConfiguration, costVars: Seq[BooleanVariable]):
        SatisfactionObjective =
    {
        val costVar = createBoolChannel()
        cc.space.post(new Conjunction(nextConstraintId(), null, costVars.toIndexedSeq, costVar))
        createSatisfactionObjective(cfg, costVar)
    }

    private def createSatisfactionObjective
        (cfg: FlatZincSolverConfiguration, costVar: BooleanVariable):
        SatisfactionObjective =
    {
        cc.space.registerObjectiveVariable(costVar)
        new SatisfactionObjective(costVar)
    }

    private def createMinimizationObjective
        (cfg: FlatZincSolverConfiguration, x: IntegerVariable):
        MinimizationObjective[IntegerValue] =
    {
        val dx = x.domain
        val maybeY =
            if (cc.space.isDanglingVariable(x)) {
                val lb =
                    dx.maybeLb.getOrElse(
                        cfg.maybeTargetObjectiveValue.map(IntegerValue.apply).getOrElse(
                            IntegerValueTraits.minValue))
                cc.logger.log("Objective variable %s is dangling, assigning %s to it".format(x, lb))
                cc.space.setValue(x, lb)
                None
            }
            else if (cfg.useProgressiveTightening && dx.maybeUb.isDefined) {
                cc.logger.log("Objective variable %s has upper bound, setting up for progressive tightening".format(x))
                val y = new IntegerVariable(cc.space.nextVariableId(), "_YUCK_UB", IntegerRange(dx.lb + One, dx.ub + One))
                cc.space.setValue(y, dx.ub + One)
                cc.implicitlyConstrainedVars += y
                val costs = createBoolChannel()
                cc.costVars += costs
                cc.space.post(new Lt(nextConstraintId(), null, x, y, costs))
                Some(y)
            } else {
                None
            }
        cc.space.registerObjectiveVariable(x)
        new MinimizationObjective[IntegerValue](x, cfg.maybeTargetObjectiveValue.map(IntegerValue.apply), maybeY)
    }

    private def createMaximizationObjective
        (cfg: FlatZincSolverConfiguration, x: IntegerVariable):
        MaximizationObjective[IntegerValue] =
    {
        val dx = x.domain
        val maybeY =
            if (cc.space.isDanglingVariable(x)) {
                val ub =
                    dx.maybeUb.getOrElse(
                        cfg.maybeTargetObjectiveValue.map(IntegerValue.apply).getOrElse(
                            IntegerValueTraits.maxValue))
                cc.logger.log("Objective variable %s is dangling, assigning %s to it".format(x, ub))
                cc.space.setValue(x, ub)
                None
            }
            else if (cfg.useProgressiveTightening && dx.maybeLb.isDefined) {
                cc.logger.log("Objective variable %s has lower bound, setting up for progressive tightening".format(x))
                val y = new IntegerVariable(cc.space.nextVariableId(), "_YUCK_LB", IntegerRange(dx.lb - One, dx.ub - One))
                cc.space.setValue(y, dx.lb - One)
                cc.implicitlyConstrainedVars += y
                val costs = createBoolChannel()
                cc.costVars += costs
                cc.space.post(new Lt(nextConstraintId(), null, y, x, costs))
                Some(y)
            } else {
                None
            }
        cc.space.registerObjectiveVariable(x)
        new MaximizationObjective[IntegerValue](x, cfg.maybeTargetObjectiveValue.map(IntegerValue.apply), maybeY)
    }

}
