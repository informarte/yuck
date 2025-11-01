package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.{Conjunction, Lt}
import yuck.core.*
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
                                objectives.append(createMinimizationObjective(goalCfg, a, maybeBound(i + 1)))
                            case Term("int_max_goal", Seq(a)) =>
                                objectives.append(createMaximizationObjective(goalCfg, a, maybeBound(i + 1)))
                            case _ => ???
                        }
                    }
                }
            case Minimize(a, _) =>
                objectives.append(createMinimizationObjective(cc.cfg, a, maybeBound(1)))
            case Maximize(a, _) =>
                objectives.append(createMaximizationObjective(cc.cfg, a, maybeBound(1)))
        }
        objectives.prepend(createSatisfactionObjective(cc.cfg, cc.costVars))
        cc.objective =
            if objectives.size == 1
            then objectives.head
            else new HierarchicalObjective(objectives.toList, cc.cfg.focusOnTopObjective, cc.cfg.stopOnFirstSolution)
    }

    private def maybeBound(i: Int): Option[IntegerValue] =
        if cc.cfg.shareBounds
        then cc.sharedBound.maybeBound().map(_.asInstanceOf[PolymorphicListValue].value(i).asInstanceOf[IntegerValue])
        else None

    private def createSatisfactionObjective
        (cfg: FlatZincSolverConfiguration, costVars: Set[BooleanVariable]):
        SatisfactionObjective =
    {
        val costVar = createBoolChannel()
        cc.post(new Conjunction(nextConstraintId(), None, costVars.toVector, costVar))
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
        (cfg: FlatZincSolverConfiguration, x: IntegerVariable, maybeBound: Option[IntegerValue]):
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
                val dy = IntegerRange(dx.lb + One, maybeBound.getOrElse(dx.ub) + One)
                val y = new IntegerVariable(cc.space.nextVariableId(), "_YUCK_UB", dy)
                cc.space.setValue(y, dx.ub + One)
                cc.implicitlyConstrainedVars += y
                val costs = createBoolChannel()
                cc.costVars += costs
                cc.post(new Lt(nextConstraintId(), None, x, y, costs))
                Some(y)
            } else {
                cc.logger.log("Objective variable %s has no upper bound, so progressive tightening is not possible".format(x))
                None
            }
        cc.space.registerObjectiveVariable(x)
        new MinimizationObjective[IntegerValue](x, cfg.maybeTargetObjectiveValue.map(IntegerValue.apply), maybeY)
    }

    private def createMaximizationObjective
        (cfg: FlatZincSolverConfiguration, x: IntegerVariable, maybeBound: Option[IntegerValue]):
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
                val dy = IntegerRange(maybeBound.getOrElse(dx.lb) - One, dx.ub - One)
                val y = new IntegerVariable(cc.space.nextVariableId(), "_YUCK_LB", dy)
                cc.space.setValue(y, dx.lb - One)
                cc.implicitlyConstrainedVars += y
                val costs = createBoolChannel()
                cc.costVars += costs
                cc.post(new Lt(nextConstraintId(), None, y, x, costs))
                Some(y)
            } else {
                cc.logger.log("Objective variable %s has no lower bound, so progressive tightening is not possible".format(x))
                None
            }
        cc.space.registerObjectiveVariable(x)
        new MaximizationObjective[IntegerValue](x, cfg.maybeTargetObjectiveValue.map(IntegerValue.apply), maybeY)
    }

}
