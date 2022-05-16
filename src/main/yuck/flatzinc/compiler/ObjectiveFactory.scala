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

    private val cfg = cc.cfg
    private val logger = cc.logger
    private val space = cc.space
    private val implicitlyConstrainedVars = cc.implicitlyConstrainedVars
    private val costVars = cc.costVars

    import HighPriorityImplicits.*

    override def run() = {
        val objectives = new mutable.ArrayBuffer[PrimitiveObjective]
        cc.ast.solveGoal match {
            case Satisfy(_) =>
                val maybeGoalHierarchy = cc.ast.solveGoal.annotations.find(_.term.id == "goal_hierarchy")
                if (maybeGoalHierarchy.isDefined) {
                    val List(ArrayConst(goals)) = maybeGoalHierarchy.get.term.params
                    for ((goal, i) <- goals.zipWithIndex) {
                        val goalCfg =
                            cfg.copy(
                                useProgressiveTightening = cfg.useProgressiveTightening && goals.size < 2,
                                maybeTargetObjectiveValue = if (i == 0) cfg.maybeTargetObjectiveValue else None)
                        goal match {
                            case Term("sat_goal", List(a)) =>
                                objectives.append(createSatisfactionObjective(compileBoolExpr(a), goalCfg))
                            case Term("int_min_goal", List(a)) =>
                                objectives.append(createMinimizationObjective(a, goalCfg))
                            case Term("int_max_goal", List(a)) =>
                                objectives.append(createMaximizationObjective(a, goalCfg))
                            case _ => ???
                        }
                    }
                }
            case Minimize(a, _) =>
                objectives.append(createMinimizationObjective(a, cfg))
            case Maximize(a, _) =>
                objectives.append(createMaximizationObjective(a, cfg))
        }
        objectives.prepend(createSatisfactionObjective(costVars, cfg))
        cc.objective =
            if (objectives.size == 1) objectives.head
            else new HierarchicalObjective(objectives.toList, cfg.focusOnTopObjective, cfg.stopOnFirstSolution)
    }

    private def createSatisfactionObjective
        (costVars: Seq[BooleanVariable], cfg: FlatZincSolverConfiguration):
        SatisfactionObjective =
    {
        val costVar = createBoolChannel
        space.post(new Conjunction(nextConstraintId(), null, costVars.toIndexedSeq, costVar))
        createSatisfactionObjective(costVar, cfg)
    }

    private def createSatisfactionObjective
        (costVar: BooleanVariable, cfg: FlatZincSolverConfiguration):
        SatisfactionObjective =
    {
        space.registerObjectiveVariable(costVar)
        new SatisfactionObjective(costVar)
    }

    private def createMinimizationObjective
        (x: IntegerVariable, cfg: FlatZincSolverConfiguration):
        MinimizationObjective[IntegerValue] =
    {
        val dx = x.domain
        val maybeY =
            if (space.isDanglingVariable(x)) {
                val lb =
                    dx.maybeLb.getOrElse(
                        cfg.maybeTargetObjectiveValue.map(IntegerValue.apply).getOrElse(
                            IntegerValueTraits.minValue))
                logger.log("Objective variable %s is dangling, assigning %s to it".format(x, lb))
                space.setValue(x, lb)
                None
            }
            else if (cfg.useProgressiveTightening && dx.maybeUb.isDefined) {
                logger.log("Objective variable %s has upper bound, setting up for progressive tightening".format(x))
                val y = new IntegerVariable(space.nextVariableId(), "_YUCK_UB", IntegerRange(dx.lb + One, dx.ub + One))
                space.setValue(y, dx.ub + One)
                implicitlyConstrainedVars += y
                val costs = createBoolChannel
                costVars += costs
                space.post(new Lt(nextConstraintId(), null, x, y, costs))
                Some(y)
            } else {
                None
            }
        space.registerObjectiveVariable(x)
        new MinimizationObjective[IntegerValue](x, cfg.maybeTargetObjectiveValue.map(IntegerValue.apply), maybeY)
    }

    private def createMaximizationObjective
        (x: IntegerVariable, cfg: FlatZincSolverConfiguration):
        MaximizationObjective[IntegerValue] =
    {
        val dx = x.domain
        val maybeY =
            if (space.isDanglingVariable(x)) {
                val ub =
                    dx.maybeUb.getOrElse(
                        cfg.maybeTargetObjectiveValue.map(IntegerValue.apply).getOrElse(
                            IntegerValueTraits.maxValue))
                logger.log("Objective variable %s is dangling, assigning %s to it".format(x, ub))
                space.setValue(x, ub)
                None
            }
            else if (cfg.useProgressiveTightening && dx.maybeLb.isDefined) {
                logger.log("Objective variable %s has lower bound, setting up for progressive tightening".format(x))
                val y = new IntegerVariable(space.nextVariableId(), "_YUCK_LB", IntegerRange(dx.lb - One, dx.ub - One))
                space.setValue(y, dx.lb - One)
                implicitlyConstrainedVars += y
                val costs = createBoolChannel
                costVars += costs
                space.post(new Lt(nextConstraintId(), null, y, x, costs))
                Some(y)
            } else {
                None
            }
        space.registerObjectiveVariable(x)
        new MaximizationObjective[IntegerValue](x, cfg.maybeTargetObjectiveValue.map(IntegerValue.apply), maybeY)
    }

}
