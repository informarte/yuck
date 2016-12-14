package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.Alldistinct
import yuck.constraints.UnaryConstraint
import yuck.constraints.DistributionMaintainer
import yuck.core._
import yuck.flatzinc.ast.Minimize
import yuck.flatzinc.ast.Maximize

/**
 * Customizable factory for creating a strategy (a move generator) for the problem at hand.
 *
 * The default implementation creates a [[yuck.core.RandomReassignmentGenerator
 * RandomReassignmentGenerator]] instance on the involved search variables.
 * This behaviour can be customized via three hooks:
 * createMoveGeneratorFor{Satisfaction, Minimization, Maximization}Goal.
 *
 * In case we end up with two move generators (one for the satisfaction goal and another
 * for the optimization goal), these generators will be stacked by creating an instance
 * of [[yuck.core.MoveGeneratorCollection MoveGeneratorCollection]] instrumented to
 * focus on the satisfaction goal in case hard constraints are violated. To this end,
 * we keep track of goal satisfaction by means of a dynamic distribution (maintained by
 * an instance of [[yuck.constraints.DistributionMaintainer DistributionMaintainer]]).
 *
 * @author Michael Marte
 */
class StrategyFactory
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    protected val cfg = cc.cfg
    protected val logger = cc.logger
    protected val ast = cc.ast
    protected val space = cc.space
    protected val variablesToIgnore = new mutable.HashSet[AnyVariable]

    override def run {
        cc.strategy = createMoveGenerator
    }

    private final def createMoveGenerator: MoveGenerator = {
        val maybeMoveGenerator0 =
            logger.withTimedLogScope("Creating a move generator for solving hard constraints") {
                createMoveGeneratorForSatisfactionGoal(cc.costVar)
            }
        cc.ast.solveGoal match {
            case Minimize(a, _) =>
                val maybeMoveGenerator1 =
                    logger.withTimedLogScope("Creating a move generator for minimizing %s".format(a)) {
                        createMoveGeneratorForMinimizationGoal(a)
                    }
                stackMoveGenerators(cc.costVar, maybeMoveGenerator0, a, maybeMoveGenerator1)
            case Maximize(a, _) =>
                val maybeMoveGenerator1 =
                    logger.withTimedLogScope("Creating a move generator for maximizing %s".format(a)) {
                        createMoveGeneratorForMaximizationGoal(a)
                    }
                stackMoveGenerators(cc.costVar, maybeMoveGenerator0, a, maybeMoveGenerator1)
            case _ =>
                maybeMoveGenerator0.getOrElse(null)
        }
    }

    private def stackMoveGenerators(
        costs0: Variable[IntegerValue], maybeMoveGenerator0: Option[MoveGenerator],
        costs1: Variable[IntegerValue], maybeMoveGenerator1: Option[MoveGenerator]): MoveGenerator =
    {
        (maybeMoveGenerator0, maybeMoveGenerator1) match {
            case (None, None) => null
            case (Some(moveGenerator0), None) => moveGenerator0
            case (None, Some(moveGenerator1)) => moveGenerator1
            case (Some(moveGenerator0), Some(moveGenerator1)) =>
                val List(objective0, objective1) = cc.objective.asInstanceOf[HierarchicalObjective].objectives
                val weight0 = createNonNegativeChannel[IntegerValue]
                space.post(new LevelWeightMaintainer(nextConstraintId, null, costs0, weight0, objective0, Ten))
                val weight1 = createNonNegativeChannel[IntegerValue]
                space.post(new LevelWeightMaintainer(nextConstraintId, null, costs1, weight1, objective1, One))
                val hotSpotDistribution = new ArrayBackedDistribution(2)
                space.post(
                    new DistributionMaintainer(
                        nextConstraintId, null, List(weight0, weight1).toIndexedSeq, hotSpotDistribution))
                new MoveGeneratorCollection(
                    immutable.IndexedSeq(moveGenerator0, moveGenerator1), randomGenerator, hotSpotDistribution, 0)
        }
    }

    private class LevelWeightMaintainer
        (id: Id[yuck.core.Constraint],
         goal: Goal,
         costs: Variable[IntegerValue],
         priority: Variable[IntegerValue],
         objective: AnyObjective,
         gain: IntegerValue)
        extends UnaryConstraint(id, goal, costs, priority)
    {
        override def toString = "levelWeightMaintainer(%s, %s, %s, %s)".format(costs, priority, objective, gain)
        override def op(costs: IntegerValue) = if (objective.isGoodEnough(costs)) One else gain
    }

    protected def createMoveGeneratorForSatisfactionGoal(x: Variable[IntegerValue]): Option[MoveGenerator] =
        createMoveGeneratorOnInvolvedSearchVariables(x)

    protected def createMoveGeneratorForMinimizationGoal(x: Variable[IntegerValue]): Option[MoveGenerator] = {
        if (space.isDanglingVariable(x) && x.domain.asInstanceOf[IntegerDomain].maybeLb.isDefined) {
            val a = x.domain.asInstanceOf[IntegerDomain].maybeLb.get
            logger.logg("Assigning %s to dangling objective variable %s".format(a, x))
            space.setValue(x, a)
            None
        } else {
            createMoveGeneratorOnInvolvedSearchVariables(x)
        }
    }

    protected def createMoveGeneratorForMaximizationGoal(x: Variable[IntegerValue]): Option[MoveGenerator] = {
        if (space.isDanglingVariable(x) && x.domain.asInstanceOf[IntegerDomain].maybeUb.isDefined) {
            val a = x.domain.asInstanceOf[IntegerDomain].maybeUb.get
            logger.logg("Assigning %s to dangling objective variable %s".format(a, x))
            space.setValue(x, a)
            None
        } else {
            createMoveGeneratorOnInvolvedSearchVariables(x)
        }
    }

    protected final def createMoveGeneratorOnInvolvedSearchVariables(x: Variable[IntegerValue]): Option[MoveGenerator] = {
        val xs =
            (if (space.isSearchVariable(x)) Set(x.asInstanceOf[AnyVariable]) else space.involvedSearchVariables(x)) --
            variablesToIgnore
        if (xs.isEmpty) {
            None
        } else {
            logger.logg("Adding exchange generator on %s".format(xs))
            Some(new RandomReassignmentGenerator(space, xs.toIndexedSeq, randomGenerator, cfg.moveSizeDistribution, null, 0))
        }
    }

}
