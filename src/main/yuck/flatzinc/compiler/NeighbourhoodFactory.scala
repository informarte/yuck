package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.Alldistinct
import yuck.constraints.UnaryConstraint
import yuck.constraints.DistributionMaintainer
import yuck.core._
import yuck.flatzinc.ast.Minimize
import yuck.flatzinc.ast.Maximize

/**
 * Customizable factory for creating a neighbourhood for the problem at hand.
 *
 * The default implementation creates a [[yuck.core.RandomReassignmentGenerator
 * RandomReassignmentGenerator]] instance on the involved search variables.
 * This behaviour can be customized via three hooks:
 * createNeighbourhoodFor{Satisfaction, Minimization, Maximization}Goal.
 *
 * In case we end up with two neighbourhoods (one for the satisfaction goal and another
 * for the optimization goal), these neighbourhoods will be stacked by creating an instance
 * of [[yuck.core.NeighbourhoodCollection NeighbourhoodCollection]] instrumented to
 * focus on the satisfaction goal in case hard constraints are violated. To this end,
 * we keep track of goal satisfaction by means of a dynamic distribution (maintained by
 * an instance of [[yuck.constraints.DistributionMaintainer DistributionMaintainer]]).
 *
 * @author Michael Marte
 */
class NeighbourhoodFactory
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    protected val cfg = cc.cfg
    protected val logger = cc.logger
    protected val ast = cc.ast
    protected val space = cc.space
    protected val implicitlyConstrainedVars = cc.implicitlyConstrainedVars

    override def run {
        cc.maybeNeighbourhood = createNeighbourhood
    }

    private final def createNeighbourhood: Option[Neighbourhood] = {
        val maybeNeighbourhood0 =
            logger.withTimedLogScope("Creating a neighbourhood for solving hard constraints") {
                createNeighbourhoodForSatisfactionGoal(cc.costVar)
            }
        cc.ast.solveGoal match {
            case Minimize(a, _) =>
                val maybeNeighbourhood1 =
                    logger.withTimedLogScope("Creating a neighbourhood for minimizing %s".format(a)) {
                        createNeighbourhoodForMinimizationGoal(a)
                    }
                stackNeighbourhoods(cc.costVar, maybeNeighbourhood0, a, maybeNeighbourhood1)
            case Maximize(a, _) =>
                val maybeNeighbourhood1 =
                    logger.withTimedLogScope("Creating a neighbourhood for maximizing %s".format(a)) {
                        createNeighbourhoodForMaximizationGoal(a)
                    }
                stackNeighbourhoods(cc.costVar, maybeNeighbourhood0, a, maybeNeighbourhood1)
            case _ =>
                maybeNeighbourhood0
        }
    }

    private def stackNeighbourhoods(
        costs0: Variable[IntegerValue], maybeNeighbourhood0: Option[Neighbourhood],
        costs1: Variable[IntegerValue], maybeNeighbourhood1: Option[Neighbourhood]):
        Option[Neighbourhood] =
    {
        (maybeNeighbourhood0, maybeNeighbourhood1) match {
            case (None, None) => None
            case (Some(neighbourhood0), None) => Some(neighbourhood0)
            case (None, Some(neighbourhood1)) => Some(neighbourhood1)
            case (Some(neighbourhood0), Some(neighbourhood1)) =>
                val List(objective0, objective1) = cc.objective.asInstanceOf[HierarchicalObjective].objectives
                val weight0 = createNonNegativeChannel[IntegerValue]
                space.post(new LevelWeightMaintainer(nextConstraintId, null, costs0, weight0, objective0, Ten))
                val weight1 = createNonNegativeChannel[IntegerValue]
                space.post(new LevelWeightMaintainer(nextConstraintId, null, costs1, weight1, objective1, One))
                val hotSpotDistribution = new ArrayBackedDistribution(2)
                space.post(
                    new DistributionMaintainer(
                        nextConstraintId, null, List(weight0, weight1).toIndexedSeq, hotSpotDistribution))
                Some(new NeighbourhoodCollection(
                        immutable.IndexedSeq(neighbourhood0, neighbourhood1), randomGenerator, Some(hotSpotDistribution), 0))
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

    protected def createNeighbourhoodForSatisfactionGoal(x: Variable[IntegerValue]): Option[Neighbourhood] =
        createNeighbourhoodOnInvolvedSearchVariables(x)

    protected def createNeighbourhoodForMinimizationGoal(x: Variable[IntegerValue]): Option[Neighbourhood] = {
        if (space.isDanglingVariable(x) && x.domain.asInstanceOf[IntegerDomain].maybeLb.isDefined) {
            val a = x.domain.asInstanceOf[IntegerDomain].maybeLb.get
            logger.logg("Assigning %s to dangling objective variable %s".format(a, x))
            space.setValue(x, a)
            None
        } else {
            createNeighbourhoodOnInvolvedSearchVariables(x)
        }
    }

    protected def createNeighbourhoodForMaximizationGoal(x: Variable[IntegerValue]): Option[Neighbourhood] = {
        if (space.isDanglingVariable(x) && x.domain.asInstanceOf[IntegerDomain].maybeUb.isDefined) {
            val a = x.domain.asInstanceOf[IntegerDomain].maybeUb.get
            logger.logg("Assigning %s to dangling objective variable %s".format(a, x))
            space.setValue(x, a)
            None
        } else {
            createNeighbourhoodOnInvolvedSearchVariables(x)
        }
    }

    protected final def createNeighbourhoodOnInvolvedSearchVariables(x: Variable[IntegerValue]): Option[Neighbourhood] = {
        val xs =
            (if (space.isSearchVariable(x)) Set(x.asInstanceOf[AnyVariable]) else space.involvedSearchVariables(x)) --
            implicitlyConstrainedVars
        if (xs.isEmpty) {
            None
        } else {
            logger.logg("Adding a neighbourhood over %s".format(xs))
            Some(new RandomReassignmentGenerator(space, xs.toIndexedSeq, randomGenerator, cfg.moveSizeDistribution, None, 0))
        }
    }

}
