package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.{Alldistinct, DistributionMaintainer, BinaryConstraint}
import yuck.core._
import yuck.flatzinc.ast.{Minimize, Maximize}

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
 * In an attempt to decouple this factory from implementation details of data structures
 * (hash sets, in particular) and the earlier compiler stages, we sort constraints and
 * variables (by id) before further processing.
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

    override def run = {
        cc.maybeNeighbourhood = createNeighbourhood
    }

    import HighPriorityImplicits._

    private final def createNeighbourhood: Option[Neighbourhood] = {
        val maybeNeighbourhood0 =
            logger.withTimedLogScope("Creating a neighbourhood for solving hard constraints") {
                createNeighbourhoodForSatisfactionGoal(cc.costVar)
            }
        val maybeNeighbourhood1 =
            cc.ast.solveGoal match {
                case Minimize(a, _) =>
                    val maybeNeighbourhood1 =
                        logger.withTimedLogScope("Creating a neighbourhood for minimizing %s".format(a)) {
                            createNeighbourhoodForMinimizationGoal[IntegerValue](a)
                        }
                    stackNeighbourhoods(cc.costVar, maybeNeighbourhood0, a, maybeNeighbourhood1)
                case Maximize(a, _) =>
                    val maybeNeighbourhood1 =
                        logger.withTimedLogScope("Creating a neighbourhood for maximizing %s".format(a)) {
                            createNeighbourhoodForMaximizationGoal[IntegerValue](a)
                        }
                    stackNeighbourhoods(cc.costVar, maybeNeighbourhood0, a, maybeNeighbourhood1)
                case _ =>
                    maybeNeighbourhood0
            }
        maybeNeighbourhood1
    }

    protected def createNeighbourhoodForSatisfactionGoal(x: BooleanVariable): Option[Neighbourhood] =
        createNeighbourhoodOnInvolvedSearchVariables(x)

    protected def createNeighbourhoodForMinimizationGoal
        [Value <: NumericalValue[Value]]
        (x: NumericalVariable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        val dx = x.domain
        if (space.isDanglingVariable(x) && dx.hasLb) {
            val a = dx.lb
            logger.logg("Assigning %s to dangling objective variable %s".format(a, x))
            space.setValue(x, a)
            None
        } else {
            createNeighbourhoodOnInvolvedSearchVariables(x)
        }
    }

    protected def createNeighbourhoodForMaximizationGoal
        [Value <: NumericalValue[Value]]
        (x: NumericalVariable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        val dx = x.domain
        if (space.isDanglingVariable(x) && dx.hasUb) {
            val a = dx.ub
            logger.logg("Assigning %s to dangling objective variable %s".format(a, x))
            space.setValue(x, a)
            None
        } else {
            createNeighbourhoodOnInvolvedSearchVariables(x)
        }
    }

    private final class Level
        [Value <: NumericalValue[Value]]
        (val costs: NumericalVariable[Value], val objective: AnyObjective)
    {
        val weight = createNonNegativeChannel[IntegerValue]
        val effect = new ReusableMoveEffectWithFixedVariable(weight)
        override def toString = (costs, weight).toString
    }

    private final class LevelWeightMaintainer
        (id: Id[yuck.core.Constraint], goal: Goal, level0: Level[BooleanValue], level1: Level[IntegerValue])
        extends Constraint(id, goal)
    {
        override def inVariables = List(level0.costs, level1.costs)
        override def outVariables = List(level0.weight, level1.weight)
        private val effects = List(level0.effect, level1.effect)
        override def toString = "levelWeightMaintainer(%s, %s)".format(level0, level1)
        override def initialize(now: SearchState) = {
            val solved = level0.objective.isGoodEnough(now.value(level0.costs))
            level0.effect.a = if (solved) Zero else One
            level1.effect.a = if (! solved || level1.objective.isGoodEnough(now.value(level1.costs))) Zero else One
            effects
        }
        override def consult(before: SearchState, after: SearchState, move: Move) =
            initialize(after)
        override def commit(before: SearchState, after: SearchState, move: Move) =
            effects
    }

    private final def stackNeighbourhoods(
        costs0: BooleanVariable, maybeNeighbourhood0: Option[Neighbourhood],
        costs1: IntegerVariable, maybeNeighbourhood1: Option[Neighbourhood]):
        Option[Neighbourhood] =
    {
        (maybeNeighbourhood0, maybeNeighbourhood1) match {
            case (None, None) => None
            case (Some(neighbourhood0), None) => Some(neighbourhood0)
            case (None, Some(neighbourhood1)) => Some(neighbourhood1)
            case (Some(neighbourhood0), Some(neighbourhood1)) =>
                val List(objective0, objective1) = cc.objective.asInstanceOf[HierarchicalObjective].objectives
                val level0 = new Level(costs0, objective0)
                val level1 = new Level(costs1, objective1)
                space.post(new LevelWeightMaintainer(nextConstraintId, null, level0, level1))
                val hotSpotDistribution = new ArrayBackedDistribution(2)
                space.post(
                    new DistributionMaintainer(
                        nextConstraintId, null,
                        OptimizationMode.Min, List(level0.weight, level1.weight).toIndexedSeq, hotSpotDistribution))
                Some(new NeighbourhoodCollection(
                        Vector(neighbourhood0, neighbourhood1), randomGenerator, Some(hotSpotDistribution), None))
        }
    }

    protected final def createNeighbourhoodOnInvolvedSearchVariables(x: AnyVariable): Option[Neighbourhood] = {
        space.registerObjectiveVariable(x)
        val xs =
            (if (space.isSearchVariable(x)) Set(x) else space.involvedSearchVariables(x)) --
            implicitlyConstrainedVars
        if (xs.isEmpty) {
            None
        } else {
            logger.logg("Adding a neighbourhood over %s".format(xs))
            Some(new RandomReassignmentGenerator(
                    space, xs.toBuffer.sorted.toIndexedSeq, randomGenerator, cfg.moveSizeDistribution, None, None))
        }
    }

}
