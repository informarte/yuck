package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.SatisfactionGoalTracker
import yuck.core._

/**
 * Customizable factory for creating a neighbourhood for the problem at hand.
 *
 * Candidates for implicit solving are handled with priority to avoid that their variables
 * get included into other neighbourhoods. (In case two candidates for implicit solving
 * compete for a variable, this conflict is resolved by random choice.)
 *
 * The default implementation for satisfaction goals creates a
 * [[yuck.core.RandomReassignmentGenerator RandomReassignmentGenerator]] instance
 * with a focus on search variables that are involved in constraint violations.
 * (The guidance is provided by an instance of [[yuck.constraints.SatisfactionGoalTracker
 * SatisfactionGoalTracker]]).
 * This behaviour can be customized by overloading createSatisfactionNeighbourhood.
 *
 * The default implementation for optimization goals creates an unbiased
 * [[yuck.core.RandomReassignmentGenerator RandomReassignmentGenerator]] instance
 * on the involved search variables.
 * This behaviour can be customized by overloading
 * create{Minimization, Maximization}Neighbourhood.
 *
 * In case we end up with two neighbourhoods (one for the satisfaction goal and another
 * for the optimization goal), these neighbourhoods will be stacked by creating an instance
 * of [[yuck.core.NeighbourhoodCollection NeighbourhoodCollection]] instrumented to
 * focus on the satisfaction goal in case hard constraints are violated. To this end,
 * we keep track of goal satisfaction by means of a dynamic distribution (maintained by
 * an instance of [[yuck.constraints.OptimizationGoalTracker OptimizationGoalTracker]]).
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

    private final def createNeighbourhood: Option[Neighbourhood] = {
        val buf = new mutable.ArrayBuffer[(PrimitiveObjective, Option[Neighbourhood])]
        for (objective <- cc.objective.primitiveObjectives) {
            cc.shadowedObjectiveVars.getOrElse(objective.x, objective.x) match {
                case costVar: BooleanVariable =>
                    logger.withTimedLogScope("Creating a neighbourhood for satisfaction") {
                        buf.append((objective, createSatisfactionNeighbourhood(costVar)))
                    }
                case objectiveVar: IntegerVariable =>
                    objective.optimizationMode match {
                        case OptimizationMode.Min =>
                            logger.withTimedLogScope("Creating a neighbourhood for minimizing %s".format(objectiveVar)) {
                                buf.append((objective, createMinimizationNeighbourhood(objectiveVar)))
                            }
                        case OptimizationMode.Max =>
                            logger.withTimedLogScope("Creating a neighbourhood for maximizing %s".format(objectiveVar)) {
                                buf.append((objective, createMaximizationNeighbourhood(objectiveVar)))
                            }
                    }
            }
        }
        val (objectives, neighbourhoods) =
            (for ((objective, Some(neighbourhood)) <- buf) yield (objective, neighbourhood)).unzip
        if (neighbourhoods.size < 2) {
            neighbourhoods.headOption
        } else {
            Some(stackNeighbourhoods(objectives.toIndexedSeq, neighbourhoods.toIndexedSeq))
        }
    }

    protected def createSatisfactionNeighbourhood(x: BooleanVariable): Option[Neighbourhood] = {
        val levelCfg = cfg.level0Configuration
        val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
        val candidatesForImplicitSolving =
            if (cfg.useImplicitSolving) {
                space.involvedConstraints(x).iterator.filter(_.isCandidateForImplicitSolving(space)).toBuffer.sorted
            } else {
                Nil
            }
        for (constraint <- randomGenerator.shuffle(candidatesForImplicitSolving)) {
            val xs = constraint.inVariables.toSet
            if ((xs & implicitlyConstrainedVars).isEmpty) {
                val maybeNeighbourhood =
                    constraint.prepareForImplicitSolving(
                        space, randomGenerator, cfg.moveSizeDistribution, _ => None, levelCfg.maybeFairVariableChoiceRate)
                if (maybeNeighbourhood.isDefined) {
                    implicitlyConstrainedVars ++= xs
                    space.markAsImplicit(constraint)
                    logger.logg("Adding a neighbourhood for implicit constraint %s".format(constraint))
                    neighbourhoods += maybeNeighbourhood.get
                }
            }
        }
        val searchVars =
            space.involvedSearchVariables(x).diff(implicitlyConstrainedVars).toBuffer.sorted.toIndexedSeq
        if (! searchVars.isEmpty) {
            for (x <- searchVars if ! x.domain.isFinite) {
                throw new VariableWithInfiniteDomainException(x)
            }
            if (levelCfg.guideOptimization) {
                val searchVarIndex = searchVars.iterator.zipWithIndex.toMap
                val costVars = cc.costVars.toIndexedSeq
                val hotSpotDistribution = DistributionFactory.createDistribution(searchVars.size)
                def involvedSearchVars(x: AnyVariable) =
                    space.involvedSearchVariables(x).diff(implicitlyConstrainedVars).iterator
                        .map(searchVarIndex).toIndexedSeq
                val involvementMatrix = costVars.iterator.map(x => (x, involvedSearchVars(x))).toMap
                space.post(
                    new SatisfactionGoalTracker(space.nextConstraintId, None, involvementMatrix, hotSpotDistribution))
                logger.logg("Adding a neighbourhood over %s".format(searchVars))
                neighbourhoods +=
                    new RandomReassignmentGenerator(
                        space, searchVars, randomGenerator,
                        cfg.moveSizeDistribution, Some(hotSpotDistribution), levelCfg.maybeFairVariableChoiceRate)
            } else {
                createNeighbourhoodOnInvolvedSearchVariables(x)
            }
        }
        if (neighbourhoods.size < 2) {
            neighbourhoods.headOption
        } else {
            Some(new NeighbourhoodCollection(neighbourhoods.toIndexedSeq, randomGenerator, None, None))
        }
    }

    protected def createMinimizationNeighbourhood
        [Value <: NumericalValue[Value]]
        (x: NumericalVariable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        createNeighbourhoodOnInvolvedSearchVariables(x)
    }

    protected def createMaximizationNeighbourhood
        [Value <: NumericalValue[Value]]
        (x: NumericalVariable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        createNeighbourhoodOnInvolvedSearchVariables(x)
    }

    protected final def createNeighbourhoodOnInvolvedSearchVariables(x: AnyVariable): Option[Neighbourhood] = {
        val xs =
            (if (space.isSearchVariable(x)) Set(x) else space.involvedSearchVariables(x))
                .diff(implicitlyConstrainedVars)
        if (xs.isEmpty) {
            None
        } else {
            logger.logg("Adding a neighbourhood over %s".format(xs))
            Some(new RandomReassignmentGenerator(
                space, xs.toBuffer.sorted.toIndexedSeq, randomGenerator, cfg.moveSizeDistribution, None, None))
        }
    }

    private final class LevelWeightMaintainer
        (id: Id[yuck.core.Constraint],
         objectives: immutable.IndexedSeq[PrimitiveObjective],
         distribution: Distribution)
        extends Constraint(id)
    {
        require(objectives.size == distribution.size)
        override def toString = "levelWeightMaintainer([%s])".format(objectives.mkString(", "))
        override def inVariables = objectives.map(_.x)
        override def outVariables = Nil
        override def initialize(now: SearchState) = {
            val solved = objectives(0).isSolution(now)
            for (i <- 0 until objectives.size) {
                distribution.setFrequency(i, if (i == 0) (if (solved) 0 else 1) else (if (solved) 1 else 0))
            }
            Nil
        }
        override def consult(before: SearchState, after: SearchState, move: Move) =
            initialize(after)
    }

    private def stackNeighbourhoods
        (objectives: immutable.IndexedSeq[PrimitiveObjective],
         neighbourhoods: immutable.IndexedSeq[Neighbourhood]):
        Neighbourhood =
    {
        require(objectives.size > 1)
        require(objectives.size == neighbourhoods.size)
        val hotSpotDistribution = new ArrayBackedDistribution(objectives.size)
        space.post(new LevelWeightMaintainer(nextConstraintId, objectives, hotSpotDistribution))
        new NeighbourhoodCollection(neighbourhoods, randomGenerator, Some(hotSpotDistribution), None)
    }

}
