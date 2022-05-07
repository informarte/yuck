package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.*
import yuck.core.*
import yuck.flatzinc.FlatZincLevelConfiguration

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
 * focus on the satisfaction goal in case hard constraints are violated.
 *
 * In an attempt to decouple this factory from implementation details of data structures
 * (hash sets, in particular) and the earlier compiler stages, we sort constraints and
 * variables (by id) before further processing.
 *
 * @author Michael Marte
 */
abstract class NeighbourhoodFactory extends CompilationPhase {

    protected val randomGenerator: RandomGenerator

    protected val cfg = cc.cfg
    protected val logger = cc.logger
    protected val ast = cc.ast
    protected val space = cc.space
    protected val implicitlyConstrainedVars = cc.implicitlyConstrainedVars
    protected val neighbourhoodsFromImplicitConstraints = new mutable.ArrayBuffer[Neighbourhood]

    override def run() = {
        cc.maybeNeighbourhood = createNeighbourhood
    }

    private final def createNeighbourhood: Option[Neighbourhood] = {
        val buf = new mutable.ArrayBuffer[(PrimitiveObjective, Option[Neighbourhood])]
        for ((objective, i) <- cc.objective.primitiveObjectives.zipWithIndex) {
            val levelCfg = if (i == 0) cfg.topLevelConfiguration else cfg.subordinateLevelConfiguration
            objective.x match {
                case costVar: BooleanVariable =>
                    logger.withTimedLogScope("Creating a neighbourhood for satisfaction") {
                        buf.append((objective, createSatisfactionNeighbourhood(levelCfg, costVar)))
                    }
                case objectiveVar: IntegerVariable =>
                    objective.optimizationMode match {
                        case OptimizationMode.Min =>
                            logger.withTimedLogScope("Creating a neighbourhood for minimizing %s".format(objectiveVar)) {
                                buf.append((objective, createMinimizationNeighbourhood(levelCfg, objectiveVar)))
                            }
                        case OptimizationMode.Max =>
                            logger.withTimedLogScope("Creating a neighbourhood for maximizing %s".format(objectiveVar)) {
                                buf.append((objective, createMaximizationNeighbourhood(levelCfg, objectiveVar)))
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

    protected def createSatisfactionNeighbourhood
        (levelCfg: FlatZincLevelConfiguration, x: BooleanVariable):
        Option[Neighbourhood] =
    {
        val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
        val candidatesForImplicitSolving =
            if (cfg.useImplicitSolving && levelCfg.isTopLevel) {
                require(space.definingConstraint(x).isInstanceOf[Conjunction])
                space.definingConstraint(x).inVariables.iterator
                    .map(space.maybeDefiningConstraint).filter(_.isDefined).map(_.get)
                    .filter(_.isCandidateForImplicitSolving(space)).toBuffer.sorted
            } else {
                Nil
            }
        val constraintHardness: Map[Class[_ <: Constraint], Int] = Map(
            (classOf[Circuit], 3), (classOf[Inverse], 3),
            (classOf[Alldistinct[_]], 2),
            (classOf[Table[_]], 1))
        def constraintRanking(constraint: Constraint): Int =
            -(constraintHardness(constraint.getClass) * constraint.inVariables.size)
        for (constraint <- randomGenerator.shuffle(candidatesForImplicitSolving).sortBy(constraintRanking)) {
            val xs = constraint.inVariables.toSet
            if ((xs & implicitlyConstrainedVars).isEmpty) {
                val maybeNeighbourhood =
                    constraint.createNeighbourhood(
                        space, randomGenerator, cfg.moveSizeDistribution, logger, cc.sigint,
                        ExtraNeighbourhoodFactoryConfiguration(
                            createHotSpotDistribution = xs => Some(createHotSpotDistribution1(xs, cc.costVars)),
                            maybeFairVariableChoiceRate = levelCfg.maybeFairVariableChoiceRate,
                            checkIncrementalCostUpdate = cfg.checkIncrementalCostUpdate,
                            checkAssignmentsToNonChannelVariables = cfg.checkAssignmentsToNonChannelVariables))
                if (maybeNeighbourhood.isDefined) {
                    implicitlyConstrainedVars ++= xs
                    space.registerImplicitConstraint(constraint)
                    logger.log("Adding a neighbourhood for implicit constraint %s".format(constraint))
                    neighbourhoods += maybeNeighbourhood.get
                    neighbourhoodsFromImplicitConstraints += maybeNeighbourhood.get
                }
            }
        }
        val xs = space.involvedSearchVariables(x).diff(implicitlyConstrainedVars).toBuffer.sorted.toIndexedSeq
        if (! xs.isEmpty) {
            for (x <- xs if ! x.domain.isFinite) {
                throw new VariableWithInfiniteDomainException(x)
            }
            logger.logg("Adding a neighbourhood over %s".format(xs))
            val maybeCostVars =
                if (levelCfg.isTopLevel) Some(cc.costVars)
                else if (space.maybeDefiningConstraint(x).map(_.isInstanceOf[Conjunction]).getOrElse(false))
                    Some(space.definingConstraint(x).asInstanceOf[Conjunction].xs)
                else None
            neighbourhoods +=
                new RandomReassignmentGenerator(
                    space, xs, randomGenerator, cfg.moveSizeDistribution,
                    if (levelCfg.guideOptimization && maybeCostVars.isDefined) Some(createHotSpotDistribution1(xs, maybeCostVars.get)) else None,
                    if (levelCfg.guideOptimization) levelCfg.maybeFairVariableChoiceRate else None)
        }
        if (neighbourhoods.size < 2) {
            neighbourhoods.headOption
        } else {
            Some(new NeighbourhoodCollection(
                neighbourhoods.toIndexedSeq, randomGenerator,
                if (levelCfg.guideOptimization) Some(createHotSpotDistribution2(neighbourhoods)) else None,
                if (levelCfg.guideOptimization) levelCfg.maybeFairVariableChoiceRate else None))
        }
    }

    private def createHotSpotDistribution1(searchVars: Seq[AnyVariable], costVars: Seq[BooleanVariable]): Distribution = {
        val searchVarIndex = searchVars.iterator.zipWithIndex.toMap
        val hotSpotDistribution = Distribution(searchVars.size)
        def involvedSearchVars(x: AnyVariable) =
            space.involvedSearchVariables(x).diff(implicitlyConstrainedVars).iterator.map(searchVarIndex).toIndexedSeq
        val involvementMatrix = costVars.iterator.map(x => (x, involvedSearchVars(x))).filter(_._2.nonEmpty).toMap
        space.post(new SatisfactionGoalTracker(space.nextConstraintId(), None, involvementMatrix, hotSpotDistribution))
        hotSpotDistribution
    }

    private def createHotSpotDistribution2(neighbourhoods: Seq[Neighbourhood]): Distribution = {
        val neighbourhoodIndex = neighbourhoods.iterator.zipWithIndex.toMap
        val hotSpotDistribution = Distribution(neighbourhoods.size)
        def involvedNeighbourhoods(x: AnyVariable) = {
            val xs = space.involvedSearchVariables(x)
            def isInvolved(neighbourhood: Neighbourhood) = {
                val ys = neighbourhood.searchVariables.toSet
                xs.exists(ys.contains)
            }
            neighbourhoods.iterator.filter(isInvolved).map(neighbourhoodIndex).toIndexedSeq
        }
        val involvementMatrix = cc.costVars.iterator.map(x => (x, involvedNeighbourhoods(x))).filter(_._2.nonEmpty).toMap
        space.post(new SatisfactionGoalTracker(space.nextConstraintId(), None, involvementMatrix, hotSpotDistribution))
        hotSpotDistribution
    }

    protected def createMinimizationNeighbourhood
        [V <: NumericalValue[V]]
        (levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        createNeighbourhoodOnInvolvedSearchVariables(levelCfg, x)
    }

    protected def createMaximizationNeighbourhood
        [V <: NumericalValue[V]]
        (levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        createNeighbourhoodOnInvolvedSearchVariables(levelCfg, x)
    }

    private def createNeighbourhoodOnInvolvedSearchVariables
        (levelCfg: FlatZincLevelConfiguration, x: AnyVariable):
        Option[Neighbourhood] =
    {
        val xs0 = if (space.isSearchVariable(x)) Set(x) else space.involvedSearchVariables(x)
        val xs = xs0.diff(implicitlyConstrainedVars)
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
            for (i <- objectives.indices) {
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
        space.post(new LevelWeightMaintainer(nextConstraintId(), objectives, hotSpotDistribution))
        new NeighbourhoodCollection(neighbourhoods, randomGenerator, Some(hotSpotDistribution), None)
    }

}
