package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.*
import yuck.core.{given, *}
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

    protected val neighbourhoodsFromImplicitConstraints = new mutable.ArrayBuffer[Neighbourhood]

    override def run() = {
        cc.maybeNeighbourhood = createNeighbourhood
    }

    private final def createNeighbourhood: Option[Neighbourhood] = {
        val buf = new mutable.ArrayBuffer[(PrimitiveObjective, Option[Neighbourhood])]
        for ((objective, i) <- cc.objective.primitiveObjectives.zipWithIndex) {
            val levelCfg = if (i == 0) cc.cfg.topLevelConfiguration else cc.cfg.subordinateLevelConfiguration
            objective.x match {
                case costVar: BooleanVariable =>
                    cc.logger.withTimedLogScope("Creating a neighbourhood for satisfaction") {
                        buf.append((objective, createSatisfactionNeighbourhood(levelCfg, costVar)))
                    }
                case objectiveVar: IntegerVariable =>
                    objective.optimizationMode match {
                        case OptimizationMode.Min =>
                            cc.logger.withTimedLogScope("Creating a neighbourhood for minimizing %s".format(objectiveVar)) {
                                buf.append((objective, createMinimizationNeighbourhood(levelCfg, objectiveVar)))
                            }
                        case OptimizationMode.Max =>
                            cc.logger.withTimedLogScope("Creating a neighbourhood for maximizing %s".format(objectiveVar)) {
                                buf.append((objective, createMaximizationNeighbourhood(levelCfg, objectiveVar)))
                            }
                    }
            }
        }
        val (objectives, neighbourhoods) =
            (for (case (objective, Some(neighbourhood)) <- buf) yield (objective, neighbourhood)).unzip
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
            if (cc.cfg.useImplicitSolving && levelCfg.isTopLevel) {
                require(cc.space.definingConstraint(x).isInstanceOf[Conjunction])
                cc.space.definingConstraint(x).inVariables.iterator
                    .map(cc.space.maybeDefiningConstraint(_)).filter(_.isDefined).map(_.get)
                    .filter(_.isCandidateForImplicitSolving(cc.space)).toBuffer.sorted.toIndexedSeq
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
            if ((xs & cc.implicitlyConstrainedVars).isEmpty) {
                val maybeNeighbourhood =
                    constraint.createNeighbourhood(
                        cc.space, randomGenerator, cc.cfg.moveSizeDistribution, cc.logger, cc.sigint,
                        ExtraNeighbourhoodFactoryConfiguration(
                            createHotSpotDistribution = xs => Some(createHotSpotDistribution1(xs, cc.costVars)),
                            maybeFairVariableChoiceRate = levelCfg.maybeFairVariableChoiceRate,
                            checkIncrementalCostUpdate = cc.cfg.checkIncrementalCostUpdate,
                            checkAssignmentsToNonChannelVariables = cc.cfg.checkAssignmentsToNonChannelVariables))
                if (maybeNeighbourhood.isDefined) {
                    cc.implicitlyConstrainedVars ++= xs
                    cc.space.registerImplicitConstraint(constraint)
                    cc.logger.log("Adding a neighbourhood for implicit constraint %s".format(constraint))
                    neighbourhoods += maybeNeighbourhood.get
                    neighbourhoodsFromImplicitConstraints += maybeNeighbourhood.get
                }
            }
        }
        val xs = cc.space.involvedSearchVariables(x).diff(cc.implicitlyConstrainedVars).toBuffer.sorted.toIndexedSeq
        if (! xs.isEmpty) {
            for (x <- xs if ! x.domain.isFinite) {
                throw new VariableWithInfiniteDomainException(x)
            }
            cc.logger.logg("Adding a neighbourhood over %s".format(xs))
            val maybeCostVars =
                if (levelCfg.isTopLevel) Some(cc.costVars)
                else if (cc.space.maybeDefiningConstraint(x).exists(_.isInstanceOf[Conjunction]))
                    Some(cc.space.definingConstraint(x).asInstanceOf[Conjunction].xs.toSet)
                else None
            neighbourhoods +=
                new RandomReassignmentGenerator(
                    cc.space, xs, randomGenerator, cc.cfg.moveSizeDistribution,
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

    private def createHotSpotDistribution1(searchVars: Seq[AnyVariable], costVars: Set[BooleanVariable]): Distribution = {
        val searchVarIndex = searchVars.iterator.zipWithIndex.toMap
        val hotSpotDistribution = Distribution(searchVars.size)
        def involvedSearchVars(x: AnyVariable) =
            cc.space.involvedSearchVariables(x).diff(cc.implicitlyConstrainedVars).iterator.map(searchVarIndex).toIndexedSeq
        val involvementMatrix = costVars.iterator.map(x => (x, involvedSearchVars(x))).filter(_._2.nonEmpty).toMap
        cc.space.post(new SatisfactionGoalTracker(cc.space.nextConstraintId(), None, involvementMatrix, hotSpotDistribution))
        hotSpotDistribution
    }

    private def createHotSpotDistribution2(neighbourhoods: Seq[Neighbourhood]): Distribution = {
        val neighbourhoodIndex = neighbourhoods.iterator.zipWithIndex.toMap
        val hotSpotDistribution = Distribution(neighbourhoods.size)
        def involvedNeighbourhoods(x: AnyVariable) = {
            val xs = cc.space.involvedSearchVariables(x)
            def isInvolved(neighbourhood: Neighbourhood) = {
                val ys = neighbourhood.searchVariables.toSet
                xs.exists(ys.contains)
            }
            neighbourhoods.iterator.filter(isInvolved).map(neighbourhoodIndex).toIndexedSeq
        }
        val involvementMatrix = cc.costVars.iterator.map(x => (x, involvedNeighbourhoods(x))).filter(_._2.nonEmpty).toMap
        cc.space.post(new SatisfactionGoalTracker(cc.space.nextConstraintId(), None, involvementMatrix, hotSpotDistribution))
        hotSpotDistribution
    }

    protected def createMinimizationNeighbourhood
        [V <: NumericalValue[V]]
        (levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (using valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        createNeighbourhoodOnInvolvedSearchVariables(levelCfg, x)
    }

    protected def createMaximizationNeighbourhood
        [V <: NumericalValue[V]]
        (levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (using valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        createNeighbourhoodOnInvolvedSearchVariables(levelCfg, x)
    }

    private def createNeighbourhoodOnInvolvedSearchVariables
        (levelCfg: FlatZincLevelConfiguration, x: AnyVariable):
        Option[Neighbourhood] =
    {
        val xs0 = if (cc.space.isSearchVariable(x)) Set(x) else cc.space.involvedSearchVariables(x)
        val xs = xs0.diff(cc.implicitlyConstrainedVars)
        if (xs.isEmpty) {
            None
        } else {
            cc.logger.logg("Adding a neighbourhood over %s".format(xs))
            Some(new RandomReassignmentGenerator(
                cc.space, xs.toBuffer.sorted.toIndexedSeq, randomGenerator, cc.cfg.moveSizeDistribution, None, None))
        }
    }

    private def stackNeighbourhoods
        (objectives: immutable.IndexedSeq[PrimitiveObjective],
         neighbourhoods: immutable.IndexedSeq[Neighbourhood]):
        Neighbourhood =
    {
        require(objectives.size > 1)
        require(objectives.size == neighbourhoods.size)
        val hotSpotDistribution = new ArrayBackedDistribution(objectives.size)
        cc.space.post(new LevelWeightMaintainer(nextConstraintId(), objectives, hotSpotDistribution))
        new NeighbourhoodCollection(neighbourhoods, randomGenerator, Some(hotSpotDistribution), None)
    }

}
