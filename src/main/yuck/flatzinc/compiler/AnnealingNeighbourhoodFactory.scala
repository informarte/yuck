package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.*
import yuck.constraints.SatisfactionGoalTracker.computeInvolvementMap
import yuck.core.*
import yuck.flatzinc.FlatZincLevelConfiguration
import yuck.util.Collections.*

/**
 * Generates focused annealing neighbourhoods for all types of goals.
 *
 * Candidates for implicit solving are handled with priority to avoid that their variables
 * get included into other neighbourhoods. (In case two candidates for implicit solving
 * compete for a variable, this conflict is resolved by random choice.)
 *
 * For constraint satisfaction, the factory produces a
 * [[yuck.core.RandomReassignmentGenerator RandomReassignmentGenerator]] instance
 * with a focus on search variables that are involved in constraint violations.
 * To guide the search, we use an instance of
 * [[SatisfactionGoalTracker SatisfactionGoalTracker]].
 *
 * The generation of neighbourhoods for optimization is driven by the constraints
 * that contribute to the objective value:
 * For each contributor, a neighbourhood is created.
 * The resulting neighbourhoods are collected and wrapped up in an instance of
 * [[yuck.core.NeighbourhoodCollection NeighbourhoodCollection]] such that the process
 * of move generation will comprise two steps: First some move neighbourhood will be
 * chosen and second the chosen neighbourhood will be asked to provide a move.
 * For guiding the first step in move generation, we use an instance of
 * [[yuck.constraints.OptimizationGoalTracker OptimizationGoalTracker]] to
 * keep track of each neighbourhood's potential to improve the objective value.
 *
 * In case we end up with two or more neighbourhoods, these will be stacked by creating
 * an instance of [[yuck.core.NeighbourhoodCollection NeighbourhoodCollection]] instrumented
 * to focus on the satisfaction goal in case hard constraints are violated.
 *
 * In an attempt to decouple this factory from implementation details of data structures
 * (hash sets, in particular) and the earlier compiler stages, we sort constraints and
 * variables (by id) before further processing.
 */
final class AnnealingNeighbourhoodFactory
    (override protected val cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase
{

    private val moveSizeDistribution = cc.cfg.annealingConfiguration.moveSizeDistribution

    private val neighbourhoodsFromImplicitConstraints = new mutable.ArrayBuffer[Neighbourhood]

    override def run() = {
        cc.maybeNeighbourhood = createNeighbourhood
    }

    private final def createNeighbourhood: Option[Neighbourhood] = {
        val buf = new mutable.ArrayBuffer[(PrimitiveObjective, Option[Neighbourhood])]
        for (objective, i) <- cc.objective.primitiveObjectives.zipWithIndex do {
            val levelCfg =
                if i == 0
                then cc.cfg.annealingConfiguration.topLevelConfiguration
                else cc.cfg.annealingConfiguration.subordinateLevelConfiguration
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
            (for case (objective, Some(neighbourhood)) <- buf yield (objective, neighbourhood)).unzip
        if neighbourhoods.size < 2
        then neighbourhoods.headOption
        else Some(stackNeighbourhoods(objectives.toVector, neighbourhoods.toVector))
    }

    protected def createSatisfactionNeighbourhood
        (levelCfg: FlatZincLevelConfiguration, x: BooleanVariable):
        Option[Neighbourhood] =
    {
        val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
        val candidatesForImplicitSolving =
            if cc.cfg.annealingConfiguration.useImplicitSolving && levelCfg.isTopLevel then {
                cc.costVarsFromRedundantConstraints.iterator.concat(Iterator.single(x))
                    .flatMap(findCandidatesForImplicitSolving).toBuffer.sorted.toIndexedSeq
            } else {
                Nil
            }
        val constraintHardness: Map[Class[? <: Constraint], Int] = Map(
            (classOf[BooleanIncreasing], 3), (classOf[Circuit], 3), (classOf[IntegerIncreasing], 3), (classOf[Inverse], 3),
            (classOf[Regular], 3),
            (classOf[AllDifferent[?, ?, ?]], 2),
            (classOf[Table[?, ?, ?]], 1))
        def constraintRanking(constraint: Constraint): Int =
            -(constraintHardness(constraint.getClass) * constraint.inVariables.size)
        for constraint <- randomGenerator.shuffle(candidatesForImplicitSolving).sortBy(constraintRanking) do {
            val xs = constraint.inVariables.toSet
            if (xs & cc.implicitlyConstrainedVars).isEmpty then {
                val (maybeNeighbourhood, _) = cc.logger.withTimedLogScope("Solving %s".format(constraint)) {
                    constraint.createNeighbourhood(
                        cc.space, randomGenerator, moveSizeDistribution,
                        createHotSpotDistribution = xs => Some(createHotSpotDistribution(xs, cc.costVars)),
                        maybeFairVariableChoiceRate = levelCfg.maybeFairVariableChoiceRate)
                }
                if maybeNeighbourhood.isDefined then {
                    cc.implicitlyConstrainedVars ++= xs
                    cc.space.registerImplicitConstraint(constraint)
                    cc.logger.log("Adding a neighbourhood for implicit constraint %s".format(constraint))
                    neighbourhoods += maybeNeighbourhood.get
                    neighbourhoodsFromImplicitConstraints += maybeNeighbourhood.get
                }
            }
        }
        val xs = cc.space.involvedSearchVariables(x).diff(cc.implicitlyConstrainedVars).toBuffer.sorted.toVector
        if ! xs.isEmpty && ! cc.sigint.isSet then {
            for x <- xs if ! x.domain.isFinite do {
                throw new VariableWithInfiniteDomainException(x)
            }
            cc.logger.logg("Adding a neighbourhood over %s".format(xs))
            val maybeCostVars =
                if levelCfg.isTopLevel
                then Some(cc.costVars)
                else if cc.space.maybeDefiningConstraint(x).exists(_.isInstanceOf[Conjunction])
                then Some(cc.space.definingConstraint(x).asInstanceOf[Conjunction].xs.toSet)
                else None
            neighbourhoods +=
                new RandomReassignmentGenerator(
                    cc.space, xs, randomGenerator, moveSizeDistribution,
                    if levelCfg.guideOptimization && maybeCostVars.isDefined
                    then Some(createHotSpotDistribution(xs, maybeCostVars.get))
                    else None,
                    if levelCfg.guideOptimization then levelCfg.maybeFairVariableChoiceRate else None)
        }
        if neighbourhoods.size < 2
        then neighbourhoods.headOption
        else {
            Some(new NeighbourhoodCollection(
                cc.space,
                neighbourhoods.toVector, randomGenerator,
                maybeSelectionSizeDistribution(neighbourhoods),
                if levelCfg.guideOptimization then Some(createHotSpotDistribution(neighbourhoods)) else None,
                if levelCfg.guideOptimization then levelCfg.maybeFairVariableChoiceRate else None))
        }
    }

    private def findCandidatesForImplicitSolving(x: BooleanVariable): Iterator[Constraint] = {
        val maybeConstraint = cc.space.maybeDefiningConstraint(x)
        if maybeConstraint.isDefined then {
            val constraint = maybeConstraint.get
            if constraint.isInstanceOf[Conjunction] || constraint.isInstanceOf[And]
            then
                constraint.inVariables.iterator
                    .flatMap(y => findCandidatesForImplicitSolving(y.asInstanceOf[BooleanVariable]))
            else if constraint.isCandidateForImplicitSolving(cc.space)
            then Iterator.single(constraint)
            else Iterator.empty
        } else {
            Iterator.empty
        }
    }

    protected def createMinimizationNeighbourhood
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (levelCfg: FlatZincLevelConfiguration, x: X)
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        Option[Neighbourhood] =
    {
        if levelCfg.guideOptimization
        then createNeighbourhood(OptimizationMode.Min, levelCfg, x)
        else createNeighbourhoodOnInvolvedSearchVariables(levelCfg, x)
    }

    protected def createMaximizationNeighbourhood
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (levelCfg: FlatZincLevelConfiguration, x: X)
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        Option[Neighbourhood] =
    {
        if levelCfg.guideOptimization
        then createNeighbourhood(OptimizationMode.Max, levelCfg, x)
        else createNeighbourhoodOnInvolvedSearchVariables(levelCfg, x)
    }

    private def createNeighbourhood
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (mode: OptimizationMode, levelCfg: FlatZincLevelConfiguration, x: X)
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        Option[Neighbourhood] =
    {
        cc.space.registerObjectiveVariable(x)
        if cc.space.isProblemParameter(x) then {
            None
        } else if cc.space.isDanglingVariable(x) then {
            mode match {
                case OptimizationMode.Min =>
                    // assign minimum value
                    createNeighbourhoodOnInvolvedSearchVariables(levelCfg, x)
                case OptimizationMode.Max =>
                    // assign maximum value
                    createNeighbourhoodOnInvolvedSearchVariables(levelCfg, x)
            }
        }
        else if cc.space.isSearchVariable(x) then {
            // x is unconstrained, so no need to add a generator for minimizing x.
            None
        }
        else if cc.space.isChannelVariable(x) then {
            createNeighbourhood(mode, levelCfg, cc.space.definingConstraint(x))
        }
        else {
            // The thing that should not be.
            ???
        }
    }

    private def createNeighbourhood
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (mode: OptimizationMode, levelCfg: FlatZincLevelConfiguration, constraint: yuck.core.Constraint)
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        Option[Neighbourhood] =
    {
        (mode, constraint) match {
            case (OptimizationMode.Min, lc: LinearCombination[A @ unchecked, D @ unchecked, X @ unchecked])
            if lc.axs.forall(ax => if ax.a < typeTraits.zero then ax.x.domain.hasUb else ax.x.domain.hasLb) =>
                createNeighbourhood(mode, levelCfg, lc.axs)
            case (OptimizationMode.Max, lc: LinearCombination[A @ unchecked, D @ unchecked, X @ unchecked])
            if lc.axs.forall(ax => if ax.a < typeTraits.zero then ax.x.domain.hasLb else ax.x.domain.hasUb) =>
                createNeighbourhood(mode, levelCfg, lc.axs)
            case (OptimizationMode.Min, sum: Sum[A @ unchecked, D @ unchecked, X @ unchecked])
            if sum.xs.forall(x => x.domain.hasLb) =>
                createNeighbourhood(mode, levelCfg, sum.xs.map(new AX(typeTraits.one, _)))
            case (OptimizationMode.Max, sum: Sum[A @ unchecked, D @ unchecked, X @ unchecked])
            if sum.xs.forall(x => x.domain.hasUb) =>
                createNeighbourhood(mode, levelCfg, sum.xs.map(new AX(typeTraits.one, _)))
            case (OptimizationMode.Min, bool2Costs: Bool2Costs1) =>
                createSatisfactionNeighbourhood(levelCfg, bool2Costs.inVariables.head.asInstanceOf[BooleanVariable])
            case _ =>
                val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
                val xs0 = cc.space.involvedSearchVariables(constraint)
                neighbourhoods ++=
                    neighbourhoodsFromImplicitConstraints.iterator.filter(_.searchVariables.intersect(xs0).nonEmpty)
                val xs = xs0.diff(cc.implicitlyConstrainedVars).toBuffer.sorted.toVector
                if xs.isEmpty then {
                    // Either there are no variables or they are all managed by neighbourhoods from implicit constraints.
                    None
                } else {
                    for x <- xs if ! x.domain.isFinite do {
                        throw new VariableWithInfiniteDomainException(x)
                    }
                    cc.logger.log("%s contributes a neighbourhood over %s".format(constraint, xs))
                        neighbourhoods +=
                            new RandomReassignmentGenerator(cc.space, xs, randomGenerator, moveSizeDistribution)
                    if neighbourhoods.size < 2 then {
                        neighbourhoods.headOption
                    } else {
                        Some(new NeighbourhoodCollection(
                            cc.space,
                            neighbourhoods.toVector, randomGenerator,
                            maybeSelectionSizeDistribution(neighbourhoods), None, None))
                    }
                }
        }
    }

    private def createNeighbourhood
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (mode: OptimizationMode, levelCfg: FlatZincLevelConfiguration, axs0: Seq[AX[A, D, X]])
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        Option[Neighbourhood] =
    {
        val axs = axs0.sortBy(_.x)
        if axs.forall(ax => cc.space.isProblemParameter(ax.x) ||
            (cc.space.isSearchVariable(ax.x) && ! cc.implicitlyConstrainedVars.contains(ax.x))) then
        {
            val weights = axs.filter(ax => cc.space.isSearchVariable(ax.x))
            if weights.isEmpty then {
                None
            } else {
                val xs = weights.map(_.x).toVector
                for x <- xs if ! x.domain.isFinite do {
                    throw new VariableWithInfiniteDomainException(x)
                }
                cc.logger.logg("Adding a neighbourhood over %s".format(xs))
                val hotSpotDistribution = createHotSpotDistribution(mode, weights)
                Some(new RandomReassignmentGenerator(
                    cc.space, xs, randomGenerator, moveSizeDistribution, Some(hotSpotDistribution)))
            }
        } else {
            val weightedNeighbourhoods = new mutable.ArrayBuffer[(AX[A, D, X], Neighbourhood)]
            for ax <- axs do {
                if cc.sigint.isSet then {
                    throw new FlatZincCompilerInterruptedException
                }
                val maybeNeighbourhood =
                    if cc.space.isChannelVariable(ax.x)
                    then createNeighbourhood(mode, levelCfg, cc.space.definingConstraint(ax.x))
                    else if cc.space.isProblemParameter(ax.x)
                    then None
                    else if cc.implicitlyConstrainedVars.contains(ax.x)
                    then neighbourhoodsFromImplicitConstraints.find(_.searchVariables.contains(ax.x))
                    else {
                        cc.logger.logg("Adding a neighbourhood over %s".format(ax.x))
                        Some(new RandomReassignmentGenerator(cc.space, Vector(ax.x), randomGenerator))
                    }
                if maybeNeighbourhood.isDefined then {
                    weightedNeighbourhoods += ax -> maybeNeighbourhood.get
                }
            }
            val maybeNeighbourhood =
                if weightedNeighbourhoods.size < 2
                then weightedNeighbourhoods.headOption.map(_._2)
                else {
                    val (weights, neighbourhoods) = weightedNeighbourhoods.unzip
                    val hotSpotDistribution = createHotSpotDistribution(mode, weights)
                    Some(new NeighbourhoodCollection(
                        cc.space,
                        neighbourhoods.toVector, randomGenerator,
                        maybeSelectionSizeDistribution(neighbourhoods),
                        Some(hotSpotDistribution), levelCfg.maybeFairVariableChoiceRate))
                }
            maybeNeighbourhood
        }
    }

    private def createNeighbourhoodOnInvolvedSearchVariables
        (levelCfg: FlatZincLevelConfiguration, x: AnyVariable):
        Option[Neighbourhood] =
    {
        val xs0 = if cc.space.isSearchVariable(x) then Set(x) else cc.space.involvedSearchVariables(x)
        val xs = xs0.diff(cc.implicitlyConstrainedVars)
        if xs.isEmpty
        then None
        else {
            cc.logger.logg("Adding a neighbourhood over %s".format(xs))
            Some(new RandomReassignmentGenerator(
                cc.space, xs.toBuffer.sorted.toVector, randomGenerator, moveSizeDistribution))
        }
    }

    private def createHotSpotDistribution(xs: IndexedSeq[AnyVariable], cs: Iterable[BooleanVariable]): Distribution = {
        val involvementMap = computeInvolvementMap(cc.space, xs, cs)
        val hotSpotDistribution = Distribution(xs.size)
        cc.post(new SatisfactionGoalTracker(cc.space.nextConstraintId(), None, involvementMap, hotSpotDistribution))
        hotSpotDistribution
    }

    private def createHotSpotDistribution(neighbourhoods: Seq[Neighbourhood]): Distribution = {
        val neighbourhoodScopes = neighbourhoods.iterator.map(_.searchVariables.toSet).toVector
        def involvedNeighbourhoods(x: AnyVariable): IntArraySeq = {
            val xs = cc.space.involvedSearchVariables(x)
            def isInvolved(i: Int) = xs.exists(neighbourhoodScopes(i).contains)
            neighbourhoods.indices.stream.filter(isInvolved).toArraySeq
        }
        val involvementMap = cc.costVars.iterator.map(x => (x, involvedNeighbourhoods(x))).toMap
        val hotSpotDistribution = Distribution(neighbourhoods.size)
        cc.post(new SatisfactionGoalTracker(cc.space.nextConstraintId(), None, involvementMap, hotSpotDistribution))
        hotSpotDistribution
    }

    private def stackNeighbourhoods
        (objectives: immutable.IndexedSeq[PrimitiveObjective],
         neighbourhoods: immutable.IndexedSeq[Neighbourhood]):
        Neighbourhood =
    {
        require(objectives.size > 1)
        require(objectives.size == neighbourhoods.size)
        val hotSpotDistribution = new ArrayBackedDistribution(objectives.size)
        cc.post(new LevelWeightMaintainer(nextConstraintId(), objectives, hotSpotDistribution))
        new NeighbourhoodCollection(cc.space, neighbourhoods, randomGenerator, None, Some(hotSpotDistribution), None)
    }

    private def createHotSpotDistribution
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (mode: OptimizationMode, weights: Seq[AX[A, D, X]])
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        Distribution =
    {
        val hotSpotDistribution = Distribution(weights.size)
        cc.post(new OptimizationGoalTracker(nextConstraintId(), None, mode, weights.toVector, hotSpotDistribution))
        hotSpotDistribution
    }

    protected def maybeSelectionSizeDistribution(neighbourhoods: Iterable[Neighbourhood]): Option[Distribution] = {
        val searchVariables = neighbourhoods.iterator.flatMap(_.searchVariables).toSet
        val neighbourhoodsAreDisjoint = searchVariables.size == neighbourhoods.iterator.map(_.searchVariables.size).sum
        if neighbourhoodsAreDisjoint then Some(moveSizeDistribution) else None
    }

}
