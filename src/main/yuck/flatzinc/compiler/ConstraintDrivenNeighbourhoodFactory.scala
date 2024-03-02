package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.{LinearCombination, OptimizationGoalTracker, Sum}
import yuck.core.*
import yuck.flatzinc.FlatZincLevelConfiguration

/**
 * Generates focused neighbourhoods for all types of goals.
 *
 * Satisfaction goals are handled by the superclass.
 *
 * The generation of neighbourhoods for optimization is driven by the constraints
 * that contribute to the objective value:
 * For each constraint, a neighbourhood is created. The resulting neighbourhoods are
 * collected and wrapped up in an instance of [[yuck.core.NeighbourhoodCollection
 * NeighbourhoodCollection]] such that the process of move generation will comprise
 * two steps: First some move neighbourhood will be chosen and second the chosen
 * neighbourhood will be asked to provide a move.
 *
 * For guiding the first step in move generation, we use an instance of
 * [[yuck.constraints.OptimizationGoalTracker OptimizationGoalTracker]] to
 * keep track of each constraint's potential to improve the objective value.
 *
 * @author Michael Marte
 */
final class ConstraintDrivenNeighbourhoodFactory
    (override protected val cc: CompilationContext,
     override protected val randomGenerator: RandomGenerator)
    extends NeighbourhoodFactory
{

    override protected def createMinimizationNeighbourhood
        [V <: NumericalValue[V]]
        (levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (using valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Min, levelCfg, x)
        else super.createMinimizationNeighbourhood(levelCfg, x)
    }

    override protected def createMaximizationNeighbourhood
        [V <: NumericalValue[V]]
        (levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (using valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Max, levelCfg, x)
        else super.createMaximizationNeighbourhood(levelCfg, x)
    }

    private def createNeighbourhood
        [V <: NumericalValue[V]]
        (mode: OptimizationMode, levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (using valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        cc.space.registerObjectiveVariable(x)
        if (cc.space.isProblemParameter(x)) {
            None
        } else if (cc.space.isDanglingVariable(x)) {
            mode match {
                case OptimizationMode.Min =>
                    // assign minimum value
                    super.createMinimizationNeighbourhood(levelCfg, x)
                case OptimizationMode.Max =>
                    // assign maximum value
                    super.createMaximizationNeighbourhood(levelCfg, x)
            }
        }
        else if (cc.space.isSearchVariable(x)) {
            // x is unconstrained, so no need to add a generator for minimizing x.
            None
        }
        else if (cc.space.isChannelVariable(x)) {
            createNeighbourhood(mode, levelCfg, cc.space.definingConstraint(x))
        }
        else {
            // The thing that should not be.
            ???
        }
    }

    private def createNeighbourhood
        [V <: NumericalValue[V]]
        (mode: OptimizationMode, levelCfg: FlatZincLevelConfiguration, constraint: yuck.core.Constraint)
        (using valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        (mode, constraint) match {
            case (OptimizationMode.Min, lc: LinearCombination[V @ unchecked])
            if lc.axs.forall(ax => if (ax.a < valueTraits.zero) ax.x.domain.hasUb else ax.x.domain.hasLb) =>
                createNeighbourhood(mode, levelCfg, lc.axs)
            case (OptimizationMode.Max, lc: LinearCombination[V @ unchecked])
            if lc.axs.forall(ax => if (ax.a < valueTraits.zero) ax.x.domain.hasLb else ax.x.domain.hasUb) =>
                createNeighbourhood(mode, levelCfg, lc.axs)
            case (OptimizationMode.Min, sum: Sum[V @ unchecked])
            if sum.xs.forall(x => x.domain.hasLb) =>
                createNeighbourhood(mode, levelCfg, sum.xs.map(new AX(valueTraits.one, _)))
            case (OptimizationMode.Max, sum: Sum[V @ unchecked])
            if sum.xs.forall(x => x.domain.hasUb) =>
                createNeighbourhood(mode, levelCfg, sum.xs.map(new AX(valueTraits.one, _)))
            case _ =>
                val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
                val xs0 = cc.space.involvedSearchVariables(constraint)
                neighbourhoods ++=
                    neighbourhoodsFromImplicitConstraints.iterator.filter(_.searchVariables.intersect(xs0).nonEmpty)
                val xs = xs0.diff(cc.implicitlyConstrainedVars).toBuffer.sorted.toVector
                if (xs.isEmpty) {
                    // Either there are no variables or they are all managed by neighbourhoods from implicit constraints.
                    None
                } else {
                    for (x <- xs if ! x.domain.isFinite) {
                        throw new VariableWithInfiniteDomainException(x)
                    }
                    cc.logger.log("%s contributes a neighbourhood over %s".format(constraint, xs))
                        neighbourhoods +=
                            new RandomReassignmentGenerator(
                                cc.space, xs, randomGenerator, cc.cfg.moveSizeDistribution, None, None)
                    if (neighbourhoods.size < 2) {
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
        [V <: NumericalValue[V]]
        (mode: OptimizationMode, levelCfg: FlatZincLevelConfiguration, axs0: Seq[AX[V]])
        (using valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        val axs = axs0.sortBy(_.x)
        if (axs.forall(ax => cc.space.isProblemParameter(ax.x) ||
            (cc.space.isSearchVariable(ax.x) && ! cc.implicitlyConstrainedVars.contains(ax.x))))
        {
            val weights = axs.filter(ax => cc.space.isSearchVariable(ax.x))
            if (weights.isEmpty) {
                None
            } else {
                val xs = weights.map(_.x).toVector
                for (x <- xs if ! x.domain.isFinite) {
                    throw new VariableWithInfiniteDomainException(x)
                }
                cc.logger.logg("Adding a neighbourhood over %s".format(xs))
                val hotSpotDistribution = createHotSpotDistribution(mode, weights)
                Some(new RandomReassignmentGenerator(
                    cc.space, xs, randomGenerator, cc.cfg.moveSizeDistribution, Some(hotSpotDistribution), None))
            }
        } else {
            val weightedNeighbourhoods = new mutable.ArrayBuffer[(AX[V], Neighbourhood)]
            for (ax <- axs) {
                if (cc.sigint.isSet) {
                    throw new FlatZincCompilerInterruptedException
                }
                val maybeNeighbourhood = {
                    if (cc.space.isChannelVariable(ax.x)) {
                        createNeighbourhood(mode, levelCfg, cc.space.definingConstraint(ax.x))
                    } else if (cc.space.isProblemParameter(ax.x)) {
                        None
                    } else if (cc.implicitlyConstrainedVars.contains(ax.x)) {
                        neighbourhoodsFromImplicitConstraints.find(_.searchVariables.contains(ax.x))
                    } else {
                        cc.logger.logg("Adding a neighbourhood over %s".format(ax.x))
                        Some(new SimpleRandomReassignmentGenerator(cc.space, Vector(ax.x), randomGenerator))
                    }
                }
                if (maybeNeighbourhood.isDefined) {
                    weightedNeighbourhoods += ax -> maybeNeighbourhood.get
                }
            }
            val maybeNeighbourhood =
                if (weightedNeighbourhoods.size < 2) {
                    weightedNeighbourhoods.headOption.map(_._2)
                } else {
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

    private def createHotSpotDistribution
        [V <: NumericalValue[V]]
        (mode: OptimizationMode, weights: Seq[AX[V]])
        (using valueTraits: NumericalValueTraits[V]):
        Distribution =
    {
        val hotSpotDistribution = Distribution(weights.size)
        cc.space.post(new OptimizationGoalTracker(nextConstraintId(), None, mode, weights.toVector, hotSpotDistribution))
        hotSpotDistribution
    }

}
