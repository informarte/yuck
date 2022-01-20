package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.{OptimizationGoalTracker, LinearCombination, Sum}
import yuck.core._
import yuck.flatzinc.FlatZincLevelConfiguration
import yuck.util.arm.Sigint

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
     override protected val randomGenerator: RandomGenerator,
     sigint: Sigint)
    extends NeighbourhoodFactory
{

    protected override def createMinimizationNeighbourhood
        [V <: NumericalValue[V]]
        (x: NumericalVariable[V])
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        val levelCfg = cfg.level1Configuration
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Min, levelCfg, x)
        else super.createMinimizationNeighbourhood(x)
    }

    protected override def createMaximizationNeighbourhood
        [V <: NumericalValue[V]]
        (x: NumericalVariable[V])
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        val levelCfg = cfg.level1Configuration
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Max, levelCfg, x)
        else super.createMaximizationNeighbourhood(x)
    }

    private def createNeighbourhood
        [V <: NumericalValue[V]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        space.registerObjectiveVariable(x)
        if (space.isProblemParameter(x)) {
            None
        } else if (space.isDanglingVariable(x)) {
            mode match {
                case OptimizationMode.Min =>
                    // assign minimum value
                    super.createMinimizationNeighbourhood(x)
                case OptimizationMode.Max =>
                    // assign maximum value
                    super.createMaximizationNeighbourhood(x)
            }
        }
        else if (space.isSearchVariable(x)) {
            // x is unconstrained, so no need to add a generator for minimizing x.
            None
        }
        else if (space.isChannelVariable(x)) {
            createNeighbourhood(mode, levelCfg, space.definingConstraint(x))
        }
        else {
            // The thing that should not be.
            ???
        }
    }

    private def createNeighbourhood
        [V <: NumericalValue[V]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, constraint: yuck.core.Constraint)
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        (mode, constraint) match {
            case (OptimizationMode.Min, lc: LinearCombination[V @ unchecked])
            if (lc.axs.forall(ax => if (ax.a < valueTraits.zero) ax.x.domain.hasUb else ax.x.domain.hasLb)) =>
                createNeighbourhood(mode, levelCfg, lc.axs)
            case (OptimizationMode.Max, lc: LinearCombination[V @ unchecked])
            if (lc.axs.forall(ax => if (ax.a < valueTraits.zero) ax.x.domain.hasLb else ax.x.domain.hasUb)) =>
                createNeighbourhood(mode, levelCfg, lc.axs)
            case (OptimizationMode.Min, sum: Sum[V @ unchecked])
            if (sum.xs.forall(x => x.domain.hasLb)) =>
                createNeighbourhood(mode, levelCfg, sum.xs.map(new AX(valueTraits.one, _)))
            case (OptimizationMode.Max, sum: Sum[V @ unchecked])
            if (sum.xs.forall(x => x.domain.hasUb)) =>
                createNeighbourhood(mode, levelCfg, sum.xs.map(new AX(valueTraits.one, _)))
            case _ => {
                val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
                val xs0 = space.involvedSearchVariables(constraint)
                neighbourhoods ++=
                    neighbourhoodsFromImplicitConstraints.iterator.filter(_.searchVariables.intersect(xs0).nonEmpty)
                val xs = xs0.diff(implicitlyConstrainedVars).toBuffer.sorted.toIndexedSeq
                if (xs.isEmpty) {
                    // Either there are no variables or they are all managed by neighbourhoods from implicit constraints.
                    None
                } else {
                    for (x <- xs if ! x.domain.isFinite) {
                        throw new VariableWithInfiniteDomainException(x)
                    }
                    logger.logg("%s contributes a neighbourhood over %s".format(constraint, xs))
                        neighbourhoods +=
                            new RandomReassignmentGenerator(space, xs, randomGenerator, cfg.moveSizeDistribution, None, None)
                    if (neighbourhoods.size < 2) {
                        neighbourhoods.headOption
                    } else {
                        Some(new NeighbourhoodCollection(neighbourhoods.toIndexedSeq, randomGenerator, None, None))
                    }
                }
            }
        }
    }

    private def createNeighbourhood
        [V <: NumericalValue[V]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, axs0: Seq[AX[V]])
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        val axs = axs0.sortBy(_.x)
        if (axs.forall(ax => space.isProblemParameter(ax.x) ||
            (space.isSearchVariable(ax.x) && ! implicitlyConstrainedVars.contains(ax.x))))
        {
            val weights = axs.filter(ax => space.isSearchVariable(ax.x))
            if (weights.isEmpty) {
                None
            } else {
                val xs = weights.map(_.x).toIndexedSeq
                for (x <- xs if ! x.domain.isFinite) {
                    throw new VariableWithInfiniteDomainException(x)
                }
                logger.logg("Adding a neighbourhood over %s".format(xs))
                val hotSpotDistribution = createHotSpotDistribution(mode, weights)
                Some(new RandomReassignmentGenerator(
                    space, xs, randomGenerator, cfg.moveSizeDistribution, Some(hotSpotDistribution), None))
            }
        } else {
            val weightedNeighbourhoods = new mutable.ArrayBuffer[(AX[V], Neighbourhood)]
            for (ax <- axs) {
                if (sigint.isSet) {
                    throw new FlatZincCompilerInterruptedException
                }
                val maybeNeighbourhood = {
                    if (space.isChannelVariable(ax.x)) {
                        createNeighbourhood(mode, levelCfg, space.definingConstraint(ax.x))
                    } else if (space.isProblemParameter(ax.x)) {
                        None
                    } else if (implicitlyConstrainedVars.contains(ax.x)) {
                        neighbourhoodsFromImplicitConstraints.find(_.searchVariables.contains(ax.x))
                    } else {
                        logger.logg("Adding a neighbourhood over %s".format(ax.x))
                        Some(new SimpleRandomReassignmentGenerator(space, Vector(ax.x), randomGenerator))
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
                        neighbourhoods.toIndexedSeq, randomGenerator, Some(hotSpotDistribution), None))
                }
            maybeNeighbourhood.map(considerFairVariableChoiceRate(_, levelCfg))
        }
    }

    private def createHotSpotDistribution
        [V <: NumericalValue[V]]
        (mode: OptimizationMode.Value, weights: Seq[AX[V]])
        (implicit valueTraits: NumericalValueTraits[V]):
        Distribution =
    {
        val hotSpotDistribution = Distribution(weights.size)
        space.post(new OptimizationGoalTracker(nextConstraintId, null, mode, weights.toIndexedSeq, hotSpotDistribution))
        hotSpotDistribution
    }

    private def considerFairVariableChoiceRate
        (neighbourhood0: Neighbourhood, levelCfg: FlatZincLevelConfiguration): Neighbourhood =
    {
        val rate = (levelCfg.maybeFairVariableChoiceRate.getOrElse(Probability(0)).value * 100).toInt
        if (rate == 0) neighbourhood0
        else {
            val xs = (neighbourhood0.searchVariables.diff(implicitlyConstrainedVars)).toBuffer.sorted.toIndexedSeq
            if (xs.isEmpty) neighbourhood0
            else {
                val neighbourhood1 =
                    new RandomReassignmentGenerator(space, xs, randomGenerator, cfg.moveSizeDistribution, None, None)
                val distribution = Distribution(0, List(100 - rate, rate))
                new NeighbourhoodCollection(Vector(neighbourhood0, neighbourhood1), randomGenerator, Some(distribution), None)
            }
        }
    }

}
