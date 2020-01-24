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
    (cc: CompilationContext, randomGenerator: RandomGenerator, sigint: Sigint)
    extends NeighbourhoodFactory(cc, randomGenerator)
{

    protected override def createNeighbourhoodForMinimizationGoal
        [Value <: NumericalValue[Value]]
        (x: NumericalVariable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        val levelCfg = cfg.level1Configuration
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Min, levelCfg, x)
        else super.createNeighbourhoodForMinimizationGoal(x)
    }

    protected override def createNeighbourhoodForMaximizationGoal
        [Value <: NumericalValue[Value]]
        (x: NumericalVariable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        val levelCfg = cfg.level1Configuration
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Max, levelCfg, x)
        else super.createNeighbourhoodForMaximizationGoal(x)
    }

    private def createNeighbourhood
        [Value <: NumericalValue[Value]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        require(x != cc.costVar)
        space.registerObjectiveVariable(x)
        if (space.isDanglingVariable(x)) {
            mode match {
                case OptimizationMode.Min =>
                    // assign minimum value
                    super.createNeighbourhoodForMinimizationGoal(x)
                case OptimizationMode.Max =>
                    // assign maximum value
                    super.createNeighbourhoodForMaximizationGoal(x)
            }
        }
        else if (space.isSearchVariable(x)) {
            // x is unconstrained, so no need to add a generator for minimizing x.
            None
        }
        else if (space.isChannelVariable(x)) {
            createNeighbourhood(mode, levelCfg, space.definingConstraint(x).get)
        }
        else {
            // The thing that should not be.
            ???
        }
    }

    private def createNeighbourhood
        [Value <: NumericalValue[Value]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, constraint: yuck.core.Constraint)
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        (mode, constraint) match {
            case (OptimizationMode.Min, lc: LinearCombination[Value @ unchecked])
            if (lc.axs.forall(ax => if (ax.a < valueTraits.zero) ax.x.domain.hasUb else ax.x.domain.hasLb)) =>
                createNeighbourhood(mode, levelCfg, lc.axs)
            case (OptimizationMode.Max, lc: LinearCombination[Value @ unchecked])
            if (lc.axs.forall(ax => if (ax.a < valueTraits.zero) ax.x.domain.hasLb else ax.x.domain.hasUb)) =>
                createNeighbourhood(mode, levelCfg, lc.axs)
            case (OptimizationMode.Min, sum: Sum[Value @ unchecked])
            if (sum.xs.forall(x => x.domain.hasLb)) =>
                createNeighbourhood(mode, levelCfg, sum.xs.map(new AX(valueTraits.one, _)))
            case (OptimizationMode.Max, sum: Sum[Value @ unchecked])
            if (sum.xs.forall(x => x.domain.hasUb)) =>
                createNeighbourhood(mode, levelCfg, sum.xs.map(new AX(valueTraits.one, _)))
            case _ => {
                if (sigint.isSet) {
                    throw new FlatZincCompilerInterruptedException
                }
                val xs =
                    (space.involvedSearchVariables(constraint).diff(implicitlyConstrainedVars))
                        .toBuffer.sorted.toIndexedSeq
                if (xs.isEmpty) {
                    None
                } else {
                    for (x <- xs if ! x.domain.isFinite) {
                        throw new VariableWithInfiniteDomainException(x)
                    }
                    logger.logg("%s contributes a neighbourhood over %s".format(constraint, xs))
                    Some(new RandomReassignmentGenerator(space, xs, randomGenerator, cfg.moveSizeDistribution, None, None))
                }
            }
        }
    }

    private def createNeighbourhood
        [Value <: NumericalValue[Value]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, axs: Seq[AX[Value]])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        if (! axs.isEmpty && axs.forall(ax => space.isProblemParameter(ax.x) || space.isSearchVariable(ax.x))) {
            val weights = axs.filter(ax => space.isSearchVariable(ax.x) && ! implicitlyConstrainedVars.contains(ax.x))
            if (weights.isEmpty) {
                None
            } else {
                val xs = weights.iterator.map(_.x.asInstanceOf[AnyVariable]).toBuffer.sorted.toIndexedSeq
                for (x <- xs if ! x.domain.isFinite) {
                    throw new VariableWithInfiniteDomainException(x)
                }
                logger.logg("Adding a neighbourhood over %s".format(xs))
                Some(
                    new RandomReassignmentGenerator(
                        space, xs, randomGenerator, cfg.moveSizeDistribution,
                        Some(createHotSpotDistribution(mode, weights)), None))
            }
        } else {
            val weightedNeighbourhoods = new mutable.ArrayBuffer[(AX[Value], Neighbourhood)]
            for (ax <- axs) {
                if (sigint.isSet) {
                    throw new FlatZincCompilerInterruptedException
                }
                val maybeConstraint = space.definingConstraint(ax.x)
                if (maybeConstraint.isDefined) {
                    val maybeNeighbourhood = createNeighbourhood(mode, levelCfg, maybeConstraint.get)
                    if (maybeNeighbourhood.isDefined) {
                        weightedNeighbourhoods += ax -> maybeNeighbourhood.get
                    }
                } else if (! space.isProblemParameter(ax.x)) {
                    val maybeNeighbourhood =
                        if (implicitlyConstrainedVars.contains(ax.x)) {
                            None
                        } else {
                            logger.logg("Adding a neighbourhood over %s".format(ax.x))
                            Some(new SimpleRandomReassignmentGenerator(space, Vector(ax.x), randomGenerator))
                        }
                    if (maybeNeighbourhood.isDefined) {
                        weightedNeighbourhoods += ax -> maybeNeighbourhood.get
                    }
                }
            }
            val maybeNeighbourhood =
                if (weightedNeighbourhoods.size < 2) {
                    weightedNeighbourhoods.headOption.map(_._2)
                } else {
                    val (weights, neighbourhoods) = weightedNeighbourhoods.unzip
                    val hotSpotDistribution = createHotSpotDistribution(mode, weights)
                    Some(new NeighbourhoodCollection(neighbourhoods.toIndexedSeq, randomGenerator, Some(hotSpotDistribution), None))
                }
            maybeNeighbourhood.map(considerVariableChoiceRate(_, levelCfg))
        }
    }

    private def createHotSpotDistribution
        [Value <: NumericalValue[Value]]
        (mode: OptimizationMode.Value, weights: Seq[AX[Value]])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Distribution =
    {
        val hotSpotDistribution = DistributionFactory.createDistribution(weights.size)
        space.post(new OptimizationGoalTracker(nextConstraintId, null, mode, weights.toIndexedSeq, hotSpotDistribution))
        hotSpotDistribution
    }

    private def considerVariableChoiceRate
        (neighbourhood0: Neighbourhood, levelCfg: FlatZincLevelConfiguration): Neighbourhood =
    {
        val rate = (levelCfg.maybeFairVariableChoiceRate.getOrElse(Probability.from(0)).value * 100).toInt
        if (rate == 0) neighbourhood0
        else {
            val xs = (neighbourhood0.searchVariables.diff(implicitlyConstrainedVars)).toBuffer.sorted.toIndexedSeq
            if (xs.isEmpty) neighbourhood0
            else {
                val neighbourhood1 =
                    new RandomReassignmentGenerator(space, xs, randomGenerator, cfg.moveSizeDistribution, None, None)
                val distribution = DistributionFactory.createDistribution(0, List(100 - rate, rate))
                new NeighbourhoodCollection(Vector(neighbourhood0, neighbourhood1), randomGenerator, Some(distribution), None)
            }
        }
    }

}
