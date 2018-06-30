package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.{DistributionMaintainer, LinearCombination, Sum}
import yuck.core._
import yuck.flatzinc.FlatZincLevelConfiguration
import yuck.util.arm.Sigint

/**
 * Generates focused neighbourhoods for satisfaction and minimization goals.
 *
 * The generation is driven by the constraints that contribute to the goal: For each one,
 * a neighbourhood is created. The resulting neighbourhoods are collected and
 * wrapped up in an instance of [[yuck.core.NeighbourhoodCollection NeighbourhoodCollection]]
 * such that the process of move generation will comprise two steps: First some move
 * neighbourhood will be chosen and second the chosen neighbourhood will be asked to provide a move.

 * To focus the search on violated constraints, we keep track of (weighted) constraint
 * violations by means of a dynamic distribution (maintained by an instance of
 * [[yuck.constraints.DistributionMaintainer DistributionMaintainer]]) for use in the
 * first step of move generation.
 *
 * Candidates for implicit solving are handled with priority to avoid that their variables
 * get included into other neighbourhoods. (In case two candidates for implicit solving
 * compete for a variable, this conflict is resolved by random choice.) This special
 * treatment is implemented using laziness to a certain extent: The procedure creates a
 * factory that already contains the neighbourhoods for implicit constraints while the
 * remaining neighbourhoods get generated on demand.
 *
 * @author Michael Marte
 */
final class ConstraintDrivenNeighbourhoodFactory
    (cc: CompilationContext, randomGenerator: RandomGenerator, sigint: Sigint)
    extends NeighbourhoodFactory(cc, randomGenerator)
{

    protected override def createNeighbourhoodForSatisfactionGoal(x: Variable[BooleanValue]) = {
        val levelCfg = cfg.level0Configuration
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Min, levelCfg, x)
        else super.createNeighbourhoodForSatisfactionGoal(x)
    }

    protected override def createNeighbourhoodForMinimizationGoal
        [Value <: NumericalValue[Value]]
        (x: Variable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        val levelCfg = cfg.level1Configuration
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Min, levelCfg, x)
        else super.createNeighbourhoodForMinimizationGoal(x)
    }

    protected override def createNeighbourhoodForMaximizationGoal
        [Value <: NumericalValue[Value]]
        (x: Variable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
        val levelCfg = cfg.level1Configuration
        if (levelCfg.guideOptimization) createNeighbourhood(OptimizationMode.Max, levelCfg, x)
        else super.createNeighbourhoodForMaximizationGoal(x)
    }

    private def createNeighbourhood
        [Value <: NumericalValue[Value]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, x: Variable[Value])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Option[Neighbourhood] =
    {
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
            createNeighbourhoodFactory(mode, levelCfg, space.definingConstraint(x).get)
                .apply.map(_.neighbourhood)
        }
        else {
            // The thing that should not be.
            ???
        }
    }

    private abstract class AnnotatedNeighbourhood {
        val neighbourhood: Neighbourhood
    }
    private case class ImplicitConstraintMaintainer(override val neighbourhood: Neighbourhood) extends AnnotatedNeighbourhood
    private case class OtherNeighbourhood(override val neighbourhood: Neighbourhood) extends AnnotatedNeighbourhood

    private def domain
        [Value <: OrderedValue[Value]]
        (x: Variable[Value])
        (implicit valueTraits: OrderedValueTraits[Value]):
        OrderedDomain[Value] =
        valueTraits.safeDowncast(x.domain)

    private def createNeighbourhoodFactory
        [Value <: NumericalValue[Value]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, constraint: yuck.core.Constraint)
        (implicit valueTraits: NumericalValueTraits[Value]):
        () => Option[AnnotatedNeighbourhood] =
    {
        (mode, constraint) match {
            case (OptimizationMode.Min, lc: LinearCombination[Value @ unchecked])
            if (lc.axs.forall(ax => if (ax.a < valueTraits.zero) domain(ax.x).hasUb else domain(ax.x).hasLb)) =>
                createNeighbourhoodFactory(mode, levelCfg, lc.axs)
            case (OptimizationMode.Max, lc: LinearCombination[Value @ unchecked])
            if (lc.axs.forall(ax => if (ax.a < valueTraits.zero) domain(ax.x).hasLb else domain(ax.x).hasUb)) =>
                createNeighbourhoodFactory(mode, levelCfg, lc.axs)
            case (OptimizationMode.Min, sum: Sum[Value @ unchecked])
            if (sum.xs.forall(x => domain(x).hasLb)) =>
                createNeighbourhoodFactory(mode, levelCfg, sum.xs.map(new AX(valueTraits.one, _)))
            case (OptimizationMode.Max, sum: Sum[Value @ unchecked])
            if (sum.xs.forall(x => domain(x).hasUb)) =>
                createNeighbourhoodFactory(mode, levelCfg, sum.xs.map(new AX(valueTraits.one, _)))
            case _ => {
                val xs = constraint.inVariables.toSet
                val maybeNeighbourhood =
                    if (constraint.isCandidateForImplicitSolving(space) && (xs & implicitlyConstrainedVars).isEmpty) {
                        constraint.prepareForImplicitSolving(space, randomGenerator, cfg.moveSizeDistribution, _ => None, None, sigint)
                    } else {
                        None
                    }
                if (maybeNeighbourhood.isDefined) {
                    implicitlyConstrainedVars ++= xs
                    space.markAsImplicit(constraint)
                    () => {
                        logger.logg("Adding a neighbourhood for implicit constraint %s".format(constraint))
                        maybeNeighbourhood.map(ImplicitConstraintMaintainer)
                    }
                } else () => {
                    if (sigint.isSet) {
                        throw new FlatZincCompilerInterruptedException
                    }
                    val xs =
                        (space.involvedSearchVariables(constraint) -- implicitlyConstrainedVars)
                        .toBuffer.sorted.toIndexedSeq
                    if (xs.isEmpty) {
                        None
                    } else {
                        for (x <- xs if ! x.domain.isFinite) {
                            throw new VariableWithInfiniteDomainException(x)
                        }
                        logger.logg("%s contributes a neighbourhood over %s".format(constraint, xs))
                        Some(OtherNeighbourhood(
                            new RandomReassignmentGenerator(space, xs, randomGenerator, cfg.moveSizeDistribution, None, None)))

                    }
                }
            }
        }
    }

    private def createNeighbourhoodFactory
        [Value <: NumericalValue[Value]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, axs: Seq[AX[Value]])
        (implicit valueTraits: NumericalValueTraits[Value]):
        () => Option[AnnotatedNeighbourhood] =
    {
        if (! axs.isEmpty && axs.forall(ax => ax.x.isParameter || space.isSearchVariable(ax.x))) {
            () => {
                val weights = axs.filter(ax => ! ax.x.isParameter && ! implicitlyConstrainedVars.contains(ax.x))
                if (weights.isEmpty) {
                    None
                } else {
                    val xs = weights.toIterator.map(_.x.asInstanceOf[AnyVariable]).toBuffer.sorted.toIndexedSeq
                    for (x <- xs if ! x.domain.isFinite) {
                        throw new VariableWithInfiniteDomainException(x)
                    }
                    logger.logg("Adding a neighbourhood over %s".format(xs))
                    Some(OtherNeighbourhood(
                        new RandomReassignmentGenerator(
                            space, xs, randomGenerator, cfg.moveSizeDistribution,
                            Some(createHotSpotDistribution(mode, weights)), None)))
                }
            }
        } else {
            val weightedNeighbourhoodFactories =
                new mutable.ArrayBuffer[(AX[Value], () => Option[AnnotatedNeighbourhood])]
            val (axs0, axs1) =
                axs
                .sortBy(_.x.id)
                .partition(ax => space.definingConstraint(ax.x).exists(_.isCandidateForImplicitSolving(space)))
            val axs2 = randomGenerator.shuffle(axs0) ++ axs1
            for (ax <- axs2) {
                if (sigint.isSet) {
                    throw new FlatZincCompilerInterruptedException
                }
                val maybeConstraint = space.definingConstraint(ax.x)
                if (maybeConstraint.isDefined) {
                    weightedNeighbourhoodFactories += ax -> createNeighbourhoodFactory(mode, levelCfg, maybeConstraint.get)
                } else if (! ax.x.isParameter) {
                    weightedNeighbourhoodFactories += ax -> (() =>
                        if (implicitlyConstrainedVars.contains(ax.x)) {
                            None
                        } else {
                            logger.logg("Adding a neighbourhood over %s".format(ax.x))
                            Some(OtherNeighbourhood(new SimpleRandomReassignmentGenerator(space, Vector(ax.x), randomGenerator)))
                        }
                    )
                }
            }
            () => {
                val (weightedImplicitConstraintMaintainers, otherWeightedNeighbourhoods) =
                    weightedNeighbourhoodFactories
                    .toIterator
                    .map{case (ax, neighbourhoodFactory) => (ax, neighbourhoodFactory.apply)}
                    .filter{case (_, maybeNeighbourhood) => maybeNeighbourhood.isDefined}
                    .map{case (ax, maybeNeighbourhood) => (ax, maybeNeighbourhood.get)}
                    .toIndexedSeq
                    .partition { case (_, ImplicitConstraintMaintainer(_)) => true; case _ => false }
                val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
                neighbourhoods ++= weightedImplicitConstraintMaintainers.toIterator.map(_._2.neighbourhood)
                if (! otherWeightedNeighbourhoods.isEmpty) {
                    val (weights, otherNeighbourhoods) =
                        otherWeightedNeighbourhoods
                        .map{case (ax, OtherNeighbourhood(neighbourhood)) => (ax, neighbourhood)}.unzip
                    neighbourhoods +=
                        considerVariableChoiceRate(
                            if (otherNeighbourhoods.size == 1) {
                                otherNeighbourhoods.head
                            } else {
                                new NeighbourhoodCollection(
                                    otherNeighbourhoods, randomGenerator, Some(createHotSpotDistribution(mode, weights)), None)
                            },
                            levelCfg
                        )
                }
                if (neighbourhoods.size < 2) {
                    neighbourhoods
                    .headOption
                    .map(if (otherWeightedNeighbourhoods.isEmpty) ImplicitConstraintMaintainer else OtherNeighbourhood)
                } else {
                    Some(OtherNeighbourhood(
                        new NeighbourhoodCollection(neighbourhoods.toIndexedSeq, randomGenerator, None, None)))
                }
            }
        }
    }

    private def createHotSpotDistribution
        [Value <: NumericalValue[Value]]
        (mode: OptimizationMode.Value, weights: Seq[AX[Value]])
        (implicit valueTraits: NumericalValueTraits[Value]):
        Distribution =
    {
        val hotSpotDistribution = DistributionFactory.createDistribution(weights.size)
        space.post(new DistributionMaintainer(nextConstraintId, null, mode, weights.toIndexedSeq, hotSpotDistribution))
        hotSpotDistribution
    }

    private def considerVariableChoiceRate
        (neighbourhood0: Neighbourhood, levelCfg: FlatZincLevelConfiguration): Neighbourhood =
    {
        val rate = (levelCfg.maybeFairVariableChoiceRate.getOrElse(Probability.from(0)).value * 100).toInt
        if (rate == 0) neighbourhood0
        else {
            val xs = (neighbourhood0.searchVariables -- implicitlyConstrainedVars).toBuffer.sorted.toIndexedSeq
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
