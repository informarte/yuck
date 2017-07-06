package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.DistributionMaintainer
import yuck.constraints.LinearCombination
import yuck.constraints.Sum
import yuck.core._


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

    protected override def createNeighbourhoodForSatisfactionGoal(x: Variable[IntegerValue]) =
        createNeighbourhoodForMinimizationGoal(x)

    protected override def createNeighbourhoodForMinimizationGoal(x: Variable[IntegerValue]) = {
        if (space.isDanglingVariable(x)) {
            // assign minimum value
            super.createNeighbourhoodForMinimizationGoal(x)
        }
        else if (space.isSearchVariable(x)) {
            // There are no hard constraints on x, so no need to add a generator for minimizing x.
            None
        }
        else if (space.isChannelVariable(x)) {
            createNeighbourhoodFactory(space.definingConstraint(x).get).apply.map(_.neighbourhood)
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

    private def createNeighbourhoodFactory(constraint: yuck.core.Constraint): () => Option[AnnotatedNeighbourhood] = {
        constraint match {
            case lc: LinearCombination[IntegerValue @ unchecked]
            if lc.axs.forall(ax => ax.a.value >= 0 && IntegerValueTraits.staticDowncast(ax.x.domain).maybeLb.exists(_.value >= 0)) =>
                // might be a minimization goal in terms of non-negative search variables
                createNeighbourhoodFactory(lc.axs)
            case sum: Sum[IntegerValue @ unchecked]
            if sum.xs.forall(x => IntegerValueTraits.staticDowncast(x.domain).maybeLb.exists(_.value >= 0)) =>
                // might be a satisfaction goal
                createNeighbourhoodFactory(sum.xs.map(new AX(One, _)))
            case _ => {
                val xs = constraint.inVariables.toSet
                val maybeNeighbourhood =
                    if (constraint.isCandidateForImplicitSolving(space) && (xs & implicitlyConstrainedVars).isEmpty) {
                        constraint.prepareForImplicitSolving(space, randomGenerator, cfg.moveSizeDistribution, _ => None, 0, sigint)
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
                    val xs = cc.space.involvedSearchVariables(constraint) -- implicitlyConstrainedVars
                    if (xs.isEmpty) {
                        None
                    } else {
                        for (x <- xs if x.domain.isInfinite) {
                            throw new VariableWithInfiniteDomainException(x)
                        }
                        logger.logg("%s contributes a neighbourhood over %s".format(constraint, xs))
                        val neighbourhood =
                            new RandomReassignmentGenerator(
                                space, xs.toIndexedSeq, randomGenerator, cfg.moveSizeDistribution, None,
                                cfg.probabilityOfFairChoiceInPercent)
                        Some(OtherNeighbourhood(neighbourhood))
                    }
                }
            }
        }
    }

    private def createNeighbourhoodFactory(axs: immutable.Seq[AX[IntegerValue]]): () => Option[AnnotatedNeighbourhood] = {
        val weightedNeighbourhoodFactories =
            new mutable.ArrayBuffer[(AX[IntegerValue], () => Option[AnnotatedNeighbourhood])]
        // Partitioning before shuffling is pointless but this step was in a prior version and it was kept to
        // facilitate the comparison of test results.
        val (axs0, axs1) =
            axs.partition(ax => cc.space.definingConstraint(ax.x).exists(_.isCandidateForImplicitSolving(space)))
        val axs2 = randomGenerator.shuffle(axs0) ++ axs1
        for (ax <- axs2) {
            if (sigint.isSet) {
                throw new FlatZincCompilerInterruptedException
            }
            val maybeConstraint = space.definingConstraint(ax.x)
            if (maybeConstraint.isDefined) {
                weightedNeighbourhoodFactories += ax -> createNeighbourhoodFactory(maybeConstraint.get)
            } else if (! ax.x.isParameter) {
                weightedNeighbourhoodFactories += ax -> (() =>
                    if (implicitlyConstrainedVars.contains(ax.x)) {
                        None
                    } else {
                        logger.logg("Adding a neighbourhood over %s".format(ax.x))
                        Some(OtherNeighbourhood(
                                new SimpleRandomReassignmentGenerator(space, immutable.IndexedSeq(ax.x), randomGenerator)))
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
                .partition{case (_, ImplicitConstraintMaintainer(_)) => true; case _ => false}
            val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
            neighbourhoods ++= weightedImplicitConstraintMaintainers.toIterator.map(_._2.neighbourhood)
            if (! otherWeightedNeighbourhoods.isEmpty) {
                val (weights, otherNeighbourhoods) =
                    otherWeightedNeighbourhoods
                    .map{case (ax, OtherNeighbourhood(neighbourhood)) => (ax, neighbourhood)}.unzip
                if (otherNeighbourhoods.size == 1) {
                    neighbourhoods += otherNeighbourhoods.head
                } else {
                    val hotSpotDistribution = DistributionFactory.createDistribution(weights.size)
                    space.post(new DistributionMaintainer(nextConstraintId, null, weights, hotSpotDistribution))
                    neighbourhoods +=
                        new NeighbourhoodCollection(otherNeighbourhoods, randomGenerator, Some(hotSpotDistribution), 0)
                }
            }
            if (neighbourhoods.size < 2) {
                neighbourhoods
                .headOption
                .map(if (otherWeightedNeighbourhoods.isEmpty) ImplicitConstraintMaintainer else OtherNeighbourhood)
            } else {
                Some(OtherNeighbourhood(
                        new NeighbourhoodCollection(
                            neighbourhoods.toIndexedSeq, randomGenerator, None, cfg.probabilityOfFairChoiceInPercent)))
            }
        }
    }

}
