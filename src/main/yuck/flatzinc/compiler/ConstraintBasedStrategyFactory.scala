package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints.DistributionMaintainer
import yuck.constraints.LinearCombination
import yuck.constraints.Sum
import yuck.core._

/**
 * Generates focused move generators for satisfaction and minimization goals.
 *
 * The generation is driven by the constraints that contribute to the goal: For each one,
 * a move generator is created. The resulting generators are collected and
 * wrapped up in an instance of [[yuck.core.MoveGeneratorCollection MoveGeneratorCollection]]
 * such that the process of move generation will comprise two steps: First some move
 * generator will be chosen and second the chosen generator will be asked to provide a move.

 * To focus the search on violated constraints, we keep track of (weighted) constraint
 * violations by means of a dynamic distribution (maintained by an instance of
 * [[yuck.constraints.DistributionMaintainer DistributionMaintainer]]) for use in the
 * first step of move generation.
 *
 * Candidates for implicit solving are handled with priority to avoid that their variables
 * get included into other move generators. (In case two candidates for implicit solving
 * compete for a variable, this conflict is resolved by random choice.) This special
 * treatment is implemented using laziness to a certain extent: The procedure creates a
 * factory that already contains the move generators for implicit constraints while the
 * remaining move generators get generated on demand.
 *
 * @author Michael Marte
 */
final class ConstraintBasedStrategyFactory
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends StrategyFactory(cc, randomGenerator)
{

    protected override def createMoveGeneratorForSatisfactionGoal(x: Variable[IntegerValue]) =
        createMoveGeneratorForMinimizationGoal(x)

    protected override def createMoveGeneratorForMinimizationGoal(x: Variable[IntegerValue]) = {
        if (space.isDanglingVariable(x)) {
            // assign minimum value
            super.createMoveGeneratorForMinimizationGoal(x)
        }
        else if (space.isSearchVariable(x)) {
            // There are hard constraints on x, so no need to add a generator for minimizing x.
            None
        }
        else if (space.isChannelVariable(x)) {
            createMoveGeneratorFactory(space.definingConstraint(x).get).apply.map(_.moveGenerator)
        }
        else {
            // The thing that should not be.
            ???
        }
    }

    private abstract class AnnotatedMoveGenerator {
        def moveGenerator: MoveGenerator
    }
    private case class ImplicitConstraintMaintainer(override val moveGenerator: MoveGenerator) extends AnnotatedMoveGenerator
    private case class OtherMoveGenerator(override val moveGenerator: MoveGenerator) extends AnnotatedMoveGenerator

    private def createMoveGeneratorFactory(constraint: yuck.core.Constraint): () => Option[AnnotatedMoveGenerator] = {
        constraint match {
            case lc: LinearCombination[IntegerValue @ unchecked]
            if lc.axs.forall(ax => ax.a.value >= 0 && IntegerValue.Traits.staticCast(ax.x.domain).maybeLb.exists(_.value >= 0)) =>
                // might be a minimization goal in terms of non-negative search variables
                createMoveGeneratorFactory(lc.axs)
            case sum: Sum[IntegerValue @ unchecked]
            if sum.xs.forall(x => IntegerValue.Traits.staticCast(x.domain).maybeLb.exists(_.value >= 0)) =>
                // might be a satisfaction goal
                createMoveGeneratorFactory(sum.xs.map(new AX(One, _)))
            case _ => {
                val xs = constraint.inVariables.toIterator.filter(space.isSearchVariable).toSet
                val maybeMoveGenerator =
                    if ((xs & variablesToIgnore).isEmpty) {
                        constraint.prepareForImplicitSolving(space, randomGenerator, cfg.moveSizeDistribution, _ => None, 0)
                    } else {
                        None
                    }
                if (maybeMoveGenerator.isDefined) {
                    variablesToIgnore ++= xs
                    space.markAsImplied(constraint)
                    () => {
                        logger.logg("Adding move generator for implicit constraint %s".format(constraint))
                        maybeMoveGenerator.map(ImplicitConstraintMaintainer)
                    }
                } else () => {
                    val xs = cc.space.involvedSearchVariables(constraint) -- variablesToIgnore
                    if (xs.isEmpty) {
                        None
                    } else {
                        logger.logg("%s contributes a move generator over %s".format(constraint, xs))
                        val moveGenerator =
                            new RandomReassignmentGenerator(
                                space, xs.toIndexedSeq, randomGenerator, cfg.moveSizeDistribution, null,
                                cfg.probabilityOfFairChoiceInPercent)
                        Some(OtherMoveGenerator(moveGenerator))
                    }
                }
            }
        }
    }

    private def createMoveGeneratorFactory(axs: immutable.Seq[AX[IntegerValue]]): () => Option[AnnotatedMoveGenerator] = {
        val weightedMoveGeneratorFactories =
            new mutable.ArrayBuffer[(AX[IntegerValue], () => Option[AnnotatedMoveGenerator])]
        // Partitioning before shuffling is pointless but this step was in a prior version and it was kept to
        // facilitate the comparison of test results.
        val (axs0, axs1) =
            axs.partition(ax => cc.space.definingConstraint(ax.x).exists(_.isCandidateForImplicitSolving(space)))
        val axs2 = randomGenerator.shuffle(axs0) ++ axs1
        for (ax <- axs2) {
            val maybeConstraint = space.definingConstraint(ax.x)
            if (maybeConstraint.isDefined) {
                weightedMoveGeneratorFactories += ax -> createMoveGeneratorFactory(maybeConstraint.get)
            } else if (! ax.x.isParameter) {
                weightedMoveGeneratorFactories += ax -> (() =>
                    if (variablesToIgnore.contains(ax.x)) {
                        None
                    } else {
                        logger.logg("Adding move generator over %s".format(ax.x))
                        Some(OtherMoveGenerator(new SimpleRandomMoveGenerator(space, immutable.IndexedSeq(ax.x), randomGenerator)))
                    }
                )
            }
        }
        () => {
            val (weightedImplicitConstraintMaintainers, otherWeightedMoveGenerators) =
                weightedMoveGeneratorFactories
                .toIterator
                .map{case (ax, moveGeneratorFactory) => (ax, moveGeneratorFactory.apply)}
                .filter{case (_, maybeMoveGenerator) => maybeMoveGenerator.isDefined}
                .map{case (ax, maybeMoveGenerator) => (ax, maybeMoveGenerator.get)}
                .toIndexedSeq
                .partition{case (_, ImplicitConstraintMaintainer(_)) => true; case _ => false}
            val moveGenerators = new mutable.ArrayBuffer[MoveGenerator]
            moveGenerators ++= weightedImplicitConstraintMaintainers.toIterator.map(_._2.moveGenerator)
            if (! otherWeightedMoveGenerators.isEmpty) {
                val (weights, otherMoveGenerators) =
                    otherWeightedMoveGenerators
                    .map{case (ax, OtherMoveGenerator(moveGenerator)) => (ax, moveGenerator)}.unzip
                if (otherMoveGenerators.size == 1) {
                    moveGenerators += otherMoveGenerators.head
                } else {
                    val hotSpotDistribution = DistributionFactory.createDistribution(weights.size)
                    space.post(new DistributionMaintainer(nextConstraintId, null, weights, hotSpotDistribution))
                    moveGenerators +=
                        new MoveGeneratorCollection(
                            otherMoveGenerators, randomGenerator, hotSpotDistribution, cfg.probabilityOfFairChoiceInPercent)
                }
            }
            if (moveGenerators.size < 2) {
                moveGenerators
                .headOption
                .map(if (otherWeightedMoveGenerators.isEmpty) ImplicitConstraintMaintainer else OtherMoveGenerator)
            } else {
                Some(OtherMoveGenerator(new MoveGeneratorCollection(moveGenerators.toIndexedSeq, randomGenerator, null, cfg.probabilityOfFairChoiceInPercent)))
            }
        }
    }

}
