package yuck.flatzinc.compiler

import scala.collection._

import yuck.annealing.RandomReassignmentGenerator
import yuck.annealing.RandomCircularSwapGenerator
import yuck.annealing.SimpleRandomMoveGenerator
import yuck.constraints.Alldistinct
import yuck.constraints.DistributionMaintainer
import yuck.constraints.LinearCombination
import yuck.constraints.Sum
import yuck.core._

/**
 * Generates focused move generators for satisfaction and minimization goals.
 *
 * The generation is driven by the constraints that contribute to the goal: Each one
 * is asked to contribute a move generator. The resulting generators are collected and
 * wrapped up in an instance of [[yuck.core.MoveGeneratorCollection MoveGeneratorCollection]]
 * such that the process of move generation will comprise two steps: First some move
 * generator will be chosen and second the chosen generator will be asked to provide a move.
 *
 * To focus the search on violated constraints, we keep track of (weighted) constraint
 * violations by means of a dynamic distribution (maintained by an instance of
 * [[yuck.constraints.DistributionMaintainer DistributionMaintainer]]) for use in the
 * first step of move generation.
 *
 * @author Michael Marte
 */
final class ConstraintBasedStrategyFactory
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends StrategyFactory(cc, randomGenerator)
{

    private val variablesToIgnore = mutable.Set[AnyVariable]()

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
            createMoveGenerator(space.definingConstraint(x).get)
        }
        else {
            // The thing that should not be.
            ???
        }
    }

    private def createMoveGenerator(constraint: yuck.core.Constraint): Option[MoveGenerator] = {
        constraint match {
            case lc: LinearCombination[IntegerValue @ unchecked]
            if lc.axs.forall(ax => ax.a.value >= 0 && ax.x.domain.asInstanceOf[IntegerDomain].maybeLb.exists(_.value >= 0)) =>
                // might be a minimization goal in terms of non-negative search variables
                createMoveGenerator(lc.axs)
            case sum: Sum[IntegerValue @ unchecked]
            if sum.xs.forall(x => x.domain.asInstanceOf[IntegerDomain].maybeLb.exists(_.value >= 0)) =>
                // might be a satisfaction goal
                createMoveGenerator(sum.xs.map(new AX(One, _)))
            case alldistinct: Alldistinct[_]
            if ((alldistinct.xs.toSet & variablesToIgnore).isEmpty && alldistinct.isTight(space)) =>
                val xs = alldistinct.xs.toIterator.filter(_.isVariable).toIndexedSeq
                val domain = xs.head.domain
                for ((x, a) <- xs.zip(randomGenerator.shuffle(domain.values.toIndexedSeq))) {
                    space.setValue(x, a)
                }
                variablesToIgnore ++= xs
                Some(new RandomCircularSwapGenerator(space, xs, randomGenerator, cfg.moveSizeDistribution, null, 0))
            case _ =>
                val xs = space.involvedSearchVariables(constraint) -- variablesToIgnore
                if (xs.isEmpty) None
                else Some(new RandomReassignmentGenerator(space, xs.toIndexedSeq, randomGenerator, cfg.moveSizeDistribution, null, 0))
        }
    }

    private def createMoveGenerator(axs: immutable.Seq[AX[IntegerValue]]): Option[MoveGenerator] = {
        val moveGenerators = new mutable.ArrayBuffer[MoveGenerator]
        val weights = new mutable.ArrayBuffer[AX[IntegerValue]]
        val (axs0, axs1) = axs.partition(ax => isDefinedByTightAlldistinctConstraint(ax.x))
        val axs2 = randomGenerator.shuffle(axs0) ++ axs1
        for (ax <- axs2) {
            val maybeConstraint = space.definingConstraint(ax.x)
            if (maybeConstraint.isDefined) {
                val constraint = maybeConstraint.get
                val maybeMoveGenerator = createMoveGenerator(constraint)
                if (maybeMoveGenerator.isDefined) {
                    moveGenerators += maybeMoveGenerator.get
                    weights += ax
                    logger.logg("%s contributed a move generator over %s".format(constraint, maybeMoveGenerator.get.xs))
                }
            } else if (! ax.x.isParameter && ! variablesToIgnore.contains(ax.x)) {
                logger.logg("Adding move generator over %s".format(ax.x))
                moveGenerators += new SimpleRandomMoveGenerator(space, immutable.IndexedSeq(ax.x), randomGenerator)
                weights += ax
            }
        }
        if (moveGenerators.size < 2) moveGenerators.headOption
        else {
            val hotSpotDistribution = DistributionFactory.createDistribution(weights.size)
            space.post(new DistributionMaintainer(nextConstraintId, null, weights.toIndexedSeq, hotSpotDistribution))
            Some(new MoveGeneratorCollection(
                moveGenerators.toIndexedSeq, randomGenerator, hotSpotDistribution, cfg.probabilityOfFairChoiceInPercent))
        }
    }

    private def isDefinedByTightAlldistinctConstraint(x: Variable[IntegerValue]): Boolean =
        space.definingConstraint(x).exists(isTightAlldistinctConstraint)

}
