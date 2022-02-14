package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.{LinearCombination, OptimizationGoalTracker, Sum}
import yuck.core.*
import yuck.flatzinc.FlatZincLevelConfiguration
import yuck.flatzinc.ast.IntConst
import yuck.util.arm.Sigint

/**
 * Generates focused neighbourhoods for satisfaction and minimization goals.
 *
 * Satisfaction goals are handled by the superclass.
 *
 * The generation of a focused neighbourhood for minimization departs from the
 * variable x to minimize.
 * Say Y is the set of search variables involved in the minimization of x.
 * For each y in Y, a so-called hot-spot indicator variable h(y) is created that reflects
 * the influence of y on x, see createHotSpotIndicators.
 *
 * Given some subset Z of Y, a neighbourhood over Z is assembled on the basis of a distribution
 * of size |Z| the frequencies of which will be kept in sync with the values of the h(z), z in Z.
 *
 * @author Michael Marte
 */
final class VariableDrivenNeighbourhoodFactory
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

    private def createNeighbourhood
        [V <: NumericalValue[V]]
        (mode: OptimizationMode.Value, levelCfg: FlatZincLevelConfiguration, x: NumericalVariable[V])
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Neighbourhood] =
    {
        require(mode == OptimizationMode.Min)
        val hotSpotIndicators =
            logger.withTimedLogScope("Creating hot-spot indicators for %s".format(x)) {
                createHotSpotIndicators(x)
            }
        val neighbourhoods = new mutable.ArrayBuffer[Neighbourhood]
        val remainingVariables = hotSpotIndicators.keys.toSet -- implicitlyConstrainedVars
        if (! remainingVariables.isEmpty) {
            val xs = remainingVariables.toBuffer.sorted.toIndexedSeq
            for (x <- xs if ! x.domain.isFinite) {
                throw new VariableWithInfiniteDomainException(x)
            }
            logger.logg("Adding a neighbourhood over %s".format(xs))
            neighbourhoods +=
                new RandomReassignmentGenerator(
                    space, xs, randomGenerator, cfg.moveSizeDistribution,
                    createHotSpotDistribution(hotSpotIndicators)(xs), levelCfg.maybeFairVariableChoiceRate)
        }
        if (neighbourhoods.size < 2) {
            neighbourhoods.headOption
        } else {
            Some(new NeighbourhoodCollection(neighbourhoods.toIndexedSeq, randomGenerator, None, None))
        }
    }

    // Creates a hot-spot indicator for each search variable y involved in computing
    // the value of the given variable x.
    // A hot-spot indicator h(y) is a variable that reflects the influence of y on x.
    // (More precisely, the higher the influence of y's value on the value of x,
    // the higher the value of h(y).)
    // The resulting hot-spot indicators are for defining hot-spot aware neighbourhoods
    // via neighbourhood-specific distributions.
    // The code below deals with three cases:
    // 1. x is a search variable itself.
    //    (In FlatZinc practice x may be the end of the last task in a makespan minimization problem.)
    // 2. x is defined by a linear combination with non-negative terms.
    //    (In FlatZinc practice there are three uses for a top-level linear combination:
    //     1. To minimize hard constraint violation, the FlatZinc compiler uses a top-level
    //        linear combination that computes the violation of hard constraints.
    //     2. To minimize soft constraint violation, the model uses reification in combination
    //        with a top-level linear combination that computes the violation of soft constraints.
    //     3. To minimize resource usage, the model uses a linear combination to compute the
    //        resource consumption.)
    // 3. All other cases.
    //    (Maybe there is only a single soft constraint.)
    private def createHotSpotIndicators
        [V <: NumericalValue[V]]
        (x: AnyVariable)
        (implicit valueTraits: NumericalValueTraits[V]):
        Map[AnyVariable, NumericalVariable[V]] =
    {
        val zs = new mutable.AnyRefMap[AnyVariable, mutable.ArrayBuffer[AX[V]]]
        if (space.isSearchVariable(x)) {
            zs += x -> new mutable.ArrayBuffer[AX[V]]
        }
        else if (space.isChannelVariable(x)) {
            for (y <- space.involvedSearchVariables(x)) {
                zs += y -> new mutable.ArrayBuffer[AX[V]]
            }
            val constraint: yuck.core.Constraint = space.definingConstraint(x)
            constraint match {
                case lc: LinearCombination[V @ unchecked] =>
                    for (ax <- lc.axs
                         if ax.a >= valueTraits.zero && ax.x.domain.maybeLb.exists(_ >= valueTraits.zero))
                    {
                        for (y <- space.involvedSearchVariables(ax.x)) {
                            zs(y) += ax
                        }
                    }
                case sum: Sum[V @ unchecked] =>
                    for (x <- sum.xs if x.domain.maybeLb.exists(_ >= valueTraits.zero)) {
                        for (y <- space.involvedSearchVariables(x)) {
                            zs(y) += new AX(valueTraits.one, x)
                        }
                    }
                case _ =>
            }
        }
        val s = new mutable.AnyRefMap[AnyVariable, NumericalVariable[V]]
        for (x <- zs.keys) {
            s += x -> createNonNegativeChannel[V]
            val zl = AX.compact(zs(x))
            if (zl.forall(ax => ax.a == valueTraits.one)) {
               space.post(new Sum(nextConstraintId, null, zl.iterator.map(ax => ax.x).toIndexedSeq, s(x)))
            } else {
               space.post(new LinearCombination(nextConstraintId, null, zl.toIndexedSeq, s(x)))
            }
        }
        s
    }

    private def createHotSpotDistribution
        [V <: NumericalValue[V]]
        (hotSpotIndicators: Map[AnyVariable, NumericalVariable[V]])
        (xs: Seq[AnyVariable])
        (implicit valueTraits: NumericalValueTraits[V]):
        Option[Distribution] =
    {
        val hotSpotDistribution = Distribution(xs.size)
        val weightedIndicators = xs.iterator.map(x => new AX(valueTraits.one, hotSpotIndicators(x))).toIndexedSeq
        space.post(new OptimizationGoalTracker(nextConstraintId, null, OptimizationMode.Min, weightedIndicators, hotSpotDistribution))
        Some(hotSpotDistribution)
    }

}
