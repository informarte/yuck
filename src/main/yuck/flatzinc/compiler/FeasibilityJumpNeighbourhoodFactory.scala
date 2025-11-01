package yuck.flatzinc.compiler

import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap
import scala.collection.*

import yuck.constraints.SatisfactionGoalTracker
import yuck.constraints.SatisfactionGoalTracker.computeInvolvementMap
import yuck.core.*
import yuck.fj.FeasibilityJumpNeighbourhood
import yuck.util.Collections.*

/**
 * @author Michael Marte
 *
 */
final class FeasibilityJumpNeighbourhoodFactory
    (override protected val cc: CompilationContext,
     randomGenerator: RandomGenerator)
    extends CompilationPhase
{

    override def run() = {
        cc.maybeNeighbourhood = createNeighbourhood
    }

    private def createNeighbourhood: Option[Neighbourhood] = {
        cc.objective match {
            case objective: SatisfactionObjective =>
                createNeighbourhood(objective)
            case hierarchicalObjective: HierarchicalObjective =>
                hierarchicalObjective.primitiveObjectives match {
                    case List(mainObjective: SatisfactionObjective, _: NumericalObjective[?]) =>
                        createNeighbourhood(mainObjective)
                    case _ =>
                        None
                }
            case _ => None
        }
    }

    private def createNeighbourhood(objective: SatisfactionObjective): Option[Neighbourhood] = {
        val xs = cc.space.involvedSearchVariables(objective.x).diff(cc.implicitlyConstrainedVars).toBuffer.sorted.toVector
        for (x <- xs if ! x.domain.isFinite) {
            throw new VariableWithInfiniteDomainException(x)
        }
        for (x <- cc.costVars) {
            cc.space.registerObjectiveVariable(x)
        }
        if xs.isEmpty
        then None
        else {
            val cs = cc.costVars.toVector
            val involvementMap = computeInvolvementMap(cc.space, xs, cs)
            val hotSpotDistribution = Distribution(xs.size)
            cc.post(new SatisfactionGoalTracker(cc.space.nextConstraintId(), None, involvementMap, hotSpotDistribution))
            val cfg = cc.cfg.feasibilityJumpConfiguration
            Some(new FeasibilityJumpNeighbourhood(
                cc.space,
                xs,
                cs,
                involvementMap,
                hotSpotDistribution,
                randomGenerator,
                cfg.moveSizeDistribution,
                cfg.maximumNumberOfJumpCandidates(xs.size),
                cfg.useConvexArgMin,
                cfg.numberOfValuesToExplore,
                cfg.minimumJumpValueCacheHitRate,
                cfg.weightDecayRate,
                cfg.resetWeightsOnPerturbation,
                cfg.scaleCostDeltas))
        }
    }

}
