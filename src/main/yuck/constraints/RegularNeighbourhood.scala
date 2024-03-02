package yuck.constraints

import scala.collection.*

import yuck.constraints.RegularGraph.*
import yuck.core.*
import yuck.util.arm.scoped

/**
 * This neighbourhood can be used to maintain any ''regular'' constraint without duplicate variables.
 *
 * It proposes a random move and uses the given RegularGraph to turn the proposal into a feasible move
 * by means of a shortest-path search from source to sink. To this end, it penalizes the edges of the
 * graph with the aim to encourage a path that implements the proposal and does not change too much else.
 *
 * @author Michael Marte
 */
class RegularNeighbourhood
    (space: Space,
     xs: immutable.IndexedSeq[IntegerVariable],
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     maybeHotSpotDistribution: Option[Distribution], maybeFairVariableChoiceRate: Option[Probability],
     graph: RegularGraph,
     initialPath: IndexedSeq[Transition])
    extends Neighbourhood
{

    private val n = xs.size

    require(n > 1)
    require(xs.toSet.size == n)
    require(xs.exists(space.isSearchVariable))
    require(xs.forall(_.domain.isFinite))
    require(xs.forall(_.hasValidValue(space.searchState)))
    require(xs.view.zip(initialPath).forall {
        case (x, Assignment(_, y, d, _)) => x == y && d.contains(space.searchState.value(x))
    })

    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)

    private val now = space.searchState

    private val uniformDistribution =
        Distribution(0, Vector.tabulate(n)(i => if space.isProblemParameter(xs(i)) then 0 else 1))

    private val frequencyRestorer = new FrequencyRestorer(moveSizeDistribution.size - 1)

    private val x2i = xs.iterator.zipWithIndex.toMap[AnyVariable, Int]

    private var currentPath: Array[Transition] = initialPath.toArray
    private var futurePath: Array[Transition] = null

    override def searchVariables = xs.iterator.filterNot(_.domain.isSingleton).toSet

    override def children = Nil

    override def nextMove = {

        val useUniformDistribution =
            maybeHotSpotDistribution.isEmpty ||
                maybeHotSpotDistribution.get.volume == 0 ||
                (maybeFairVariableChoiceRate.isDefined && randomGenerator.nextDecision(maybeFairVariableChoiceRate.get))
        val priorityDistribution = if useUniformDistribution then uniformDistribution else maybeHotSpotDistribution.get

        val proposal = scoped(frequencyRestorer) {
            priorityDistribution
                .nextIndices(randomGenerator, moveSizeDistribution.nextIndex(randomGenerator), frequencyRestorer)
                .map(i => {
                    val x = xs(i)
                    val a = x.domain.nextRandomValue(randomGenerator, now.value(x))
                    x -> a
                })
                .to(immutable.HashMap)
        }

        val move = new BulkMove(space.nextMoveId())
        val searchIsRequired = proposal.exists((x, a) => ! currentPath(x2i(x)).asInstanceOf[Assignment].d.contains(a))
        if (searchIsRequired) {
            // The proposal will be ignored in parts or totally if it is infeasible.
            futurePath = graph.computeShortestPath(new PenaltyProvider(proposal, now)).get
            for (case Assignment(_, x, d, _) <- futurePath) {
                val a = now.value(x)
                val effect = x.reuseableEffect
                if (proposal.contains(x)) {
                    val b = proposal(x)
                    if (d.contains(b)) {
                        effect.a = b
                        move += effect
                    } else if (d.contains(a)) {
                        effect.a = d.nextRandomValue(randomGenerator, a)
                        move += effect
                    } else {
                        effect.a = d.randomValue(randomGenerator)
                        move += effect
                    }
                } else if (! d.contains(a)) {
                    effect.a = d.randomValue(randomGenerator)
                    move += effect
                }
            }
        } else {
            for ((x, a) <- proposal) {
                val effect = x.reuseableEffect
                effect.a = a
                move += effect
            }
            futurePath = currentPath
        }

        move
    }

    override def commit(move: Move) = {
        currentPath = futurePath
    }

    // Just forbidding the current values (of randomly chosen variables) was not good enough:
    // with the resulting neighbourhoods, the solver failed to optimize solutions.
    // The current approach (based on random move proposals) does not suffer from this flaw.
    private final class PenaltyProvider
        (proposal: immutable.HashMap[IntegerVariable, IntegerValue], searchState: SearchState)
        extends DistanceProvider
    {
        override def apply(transition: Transition) =
            transition match {
                case Assignment(_, x, d, _) =>
                    if (proposal.contains(x)) {
                        // x is affected by the proposal: Strongly discourage all values except for the proposed one.
                        if d.contains(proposal(x)) then 0 else n
                    } else {
                        // x is not affected by the proposal: Discourage any change of its value.
                        if d.contains(searchState.value(x)) then 0 else 1
                    }
                case _ => 0
            }
    }

}
