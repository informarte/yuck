package yuck.fj

import java.util.Arrays

import scala.collection.*
import scala.collection.mutable.ArrayBuffer

import yuck.core.*
import yuck.util.arm.scoped
import yuck.util.Collections.*

/**
 * Generates random downhill moves.
 *
 * This class implements part of the Generalized Feasibility jump method described in the CPAIOR-2024
 * paper on ViolationLS by Davies et al.
 *
 * When convexArgMin is set, the implementation assumes a convex cost function, caching all jump values.
 * Otherwise a random sampling approach is used and jump-value caching is limited to decision
 * variables all values of which are explored according to numberOfValuesToExplore.
 *
 * If moveSizeDistribution allows for it, compound moves (affecting more than variable) may be generated.
 *
 * @author Michael Marte
 */
final class FeasibilityJumpNeighbourhood
    (override protected val space: Space,
     xs: immutable.Vector[AnyVariable],
     cs: immutable.Vector[BooleanVariable],
     involvementMap: immutable.Map[BooleanVariable, IntArraySeq],
     hotSpotDistribution: Distribution,
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     maximumNumberOfJumpCandidates: Int,
     useConvexArgMin: Boolean,
     numberOfValuesToExplore: Int => Int,
     minimumJumpValueCacheHitRate: Double,
     weightDecayRate: Double,
     resetWeightsOnPerturbation: Boolean,
     scaleCostDeltas: Boolean)
    extends Neighbourhood
{

    import FeasibilityJumpNeighbourhood.*

    private val n = xs.size
    require(n > 0)
    require(n == xs.toSet.size)

    require(xs.forall(space.isSearchVariable))
    require(xs.forall(_.domain.isFinite))

    require(cs.forall(space.isObjectiveVariable))

    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)

    private val m = cs.size
    require(m > 0)

    private val xsir = xs.indices
    private val csir = cs.indices

    require(involvementMap.keySet.subsetOf(cs.toSet))
    for (xis <- involvementMap.values) {
        require(xis.isSorted)
        require(xis.inlineForall(xsir.contains))
    }

    require(hotSpotDistribution.size == xs.size)

    require(maximumNumberOfJumpCandidates > 0)

    private val frequencyRestorer = new FrequencyRestorer(n)

    private val weights = Array.fill(m)(1.0)

    private def resetWeights(): Unit = {
        Arrays.fill(weights, 1.0)
    }

    private val costDeltaSampleSizes = Array.fill(m)(0.0)
    private val costDeltaAverages = Array.fill(m)(0.0)
    private val costDeltaScalingFactors = Array.fill(m)(1.0)

    private def setCostDeltaScalingFactors(): Unit = {
        var ci = m
        while (ci > 0) {
            ci -= 1
            if (costDeltaSampleSizes(ci) > 0.0) {
                costDeltaScalingFactors(ci) = 1.0 / costDeltaAverages(ci)
                costDeltaSampleSizes(ci) = 0.0
                costDeltaAverages(ci) = 0.0
            }
        }
    }

    // The involved search variables grouped by constraint and sorted by index
    private val ci2xis: immutable.ArraySeq[IntArraySeq] =
        csir.stream.mapToObj(ci => involvementMap(cs(ci))).toArraySeq

    // The affected constraints grouped by search variable and sorted by index
    private val xi2cis: immutable.ArraySeq[IntArraySeq] =
        xsir.stream
            .mapToObj(xi => csir.stream.filter(ci => ci2xis(ci).fasterContains(xi)).toArraySeq)
            .toArraySeq

    // The affected jump-value cache entries by search variable and sorted by index
    private lazy val xi2xis: immutable.ArraySeq[IntArraySeq] =
        xsir.stream
            // Combining distinct with sorting is significantly faster than using a mutable set.
            // In-place sorting is significantly faster than toBuffer.sorted.
            .mapToObj(xi1 => xi2cis(xi1).stream
                .flatMap(ci => ci2xis(ci).stream)
                .filter(xi2 => xi2 != xi1 && (useConvexArgMin || cacheJumpValuesFor(xs(xi2))))
                .fasterDistinct
                .toArray
                .sortInPlace()
                .toArraySeq)
            .toArraySeq

    private val jumpValueCache = Array.fill[JumpCandidate](n)(null)
    private var remainingNumberOfJumpValueCacheEvaluationQueries = 1000
    private var numberOfJumpValueCacheQueries = 0L
    private var numberOfJumpValueCacheHits = 0L
    // Value caching only make sense when the involvement matrix is "sparse".
    // (Otherwise moves just invalidate the entire cache, so it's all overhead and no benefit.)
    private var useJumpValueCache = ci2xis.forall(_.size < n)

    private def clearJumpValueCache(): Unit = {
        Arrays.fill(jumpValueCache.asInstanceOf[Array[AnyRef]], null)
    }

    private def cacheJumpValuesFor(x: AnyVariable): Boolean = {
        val n = x.domain.size - 1
        require(n > 0)
        numberOfValuesToExplore(n) >= n
    }

    private case class NoJumpFound() extends Move(space.nextMoveId()) {
        override val effects = Nil
    }

    private case class Jump(override val effects: List[AnyMoveEffect], xis: List[Int]) extends Move(space.nextMoveId())

    override def searchVariables = xs.toSet

    override def children = Nil

    override def nextMove() = {

        val maybeJumpCandidate =
            findJumpCandidate(moveSizeDistribution.nextIndex(randomGenerator), JumpCandidate(Nil, Nil, null, 0.0))

        maybeJumpCandidate match {
            case None =>
                var ci = m
                while (ci > 0) {
                    ci -= 1
                    weights(ci) *= weightDecayRate
                    if (! space.searchState.value(cs(ci)).truthValue) {
                        weights(ci) += 1.0
                    }
                }
                if (useJumpValueCache) {
                    clearJumpValueCache()
                }
                NoJumpFound()
            case Some(JumpCandidate(effects, xis, _, _)) =>
                Jump(effects, xis)
        }

    }

    override def commit(move: Move) = {
        move match {
            case NoJumpFound() =>
            case Jump(_, xis) =>
                if (useJumpValueCache) {
                    for (xi <- xis) {
                        jumpValueCache(xi) = null
                        xi2xis(xi).inlineForeach(yi => jumpValueCache(yi) = null)
                    }
                }
        }
    }

    override def perturb(perturbationProbability: Probability) = {
        require(perturbationProbability.value > 0)
        val move = new BulkMove(space.nextMoveId())
        while (move.isEmpty) {
            for (x <- xs) {
                if (randomGenerator.nextDecision(perturbationProbability)) {
                    move += x.nextRandomMoveEffect(space, randomGenerator)
                }
            }
        }
        space.consult(move)
        space.commit(move)
        if (resetWeightsOnPerturbation) {
            resetWeights()
        }
        if (scaleCostDeltas) {
            setCostDeltaScalingFactors()
        }
        if (useJumpValueCache) {
            clearJumpValueCache()
        }
    }

    def onObjectiveTightened(): Unit = {
        if (useJumpValueCache) {
            clearJumpValueCache()
        }
    }

    private def findJumpCandidate(moveSize: Int, acc: JumpCandidate): Option[JumpCandidate] = {
        if moveSize == 0 || hotSpotDistribution.volume == 0
        then None
        else {
            val maybeCandidate = scoped(frequencyRestorer) {
                updateHotSpotDistribution(acc)
                hotSpotDistribution
                    .nextIndices(randomGenerator, n, frequencyRestorer)
                    .map(xi => findJumpCandidate(acc, xi))
                    .filter(moveSize > 1 || _.score < 0.0)
                    .take(maximumNumberOfJumpCandidates)
                    .minByOption(_.score)
            }
            maybeCandidate.flatMap(candidate =>
                if moveSize == 1 || candidate.score < 0.0
                then maybeCandidate
                else findJumpCandidate(moveSize - 1, candidate)
            )
        }
    }

    private def updateHotSpotDistribution(acc: JumpCandidate): Unit = {
        if (! acc.xis.isEmpty) {
            for (case c: BooleanVariable <- acc.move) {
                if (involvementMap.contains(c)) {
                    val satisfiedBefore = space.searchState.value(c).truthValue
                    val satisfiedAfter = acc.move.value(c).truthValue
                    if (satisfiedBefore != satisfiedAfter) {
                        val delta = if satisfiedBefore && ! satisfiedAfter then 1L else -1L
                        involvementMap(c).inlineForeach(xi => {
                            frequencyRestorer.memorize(hotSpotDistribution, xi)
                            hotSpotDistribution.addFrequencyDelta(xi, delta)
                        })
                    }
                }
            }
            for (xi <- acc.xis) {
                frequencyRestorer.memorize(hotSpotDistribution, xi)
                hotSpotDistribution.setFrequency(xi, 0)
            }
        }
    }

    private def findJumpCandidate(acc: JumpCandidate, xi: Int): JumpCandidate = {
        val x = xs(xi)
        val cachedJumpCandidate =
            if useJumpValueCache && (useConvexArgMin || cacheJumpValuesFor(x)) && acc.changes.isEmpty
            then {
                numberOfJumpValueCacheQueries += 1
                val jumpValueCacheEntry = jumpValueCache(xi)
                if (jumpValueCacheEntry != null) {
                    numberOfJumpValueCacheHits += 1
                }
                if (remainingNumberOfJumpValueCacheEvaluationQueries > 0) {
                    remainingNumberOfJumpValueCacheEvaluationQueries -= 1
                    if (remainingNumberOfJumpValueCacheEvaluationQueries == 0) {
                        val cacheHitRate = numberOfJumpValueCacheHits.toDouble / numberOfJumpValueCacheQueries.toDouble
                        useJumpValueCache = cacheHitRate >= minimumJumpValueCacheHitRate
                    }
                }
                jumpValueCacheEntry
            }
            else null
        if (cachedJumpCandidate == null) {
            val jumpCandidate = x match {
                case y: BooleanVariable => findJumpCandidate(acc, y, xi)
                case y: IntegerVariable =>
                    if useConvexArgMin
                    then findJumpCandidate(acc, y, xi)
                    else findJumpCandidate(acc, y.asInstanceOf[Variable[IntegerValue]], xi)
                case y: IntegerSetVariable => findJumpCandidate(acc, y, xi)
            }
            if (useJumpValueCache && jumpCandidate.changes.size == 1 && (useConvexArgMin || cacheJumpValuesFor(x))) {
                val xi = jumpCandidate.xis.head
                jumpValueCache(xi) = jumpCandidate
            }
            jumpCandidate
        } else {
            cachedJumpCandidate
        }
    }

    private def findJumpCandidate[V <: Value[V]](acc: JumpCandidate, x: Variable[V], xi: Int): JumpCandidate = {
        val a = space.searchState.value(x)
        if cacheJumpValuesFor(x)
        then {
            x.domain.valuesIterator
                .filter(_ != a)
                .map(b => computeScore(acc, x, xi, b))
                .minBy(_.score)
        } else {
            val values = new ArrayBuffer[V](x.domain.size - 1)
            values ++= x.domain.valuesIterator.filter(_ != a)
            randomGenerator
                .lazyShuffleInPlace(values)
                .map(b => computeScore(acc, x, xi, b))
                .take(numberOfValuesToExplore(x.domain.size - 1))
                .minBy(_.score)
        }
    }

    private def findJumpCandidate(acc: JumpCandidate, x: BooleanVariable, xi: Int): JumpCandidate = {
        val a = space.searchState.value(x)
        val b = if a == True then False else True
        computeScore(acc, x, xi, b)
    }

    // We don't exclude the current value from search because splitting the domain might double the effort.
    // Should the current value be found to be the best, then its score would be 0, implying all other values
    // having worse scores. So we won't miss any jump candidate.
    private def findJumpCandidate(acc: JumpCandidate, x: IntegerVariable, xi: Int): JumpCandidate =
        x.domain match {
            case range: IntegerRange =>
                convexArgMin(acc, x, xi, range)
            case rangeList: IntegerRangeList =>
                rangeList.ranges.iterator.map(convexArgMin(acc, x, xi, _)).minBy(_.score)
        }

    private def convexArgMin(acc: JumpCandidate, x: IntegerVariable, xi: Int, range: IntegerRange): JumpCandidate = {
        require(range.isFinite)
        convexArgMin(acc, x, xi, range.lb.value, null, range.ub.value, null)
    }

    // Assuming a convex cost function, convexArgMin uses ternary search to compute the jump value of x.
    private def convexArgMin
        (acc: JumpCandidate,
         x: IntegerVariable, xi: Int,
         left: Long, leftCandidate: JumpCandidate,
         right: Long, rightCandidate: JumpCandidate):
        JumpCandidate =
    {
        if (right - left > 3) {
            val leftThird = (2 * left + right) / 3
            val rightThird = (left + 2 * right) / 3
            val leftThirdCandidate = computeScore(acc, x, xi, IntegerValue(leftThird))
            val rightThirdCandidate = computeScore(acc, x, xi, IntegerValue(rightThird))
            if (leftThirdCandidate.score > rightThirdCandidate.score) {
                convexArgMin(acc, x, xi, leftThird, leftThirdCandidate, right, rightCandidate)
            } else {
                convexArgMin(acc, x, xi, left, leftCandidate, rightThird, rightThirdCandidate)
            }
        } else {
            IntegerRange(left, right).valuesIterator
                .map(a =>
                    if a.value == left && leftCandidate != null
                    then leftCandidate
                    else if a.value == right && rightCandidate != null
                    then rightCandidate
                    else computeScore(acc, x, xi, a))
                .minBy(_.score)
        }
    }

    // The work done by this method dominates everything else, so no need for code tuning in other places.
    private def computeScore[V <: Value[V]](acc: JumpCandidate, x: Variable[V], xi: Int, a: V): JumpCandidate = {
        val move = new BulkMove(space.nextMoveId())
        move ++= acc.changes
        move += new ImmutableMoveEffect(x, a)
        val before = space.searchState
        val after = space.consult(move)
        var score = 0.0
        xi2cis(xi).inlineForeach(ci => {
            val c = cs(ci)
            val costDelta = after.value(c).violation - before.value(c).violation
            if (costDelta != 0) {
                if (scaleCostDeltas) {
                    costDeltaSampleSizes(ci) += 1.0
                    costDeltaAverages(ci) += (abs(costDelta) - costDeltaAverages(ci)) / costDeltaSampleSizes(ci)
                    score += weights(ci) * costDelta * costDeltaScalingFactors(ci)
                } else {
                    score += weights(ci) * costDelta
                }
            }
        })
        JumpCandidate(move.effects.toList, xi :: acc.xis, after.move, score)
    }

}

/**
 * Companion object to FeasibilityJumpNeighbourhood.
 *
 * @author Michael Marte
 */
object FeasibilityJumpNeighbourhood {

    private case class JumpCandidate(changes: List[AnyMoveEffect], xis: List[Int], move: Move, score: Double)

}
