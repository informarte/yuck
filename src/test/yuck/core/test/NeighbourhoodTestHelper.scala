package yuck.core.test

import scala.collection._

import yuck.core._
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger
import yuck.util.testing.YuckAssert

/**
 * @author Michael Marte
 *
 */
final class NeighbourhoodTestHelper
    [Value <: AnyValue]
    (logger: LazyLogger,
     xs: IndexedSeq[Variable[Value]],
     moveSizeDistribution: Distribution,
     maybeHotSpotDistribution: Option[Distribution], // goes together with xs
     maybeFairVariableChoiceRate: Option[Probability])
    (implicit valueTraits: ValueTraits[Value])
    extends YuckAssert
{

    require(! xs.isEmpty)
    require(xs.toSet.size == xs.size)
    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)
    require(maybeHotSpotDistribution.isEmpty || maybeHotSpotDistribution.get.size == xs.size)
    private val fairVariableChoiceRate =
        maybeFairVariableChoiceRate.getOrElse(Probability.from(1.0)).value
    require(fairVariableChoiceRate >= 0 && fairVariableChoiceRate <= 1)
    require(maybeHotSpotDistribution.isDefined || fairVariableChoiceRate == 1)

    final class MeasurementResult {
        val moveSizeFrequencies = new Array[Int](moveSizeDistribution.size)
        val variableFrequencies = new mutable.HashMap[Variable[Value], Int] ++ xs.map(_ -> 0)
    }

    private val sampleSize = 10000

    def measure(neighbourhood: Neighbourhood): MeasurementResult = {
        require(neighbourhood.searchVariables == xs.toSet)
        val result = new MeasurementResult
        for (i <- 1 to sampleSize) {
            val move = neighbourhood.nextMove
            result.moveSizeFrequencies(move.size) += 1
            val ys = move.involvedVariables.map(valueTraits.safeDowncast).toBuffer
            assertEq(ys.size, ys.toSet.size)
            assert(ys.forall(xs.contains))
            for (y <- ys) {
                assert(y.domain.contains(valueTraits.safeDowncast(move.anyValue(y))))
                result.variableFrequencies(y) += 1
            }
        }
        result
    }

    def checkMoveSizeFrequencies(result: MeasurementResult, tolerance: Double, maxFailureRate: Double): Unit = {
        // checkMoveSizeFrequency(n) is true iff the observed frequency of moves of size n does not differ widely
        // from the frequency stipulated by moveSizeDistribution.
        def checkMoveSizeFrequency(n: Int): Boolean = {
            val observation = result.moveSizeFrequencies(n).toDouble
            val expectation = sampleSize * moveSizeDistribution.probability(n).value
            val ok = observation >= expectation * (1 - tolerance) && observation <= expectation * (1 + tolerance)
            if (! ok) {
                logger.log("moveSizeFrequencies = %s".format(result.moveSizeFrequencies.toVector))
                logger.log("moveSize = %s".format(n))
                logger.log("observation = %s".format(observation))
                logger.log("expectation = %s".format(expectation))
                logger.log("----------")
            }
            ok
        }
        val failureCount = (1 until moveSizeDistribution.size).iterator.map(checkMoveSizeFrequency).count(! _)
        assertLe(failureCount, moveSizeDistribution.size * maxFailureRate)
    }

    def checkVariableFrequencies(result: MeasurementResult, tolerance: Double, maxFailureRate: Double): Unit = {
        lazy val hotSpotDistribution = maybeHotSpotDistribution.get
        // checkVariableFrequency(i) is true iff the observed frequency of xs(i) does not differ widely
        // from the frequency stipulated by hotSpotDistribution.
        def checkVariableFrequency(i: Int): Boolean = {
            // E(n) is the expected number of moves of size n that affect x(i).
            def E(n: Int): Double = {
                // So let's compute the probability of creating a move of size n that affects xs(i).
                // We have to consider both fair and biased move generation.
                // For fair move generation, we use the hypergeometric distribution.
                // https://math.stackexchange.com/a/202559
                // https://en.wikipedia.org/wiki/Binomial_coefficient#Generalization_and_connection_to_the_binomial_series
                def binCoeff(n: Int, k: Int): Double =
                    (0 until k).iterator.map(i => (n - i).toDouble / (k - i).toDouble).product
                def h(k: Int, N: Int, M: Int, n: Int): Double =
                    binCoeff(M, k) * binCoeff(N - M, n - k) / binCoeff(N, n)
                // For biased move generation, we consider the move to consist of n positions that get populated
                // with variables from left to right.
                // P(j) is the probability of choosing xs(i) on some position k with 0 <= j <= k < n
                // (considering the current state of hotSpotDistribution).
                def P(j: Int): Double =
                    if (j == n - 1) hotSpotDistribution.probability(i).value
                    else {
                        // Q(k) is the probability of choosing xs(i) on some position l with j <= l < n
                        // under the assumption that xs(k) gets chosen on position j.
                        def Q(k: Int): Double =
                            // consider xs(k) for position j
                            if (k == i) hotSpotDistribution.probability(i).value
                            else {
                                hotSpotDistribution.probability(k).value * {
                                    val f = hotSpotDistribution.frequency(k)
                                    hotSpotDistribution.setFrequency(k, 0)
                                    val p = P(j + 1)
                                    hotSpotDistribution.setFrequency(k, f)
                                    p
                                }
                            }
                        (0 until xs.size).iterator.map(Q).sum
                    }
                val p =
                    (if (fairVariableChoiceRate > 0) fairVariableChoiceRate * h(1, xs.size, 1, n) else 0) +
                        (if (fairVariableChoiceRate < 1) (1 - fairVariableChoiceRate) * P(0) else 0)
                result.moveSizeFrequencies(n) * p
            }
            val observation = result.variableFrequencies(xs(i))
            val expectation = (1 until moveSizeDistribution.size).iterator.map(E).sum
            if (expectation == 0) {
                // A forbidden variable must not be chosen!
                assertEq(observation, expectation)
                true
            } else {
                val ok = observation >= expectation * (1 - tolerance) && observation <= expectation * (1 + tolerance)
                if (! ok) {
                    logger.log("variableFrequencies = %s".format(xs.map(result.variableFrequencies)))
                    logger.log("x = %s".format(xs(i)))
                    logger.log("observation = %s".format(observation))
                    logger.log("expectation = %s".format(expectation))
                    logger.log("----------")
                }
                ok
            }
        }
        val failureCount = (0 until xs.size).iterator.map(checkVariableFrequency).count(! _)
        assertLe(failureCount, xs.size * maxFailureRate)
    }

}

/**
 * @author Michael Marte
 *
 */
final object NeighbourhoodTestHelper {

    def createSpace
        (logger: LazyLogger, sigint: Sigint, randomGenerator: RandomGenerator, domains: Seq[IntegerDomain]):
        (Space, immutable.IndexedSeq[IntegerVariable]) =
    {
        val space = new Space(logger, sigint)
        val xs =
            for ((i, domain) <- (0 until domains.size).zip(domains)) yield {
                val x = new IntegerVariable(space.nextVariableId, "x%d".format(i), domain)
                space.setValue(x, x.domain.randomValue(randomGenerator))
                x
            }
        space.post(new DummyConstraint(space.nextConstraintId, xs, Nil))
        (space, xs)
    }

}
