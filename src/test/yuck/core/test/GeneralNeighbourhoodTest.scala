package yuck.core.test

import scala.collection.*

import org.junit.jupiter.api.{BeforeEach, Test}

import yuck.core.*
import yuck.test.util.UnitTest

abstract class GeneralNeighbourhoodTest
    [V <: Value[V]]
    (using valueTraits: ValueTraits[V])
    extends UnitTest
{

    protected val randomGenerator = new JavaRandomGenerator
    protected val space = new Space(logger, sigint)

    protected val xs: immutable.IndexedSeq[Variable[V]]
    protected val moveSizeDistribution: Distribution
    protected val maybeHotSpotDistribution: Option[Distribution] // goes together with xs
    protected val maybeFairChoiceRate: Option[Probability]
    private lazy val fairChoiceRate = maybeFairChoiceRate.getOrElse(Probability(1.0)).value
    protected lazy val neighbourhood: Neighbourhood

    @BeforeEach
    def setup(): Unit = {
        require(! xs.isEmpty)
        require(xs.toSet.size == xs.size)
        for x <- xs do {
            space.setValue(x, x.domain.randomValue(randomGenerator))
        }
        space.post(new DummyConstraint(space.nextConstraintId(), xs, Nil))
        space.initialize()
        require(xs.toSet == neighbourhood.searchVariables)
        require(moveSizeDistribution.frequency(0) == 0)
        require(moveSizeDistribution.volume > 0)
        require(maybeHotSpotDistribution.isEmpty || maybeHotSpotDistribution.get.size == xs.size)
        val fairChoiceRate = maybeFairChoiceRate.getOrElse(Probability(1.0)).value
        require(fairChoiceRate >= 0 && fairChoiceRate <= 1)
        require(maybeHotSpotDistribution.isDefined || fairChoiceRate == 1)
    }

    final class MeasurementResult {
        val moveSizeFrequencies = new Array[Int](moveSizeDistribution.size)
        val variableFrequencies = mutable.HashMap.from(xs.map(_ -> 0))
    }

    final class AcceptableDeviation(val tolerance: Double, val maxFailureRate: Double)

    protected val numberOfMoves = 10000
    protected val acceptableMoveSizeFrequencyDeviation: AcceptableDeviation
    protected val acceptableVariableFrequencyDeviation: AcceptableDeviation

    @Test
    def testMoveGeneration(): Unit = {
        val result = observeMoveGeneration()
        checkMoveSizeFrequencies(
            result,
            acceptableMoveSizeFrequencyDeviation.tolerance, acceptableMoveSizeFrequencyDeviation.maxFailureRate)
        checkVariableFrequencies(
            result,
            acceptableVariableFrequencyDeviation.tolerance, acceptableVariableFrequencyDeviation.maxFailureRate)
    }

    protected val numberOfPerturbations = 1000
    protected val perturbationProbability = Probability(0.5)

    @Test
    def testPerturbation(): Unit = {
        val now = space.searchState
        val numbersOfChangedAssignments = new mutable.ArrayBuffer[Int](numberOfPerturbations)
        for i <- 0 until numberOfPerturbations do {
            val before = now.clone()
            neighbourhood.perturb(perturbationProbability)
            numbersOfChangedAssignments += xs.count(x => before.value(x) != now.value(x))
        }
        val avg = numbersOfChangedAssignments.sum.toDouble / numberOfPerturbations
        assertGt(avg, xs.size * perturbationProbability.value * 0.9)
        assertLt(avg, xs.size * perturbationProbability.value * 1.1)
    }

    private def observeMoveGeneration(): MeasurementResult = {
        val result = new MeasurementResult
        for i <- 1 to numberOfMoves do {
            val move = neighbourhood.nextMove()
            result.moveSizeFrequencies(move.size) += 1
            val ys = move.involvedVariablesIterator.map(valueTraits.safeDowncast).toVector
            assertEq(ys.size, ys.toSet.size)
            assert(ys.forall(xs.contains))
            for y <- ys do {
                assert(y.domain.contains(move.value(y)))
                result.variableFrequencies(y) += 1
            }
        }
        result
    }

    private def checkMoveSizeFrequencies(result: MeasurementResult, tolerance: Double, maxFailureRate: Double): Unit = {
        // checkMoveSizeFrequency(n) is true iff the observed frequency of moves of size n does not differ widely
        // from the frequency stipulated by moveSizeDistribution.
        def checkMoveSizeFrequency(n: Int): Boolean = {
            val observation = result.moveSizeFrequencies(n).toDouble
            val expectation = numberOfMoves * moveSizeDistribution.probability(n).value
            val ok = observation >= expectation * (1 - tolerance) && observation <= expectation * (1 + tolerance)
            if ! ok then {
                logger.log("moveSizeFrequencies = %s".format(result.moveSizeFrequencies.toVector))
                logger.log("moveSize = %s".format(n))
                logger.log("observation = %s".format(observation))
                logger.log("expectation = %s".format(expectation))
                logger.log("----------")
            }
            ok
        }
        val failureCount = (1 until moveSizeDistribution.size).iterator.map(checkMoveSizeFrequency).count(! _)
        import scala.math.Ordering.Double.TotalOrdering
        assertLe(failureCount.toDouble, moveSizeDistribution.size * maxFailureRate)
    }

    private def checkVariableFrequencies(result: MeasurementResult, tolerance: Double, maxFailureRate: Double): Unit = {
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
                    if j == n - 1
                    then hotSpotDistribution.probability(i).value
                    else {
                        // Q(k) is the probability of choosing xs(i) on some position l with j <= l < n
                        // under the assumption that xs(k) gets chosen on position j.
                        def Q(k: Int): Double =
                            // consider xs(k) for position j
                            if k == i
                            then hotSpotDistribution.probability(i).value
                            else {
                                hotSpotDistribution.probability(k).value * {
                                    val f = hotSpotDistribution.frequency(k)
                                    hotSpotDistribution.setFrequency(k, 0)
                                    val p = P(j + 1)
                                    hotSpotDistribution.setFrequency(k, f)
                                    p
                                }
                            }
                        xs.indices.iterator.map(Q).sum
                    }
                val p =
                    (if fairChoiceRate > 0 then fairChoiceRate * h(1, xs.size, 1, n) else 0) +
                        (if fairChoiceRate < 1 then (1 - fairChoiceRate) * P(0) else 0)
                result.moveSizeFrequencies(n) * p
            }
            val observation = result.variableFrequencies(xs(i))
            val expectation = (1 until moveSizeDistribution.size).iterator.map(E).sum
            if expectation == 0 then {
                // A forbidden variable must not be chosen!
                assertEq(observation, expectation)
                true
            } else {
                val ok = observation >= expectation * (1 - tolerance) && observation <= expectation * (1 + tolerance)
                if ! ok then {
                    logger.log("variableFrequencies = %s".format(xs.map(result.variableFrequencies)))
                    logger.log("x = %s".format(xs(i)))
                    logger.log("observation = %s".format(observation))
                    logger.log("expectation = %s".format(expectation))
                    logger.log("----------")
                }
                ok
            }
        }
        val failureCount = xs.indices.iterator.map(checkVariableFrequency).count(! _)
        import scala.math.Ordering.Double.TotalOrdering
        assertLe(failureCount.toDouble, xs.size * maxFailureRate)
    }

}
