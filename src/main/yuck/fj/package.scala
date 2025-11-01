package yuck

import yuck.core.{Distribution, Probability, ld, max}

/**
 * @author Michael Marte
 *
 */
package object fj {

    val DefaultMoveSizeDistribution = Distribution(1, List(80, 20))
    def defaultMaximumNumberOfJumpCandidates(n: Int): Int = if n <= 1 then n else ld(n).round.toInt
    val DefaultMinimumNumberOfValuesToExplore: Int = 8
    def defaultNumberOfValuesToExplore(n: Int): Int =
        max(DefaultMinimumNumberOfValuesToExplore, if n <= 1 then n else ld(n).round.toInt)
    val DefaultMinimumJumpValueCacheHitRate = 0.1
    val DefaultWeightDecayRate = 0.95 // from ViolationLS
    def defaultNumberOfMovesPerRound(n: Int): Int = if n <= 1 then n else (n / ld(n)).round.toInt
    val DefaultNumberOfSuccessiveFutileRoundsUntilPerturbation = 16 // ViolationLS uses 100
    val DefaultPerturbationProbability = Probability(0.1) // from ViolationLS

}
