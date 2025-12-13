package yuck.core.test

import yuck.core.*

abstract class NeighbourhoodTestGenerator {

    protected val moveSizeDistributions: Seq[Distribution]
    protected val hotSpotDistributions: Seq[Seq[Int]] =
        List(List(25, 0, 5, 25, 50, 0, 15, 20, 0, 10))
    protected val fairVariableChoiceRates: Seq[Probability] =
        List(0, 10, 50, 100).map(Probability.apply)
    protected val numbersOfVariables: Seq[Int] =
        List(10)

    private def configurations =
        for moveSizeDistribution <- moveSizeDistributions
            maybeHotSpotDistribution <- None +: hotSpotDistributions.map(Some(_))
            fairVariableChoiceRate <- if maybeHotSpotDistribution.isDefined then fairVariableChoiceRates else List(Probability(100))
            numberOfVariables <- if maybeHotSpotDistribution.isDefined then List(maybeHotSpotDistribution.get.size) else numbersOfVariables
        yield
            Array(moveSizeDistribution, maybeHotSpotDistribution.map(Distribution(0, _)), Some(fairVariableChoiceRate), numberOfVariables)

    def parameters = configurations.toArray

}
