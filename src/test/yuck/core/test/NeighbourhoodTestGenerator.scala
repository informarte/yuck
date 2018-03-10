package yuck.core.test

import scala.collection._
import scala.collection.JavaConverters._

import org.junit._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
abstract class NeighbourhoodTestGenerator {

    import DistributionFactory.createDistribution

    private val randomGenerator = new JavaRandomGenerator

    protected val moveSizeDistributions: Seq[Distribution]
    protected val hotSpotDistributions: Seq[Distribution] =
        List(List(25, 0, 5, 25, 50, 0, 15, 20, 0, 10)).map(createDistribution(0, _))
    protected val fairVariableChoiceRates: Seq[Probability] =
        List(0, 10, 50, 100).map(Probability.from)
    protected val numbersOfVariables: Seq[Int] =
        List(10)

    private def configurations =
        for (moveSizeDistribution <- moveSizeDistributions;
             maybeHotSpotDistribution <- None +: hotSpotDistributions.map(Some(_));
             fairVariableChoiceRate <-
                 if (maybeHotSpotDistribution.isDefined) fairVariableChoiceRates else List(Probability.from(100));
             numberOfVariables <-
                 if (maybeHotSpotDistribution.isDefined) List(maybeHotSpotDistribution.get.size) else numbersOfVariables)
            yield Vector(
                    randomGenerator.nextGen, moveSizeDistribution, maybeHotSpotDistribution, Some(fairVariableChoiceRate),
                    numberOfVariables)

    @runners.Parameterized.Parameters(name = "{index}: {1}, {2}, {3}, {4}")
    def parameters = configurations.map(_.toArray).asJava

}
