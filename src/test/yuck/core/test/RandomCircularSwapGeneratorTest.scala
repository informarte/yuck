package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class RandomCircularSwapGeneratorTest
    (randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     maybeHotSpotDistribution: Option[Distribution],
     maybeFairVariableChoiceRate: Option[Probability],
     numberOfVariables: Int)
    extends UnitTest
{

    private val domains = for (i <- 0 until numberOfVariables) yield IntegerRange(0, numberOfVariables - 1)
    private val (space, xs) = NeighbourhoodTestHelper.createSpace(logger, sigint, randomGenerator, domains)
    val neighbourhood =
        new RandomCircularSwapGenerator(
            space, xs, randomGenerator, moveSizeDistribution, maybeHotSpotDistribution, maybeFairVariableChoiceRate)
    private val helper =
        new NeighbourhoodTestHelper(
            space, neighbourhood, xs, moveSizeDistribution, maybeHotSpotDistribution, maybeFairVariableChoiceRate, logger)

    @Test
    def testNeighbourhood(): Unit = {
        val result = helper.testMoveGeneration()
        helper.checkMoveSizeFrequencies(result, 0.1, 0)
        helper.checkVariableFrequencies(result, 0.1, 0)
    }

}

/**
 * @author Michael Marte
 *
 */
object RandomCircularSwapGeneratorTest extends NeighbourhoodTestGenerator {

    override protected val moveSizeDistributions =
        List(List(0, 90, 10), List(0, 50, 35, 10)).map(Distribution(1, _))

}
