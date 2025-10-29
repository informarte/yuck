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
final class RandomReassignmentGeneratorTest
    (randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     maybeHotSpotDistribution: Option[Distribution],
     maybeFairVariableChoiceRate: Option[Probability],
     numberOfVariables: Int)
    extends UnitTest
{

    private val domains = for (i <- 0 until numberOfVariables) yield IntegerRange(0, numberOfVariables - 1)
    private val (space, xs) = NeighbourhoodTestHelper.createSpace(logger, sigint, randomGenerator, domains)
    private val neighbourhood =
        new RandomReassignmentGenerator(
            space, xs, randomGenerator, moveSizeDistribution, maybeHotSpotDistribution, maybeFairVariableChoiceRate)
    private val helper =
        new NeighbourhoodTestHelper(
            space, neighbourhood, xs, moveSizeDistribution, maybeHotSpotDistribution, maybeFairVariableChoiceRate, logger)

    @Test
    def testMoveGeneration(): Unit = {
        val result = helper.testMoveGeneration()
        helper.checkMoveSizeFrequencies(result, 0.1, 0)
        helper.checkVariableFrequencies(result, 0.1, 1)
    }

    @Test
    def testPerturbation(): Unit = {
        helper.testPerturbation()
    }

}

/**
 * @author Michael Marte
 *
 */
object RandomReassignmentGeneratorTest extends NeighbourhoodTestGenerator {

    override protected val moveSizeDistributions =
        List(List(100), List(90, 10), List(50, 35, 15), List(50, 25, 15, 10)).map(Distribution(1, _))

}
