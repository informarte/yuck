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

    private val domains =
        for (i <- 0 until numberOfVariables) yield IntegerRange(Zero, IntegerValue(numberOfVariables - 1))
    private val (space, xs) =
        NeighbourhoodTestHelper.createSpace(logger, sigint, randomGenerator, domains)
    private val helper =
        new NeighbourhoodTestHelper(logger, xs, moveSizeDistribution, maybeHotSpotDistribution, maybeFairVariableChoiceRate)

    @Test
    def testNeighbourhood: Unit = {
        val neighbourhood =
            new RandomReassignmentGenerator(
                space, xs, randomGenerator, moveSizeDistribution, maybeHotSpotDistribution, maybeFairVariableChoiceRate)
        val result = helper.measure(neighbourhood)
        helper.checkMoveSizeFrequencies(result, 0.1, 0)
        helper.checkVariableFrequencies(result, 0.1, 1)
    }

}

/**
 * @author Michael Marte
 *
 */
object RandomReassignmentGeneratorTest extends NeighbourhoodTestGenerator {

    protected override val moveSizeDistributions =
        List(List(100), List(90, 10), List(50, 35, 15), List(50, 25, 15, 10)).map(Distribution(1, _))

}
