package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
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
        for (i <- 0 until numberOfVariables) yield new IntegerRange(Zero, IntegerValue.get(numberOfVariables - 1))
    private val (space, xs) =
        NeighbourhoodTestHelper.createSpace(logger, randomGenerator, domains)
    private val helper =
        new NeighbourhoodTestHelper(logger, xs, moveSizeDistribution, maybeHotSpotDistribution, maybeFairVariableChoiceRate)

    @Test
    def testNeighbourhood {
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
final object RandomReassignmentGeneratorTest extends NeighbourhoodTestGenerator {

    import DistributionFactory.createDistribution

    protected override val moveSizeDistributions =
        List(List(100), List(90, 10), List(50, 35, 15), List(50, 25, 15, 10)).map(createDistribution(1, _))

}
