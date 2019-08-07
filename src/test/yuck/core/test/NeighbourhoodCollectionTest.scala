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
class NeighbourhoodCollectionTest
    (randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     maybeHotSpotDistribution: Option[Distribution],
     maybeFairChoiceRate: Option[Probability],
     numberOfVariables: Int)
    extends UnitTest
{

    private val domains =
        for (i <- 0 until numberOfVariables) yield new IntegerRange(Zero, IntegerValue.get(numberOfVariables - 1))
    private val (space, xs) =
        NeighbourhoodTestHelper.createSpace(logger, sigint, randomGenerator, domains)
    private val helper =
        new NeighbourhoodTestHelper(logger, xs, moveSizeDistribution, maybeHotSpotDistribution, maybeFairChoiceRate)

    @Test
    def testNeighbourhood: Unit = {
        val neighbourhoods =
            for (i <- 0 until numberOfVariables) yield
                new SimpleRandomReassignmentGenerator(space, Vector(xs(i)), randomGenerator)
        val neighbourhood =
            new NeighbourhoodCollection(
                neighbourhoods, randomGenerator, maybeHotSpotDistribution, maybeFairChoiceRate)
        val result = helper.measure(neighbourhood)
        helper.checkVariableFrequencies(result, 0.1, 0.1)
    }

}

/**
 * @author Michael Marte
 *
 */
final object NeighbourhoodCollectionTest extends NeighbourhoodTestGenerator {

    import DistributionFactory.createDistribution

    protected override val moveSizeDistributions = List(List(100)).map(createDistribution(1, _))

}
