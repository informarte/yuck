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
final class SimpleRandomReassignmentGeneratorTest extends UnitTest {

    import DistributionFactory.createDistribution

    private val randomGenerator = new JavaRandomGenerator
    private val numberOfVariables = 10
    private val domains =
        for (i <- 0 until numberOfVariables) yield new IntegerRange(Zero, IntegerValue.get(numberOfVariables - 1))
    private val (space, xs) = NeighbourhoodTestHelper.createSpace(logger, randomGenerator, domains)
    private val helper = new NeighbourhoodTestHelper(logger, xs, createDistribution(1, List(100)), None, None)

    @Test
    def testNeighbourhood {
        val neighbourhood = new SimpleRandomReassignmentGenerator(space, xs, randomGenerator)
        val result = helper.measure(neighbourhood)
        helper.checkMoveSizeFrequencies(result, 0, 0)
        helper.checkVariableFrequencies(result, 0.1, 0)
    }

}
