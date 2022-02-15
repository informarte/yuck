package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class SimpleRandomReassignmentGeneratorTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val numberOfVariables = 10
    private val domains =
        for (i <- 0 until numberOfVariables) yield IntegerRange(Zero, IntegerValue(numberOfVariables - 1))
    private val (space, xs) = NeighbourhoodTestHelper.createSpace(logger, sigint, randomGenerator, domains)
    private val helper = new NeighbourhoodTestHelper(logger, xs, Distribution(1, List(100)), None, None)

    @Test
    def testNeighbourhood(): Unit = {
        val neighbourhood = new SimpleRandomReassignmentGenerator(space, xs, randomGenerator)
        val result = helper.measure(neighbourhood)
        helper.checkMoveSizeFrequencies(result, 0, 0)
        helper.checkVariableFrequencies(result, 0.1, 0)
    }

}
