package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.{InverseFunction, SelfInverseNeighbourhood}
import yuck.core.*

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class SelfInverseNeighbourhoodTest(fOffset: Int) extends InverseNeighbourhoodTest {

    @Test
    def testMoveGeneration(): Unit = {
        val domainSize = 10
        val fDomain = IntegerRange(fOffset, fOffset + domainSize - 1)
        val xs = for (i <- 1 to domainSize) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), fDomain)
        val f = new InverseFunction(xs, fOffset)
        testMoveGeneration(f, f, 1000, classOf[SelfInverseNeighbourhood])
    }

}

/**
 * @author Michael Marte
 *
 */
object SelfInverseNeighbourhoodTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(-1, 0, 1).asJava

}
