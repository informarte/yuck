package yuck.constraints.test

import org.junit._

import scala.jdk.CollectionConverters._

import yuck.constraints.{InverseFunction, SimpleInverseNeighbourhood}
import yuck.core._

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class SimpleInverseNeighbourhoodTest(fOffset: Int, gOffset: Int) extends InverseNeighbourhoodTest {

    @Test
    def testMoveGeneration: Unit = {
        val domainSize = 100
        val fDomain = IntegerRange(gOffset, gOffset + domainSize - 1)
        val gDomain = IntegerRange(fOffset, fOffset + domainSize - 1)
        val xs = for (i <- 1 to domainSize) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), fDomain)
        val ys = for (i <- 1 to domainSize) yield new IntegerVariable(space.nextVariableId, "y%d".format(i), gDomain)
        val f = new InverseFunction(xs, fOffset)
        val g = new InverseFunction(ys, gOffset)
        testMoveGeneration(f, g, 100, classOf[SimpleInverseNeighbourhood])
    }

}

/**
 * @author Michael Marte
 *
 */
object SimpleInverseNeighbourhoodTest {

    private def offsets = List(-1, 0, 1).map(Integer.valueOf)
    private def configurations = for (fOffset <- offsets; gOffset <- offsets) yield Vector(fOffset, gOffset)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
