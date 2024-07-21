package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.{InverseFunction, SimpleInverseNeighbourhood}
import yuck.core.*

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
final class SimpleInverseNeighbourhoodTest(fOffset: Int, gOffset: Int) extends InverseNeighbourhoodTest {

    private val DomainSize = 100

    private val fDomain = IntegerRange(gOffset, gOffset + DomainSize - 1)
    private val xs = for (i <- 1 to DomainSize) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), fDomain)
    override protected val f = new InverseFunction(xs, fOffset)

    private val gDomain = IntegerRange(fOffset, fOffset + DomainSize - 1)
    private val ys = for (i <- 1 to DomainSize) yield new IntegerVariable(space.nextVariableId(), "y%d".format(i), gDomain)
    override protected val g = new InverseFunction(ys, gOffset)

    override protected val expectedNeighbourhoodClass = classOf[SimpleInverseNeighbourhood]

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
