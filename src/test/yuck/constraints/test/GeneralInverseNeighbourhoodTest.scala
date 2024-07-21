package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.{GeneralInverseNeighbourhood, InverseFunction}
import yuck.core.*

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
final class GeneralInverseNeighbourhoodTest(fOffset: Int, gOffset: Int) extends InverseNeighbourhoodTest {

    private val domainSize = 10

    private val fDomain = IntegerRange(gOffset, gOffset + domainSize - 1)
    private val xs = for (i <- 0 until domainSize) yield {
        val dx = if (i == 0) IntegerDomain(List(gOffset + domainSize - 1)) else fDomain.diff(IntegerDomain(List(gOffset + i)))
        new IntegerVariable(space.nextVariableId(), "x%d".format(i + 1), dx)
    }
    override protected val f = new InverseFunction(xs, fOffset)

    private val gDomain = IntegerRange(fOffset, fOffset + domainSize - 1)
    private val ys = for (i <- 0 until domainSize) yield {
        val dy = if (i == domainSize - 1) IntegerDomain(List(fOffset)) else gDomain.diff(IntegerDomain(List(fOffset + i)))
        new IntegerVariable(space.nextVariableId(), "y%d".format(i + 1), dy)
    }
    override protected val g = new InverseFunction(ys, gOffset)

    override protected val expectedNeighbourhoodClass = classOf[GeneralInverseNeighbourhood]

}

/**
 * @author Michael Marte
 *
 */
object GeneralInverseNeighbourhoodTest {

    private def offsets = List(-1, 0, 1).map(Integer.valueOf)
    private def configurations = for (fOffset <- offsets; gOffset <- offsets) yield Vector(fOffset, gOffset)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
