package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.{InverseFunction, SelfInverseNeighbourhood}
import yuck.core.*

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
final class SelfInverseNeighbourhoodTest(fOffset: Int) extends InverseNeighbourhoodTest {

    private val DomainSize = 10

    private val fDomain = IntegerRange(fOffset, fOffset + DomainSize - 1)
    private val xs = for (i <- 1 to DomainSize) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), fDomain)
    override protected val f = new InverseFunction(xs, fOffset)
    override protected val g = f

    override protected val expectedNeighbourhoodClass = classOf[SelfInverseNeighbourhood]

}

/**
 * @author Michael Marte
 *
 */
object SelfInverseNeighbourhoodTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(-1, 0, 1).asJava

}
