package yuck.constraints.test

import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.constraints.{InverseFunction, SimpleInverseNeighbourhood}
import yuck.core.*

@ParameterizedClass
@MethodSource(Array("parameters"))
final class SimpleInverseNeighbourhoodTest(fOffset: Int, gOffset: Int) extends InverseNeighbourhoodTest {

    private val domainSize = 100

    private val fDomain = IntegerRange(gOffset, gOffset + domainSize - 1)
    private val xs = for i <- 1 to domainSize yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), fDomain)
    override protected val f = new InverseFunction(xs, fOffset)

    private val gDomain = IntegerRange(fOffset, fOffset + domainSize - 1)
    private val ys = for i <- 1 to domainSize yield new IntegerVariable(space.nextVariableId(), "y%d".format(i), gDomain)
    override protected val g = new InverseFunction(ys, gOffset)

    override protected val expectedNeighbourhoodClass = classOf[SimpleInverseNeighbourhood]

}

object SimpleInverseNeighbourhoodTest {

    private def offsets = List(-1, 0, 1).map(Integer.valueOf)
    private def configurations = for fOffset <- offsets; gOffset <- offsets yield Array(fOffset, gOffset)

    def parameters = configurations.toArray

}
