package yuck.constraints.test

import org.junit.*

import yuck.constraints.{InverseFunction, SelfInverseNeighbourhood}
import yuck.core.*

@runner.RunWith(classOf[runners.Parameterized])
final class SelfInverseNeighbourhoodTest(fOffset: Int) extends InverseNeighbourhoodTest {

    private val domainSize = 10

    private val fDomain = IntegerRange(fOffset, fOffset + domainSize - 1)
    private val xs = for i <- 1 to domainSize yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), fDomain)
    override protected val f = new InverseFunction(xs, fOffset)
    override protected val g = f

    override protected val expectedNeighbourhoodClass = classOf[SelfInverseNeighbourhood]

}

object SelfInverseNeighbourhoodTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = Array(-1, 0, 1).map(Int.box)

}
