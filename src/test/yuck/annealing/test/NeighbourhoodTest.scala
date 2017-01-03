package yuck.annealing.test

import org.junit._

import scala.collection._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class NeighbourhoodTest extends UnitTest {

    @Test
    def testSimpleRandomNeighbourhood {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        space.setValue(s, Zero)
        val xs = immutable.IndexedSeq(s)
        val neighbourhood = new SimpleRandomReassignmentGenerator(space, xs, new JavaRandomGenerator)
        for (i <- 1 to 10000) {
            val effects = neighbourhood.nextMove.effects
            assert(effects.toIterator.hasNext)
            val effect = effects.toIterator.next
            assertEq(effect.anyVariable, s)
            assert(d.contains(effect.anyValue.asInstanceOf[IntegerValue]))
        }
    }

}
