package yuck.constraints.test

import org.junit._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class ReificationTest extends UnitTest {

    @Test
    def testReification {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val x = space.createVariable("x", new IntegerDomain(Zero, Nine))
        val b = space.createVariable("b", new BooleanDomain(true, true))
        val costs = space.createVariable("costs", UnboundedIntegerDomain)
        val c = new Reification(space.constraintIdFactory.nextId, null, x, b, costs)
        space.post(c)
        val now = space.searchState
        space.setValue(x, Zero).setValue(b, True).initialize
        assertEq(now.value(costs), Zero)
        space.setValue(x, Five).setValue(b, True).initialize
        assertEq(now.value(costs), Five)
        val move1 = new ChangeValue(space.moveIdFactory.nextId, x, Zero)
        val after1 = space.consult(move1)
        assertEq(now.value(costs), Five)
        assertEq(after1.value(costs), Zero)
        space.setValue(x, Zero).setValue(b, False).initialize
        assertEq(now.value(costs), One)
        space.setValue(x, Five).setValue(b, False).initialize
        assertEq(now.value(costs), Zero)
        val move2 = new ChangeValue(space.moveIdFactory.nextId, x, Zero)
        val after2 = space.consult(move2)
        assertEq(now.value(costs), Zero)
        assertEq(after2.value(costs), One)
    }

}
