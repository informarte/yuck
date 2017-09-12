package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CountConstTest extends UnitTest {

    @Test
    def testCountConst {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val n = space.createVariable("n", NonNegativeIntegerRange)
        val c = new CountConst(space.constraintIdFactory.nextId, null, List(s, t, u), One, n)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(n), Three)
        val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
        val after = space.consult(move)
        assertEq(now.value(s), One)
        assertEq(now.value(n), Three)
        assertEq(after.value(s), Two)
        assertEq(after.value(n), Two)
        space.commit(move)
        assertEq(now.value(s), Two)
        assertEq(now.value(n), Two)
        space.initialize
        assertEq(now.value(n), Two)
    }

}
