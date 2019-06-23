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
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val n = new IntegerVariable(space.nextVariableId, "n", NonNegativeIntegerRange)
        val c = new CountConst(space.nextConstraintId, null, List(s, t, u), One, n)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(n), Three)
        val move = new ChangeValue(space.nextMoveId, s, Two)
        val after = space.consult(move)
        assertEq(after.value(n), Two)
        space.commit(move)
        assertEq(now.value(n), Two)
        space.initialize
        assertEq(now.value(n), Two)
    }

}
