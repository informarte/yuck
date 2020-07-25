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
final class ElementVarTest extends UnitTest with StandardConstraintTestTooling[IntegerValue] {

    @Test
    def testArrayAccess: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val xs = immutable.IndexedSeq(s, t, u)
        val i = new IntegerVariable(space.nextVariableId, "i", d)
        val y = space.createVariable("y", NonNegativeIntegerRange)
        space.post(new ElementVar(space.nextConstraintId, null, xs, i, y, 1))
        assertEq(space.searchVariables, Set(s, t, u, i))
        runScenario(
            TestScenario(
                space,
                y,
                Initialize("setup", One, (s, One), (t, Two), (u, Three), (i, One)),
                ConsultAndCommit("1", Zero, (s, Zero)),
                ConsultAndCommit("2", Two, (i, Two))))
    }

}
