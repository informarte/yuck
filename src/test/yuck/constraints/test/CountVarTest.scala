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
final class CountVarTest extends UnitTest with StandardConstraintTestTooling[IntegerValue] {

    @Test
    def testCountVar: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val what = space.createVariable("what", d)
        val n = new IntegerVariable(space.nextVariableId, "n", NonNegativeIntegerRange)
        space.post(new CountVar(space.nextConstraintId, null, List(s, t, u), what, n))
        assertEq(space.searchVariables, Set(s, t, u, what))
        runScenario(
            TestScenario(
                space,
                n,
                Initialize("setup", Three, (s, One), (t, One), (u, One), (what, One)),
                ConsultAndCommit("1", Two, (s, Two)),
                ConsultAndCommit("2", One, (what, Two))))
    }

}
