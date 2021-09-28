package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.constraints._
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CountConstTest extends UnitTest with ConstraintTestTooling {

    @Test
    def testCountConst: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val n = new IntegerVariable(space.nextVariableId, "n", NonNegativeIntegerRange)
        space.post(new CountConst(space.nextConstraintId, null, List(s, t, u), One, n))
        assertEq(space.searchVariables, Set(s, t, u))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", (s, One), (t, One), (u, One), (n, Three)),
                ConsultAndCommit("1", (s, Two), (n, Two)),
                ConsultAndCommit("2", (s, One), (u, Two), (n, Two)),
                ConsultAndCommit("3", (u, One), (n, Three))))
    }

}
