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
final class MaximumTest extends UnitTest with StandardConstraintTestTooling[IntegerValue] {

    @Test
    def testMaximum: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val max = new IntegerVariable(space.nextVariableId, "costs", CompleteIntegerRange)
        space.post(new Maximum(space.nextConstraintId, null, List(s, t, u), max))
        assertEq(space.searchVariables, Set(s, t, u))
        runScenario(
            TestScenario(
                space,
                max,
                Initialize("setup", Three, (s, One), (t, Two), (u, Three)),
                ConsultAndCommit("1", Two, (u, Two)),
                ConsultAndCommit("2", Three, (s, Three))))
    }

}
