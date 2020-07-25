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
final class ElementConstTest extends UnitTest with StandardConstraintTestTooling[IntegerValue] {

    @Test
    def testArrayAccess: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", ZeroToZeroIntegerRange)
        val t = space.createVariable("t", OneToOneIntegerRange)
        val as = immutable.IndexedSeq(Zero, One, Two)
        val i = new IntegerVariable(space.nextVariableId, "i", d)
        val y = space.createVariable("y", NonNegativeIntegerRange)
        space.post(new ElementConst(space.nextConstraintId, null, as, i, y, 1))
        assertEq(space.searchVariables, Set(i))
        runScenario(
            TestScenario(
                space,
                y,
                Initialize("setup", Zero, (i, One)),
                ConsultAndCommit("1", One, (i, Two)),
                ConsultAndCommit("2", Two, (i, Three))))
    }

}
