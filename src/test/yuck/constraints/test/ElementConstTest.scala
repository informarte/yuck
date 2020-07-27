package yuck.constraints.test

import org.junit._

import scala.collection._
import scala.jdk.CollectionConverters._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class ElementConstTest(offset: Int) extends UnitTest with StandardConstraintTestTooling[IntegerValue] {

    @Test
    def testArrayAccess: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", ZeroToZeroIntegerRange)
        val t = space.createVariable("t", OneToOneIntegerRange)
        val as = immutable.IndexedSeq(Zero, One, Two)
        val indexRange = IntegerDomain.createRange(IntegerValue.get(offset), IntegerValue.get(offset + 2))
        val i = new IntegerVariable(space.nextVariableId, "i", indexRange)
        val y = space.createVariable("y", NonNegativeIntegerRange)
        space.post(new ElementConst(space.nextConstraintId, null, as, i, y, offset))
        assertEq(space.searchVariables, Set(i))
        runScenario(
            TestScenario(
                space,
                y,
                Initialize("setup", Zero, (i, IntegerValue.get(offset))),
                ConsultAndCommit("1", One, (i, IntegerValue.get(offset + 1))),
                ConsultAndCommit("2", Two, (i, IntegerValue.get(offset + 2)))))
    }

}

/**
 * @author Michael Marte
 *
 */
object ElementConstTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(-1, 0, 1).asJava

}
