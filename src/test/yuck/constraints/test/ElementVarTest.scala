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
final class ElementVarTest(offset: Int) extends UnitTest with AssignmentPropagationTestTooling {

    @Test
    def testArrayAccess: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val xs = immutable.IndexedSeq(s, t, u)
        val indexRange = IntegerRange(IntegerValue(offset), IntegerValue(offset + 2))
        val i = new IntegerVariable(space.nextVariableId, "i", indexRange)
        val y = space.createVariable("y", NonNegativeIntegerRange)
        space.post(new ElementVar(space.nextConstraintId, null, xs, i, y, offset))
        assertEq(space.searchVariables, Set(s, t, u, i))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", (s, One), (t, Two), (u, Three), (i, IntegerValue(offset)), (y, One)),
                ConsultAndCommit("1", (s, Zero), (y, Zero)),
                ConsultAndCommit("2", (i, IntegerValue(offset + 1)), (y, Two))))
    }

}

/**
 * @author Michael Marte
 *
 */
object ElementVarTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(-1, 0, 1).asJava

}
