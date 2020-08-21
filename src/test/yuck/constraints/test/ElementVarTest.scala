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
final class ElementVarTest(offset: Int) extends UnitTest with CostComputationTestTooling[IntegerValue] {

    @Test
    def testArrayAccess: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val xs = immutable.IndexedSeq(s, t, u)
        val indexRange = IntegerDomain.createRange(IntegerValue.get(offset), IntegerValue.get(offset + 2))
        val i = new IntegerVariable(space.nextVariableId, "i", indexRange)
        val y = space.createVariable("y", NonNegativeIntegerRange)
        space.post(new ElementVar(space.nextConstraintId, null, xs, i, y, offset))
        assertEq(space.searchVariables, Set(s, t, u, i))
        runScenario(
            CostComputationTestScenario(
                space,
                y,
                Initialize("setup", One, (s, One), (t, Two), (u, Three), (i, IntegerValue.get(offset))),
                ConsultAndCommit("1", Zero, (s, Zero)),
                ConsultAndCommit("2", Two, (i, IntegerValue.get(offset + 1)))))
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
