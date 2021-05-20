package yuck.constraints.test

import org.junit._

import scala.collection._
import scala.jdk.CollectionConverters._

import yuck.constraints._
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class ElementConstTest(offset: Int) extends UnitTest with AssignmentPropagationTestTooling {

    @Test
    def testArrayAccess: Unit = {
        val space = new Space(logger, sigint)
        val as = immutable.IndexedSeq(Zero, One, Two)
        val indexRange = IntegerRange(IntegerValue(offset), IntegerValue(offset + 2))
        val i = new IntegerVariable(space.nextVariableId, "i", indexRange)
        val y = space.createVariable("y", NonNegativeIntegerRange)
        space.post(new ElementConst(space.nextConstraintId, null, as, i, y, offset))
        assertEq(space.searchVariables, Set(i))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", (i, IntegerValue(offset)), (y, Zero)),
                ConsultAndCommit("1", (i, IntegerValue(offset - 1)), (y, Zero)),
                ConsultAndCommit("2", (i, IntegerValue(offset + 1)), (y, One)),
                ConsultAndCommit("3", (i, IntegerValue(offset + 2)), (y, Two)),
                ConsultAndCommit("4", (i, IntegerValue(offset + 3)), (y, Two))))
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
