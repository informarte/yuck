package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.*
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.*
import yuck.test.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class ElementConstTest(offset: Int) extends UnitTest with ConstraintTestTooling {

    private val Values = Vector(Four, Seven, Two)

    private val space = new Space(logger, sigint)

    private val indexRange = IntegerRange(offset, offset + 2)
    private val i = new IntegerVariable(space.nextVariableId(), "i", CompleteIntegerRange)
    private val y = new IntegerVariable(space.nextVariableId(), "y", CompleteIntegerRange)
    private val constraint = new ElementConst(space.nextConstraintId(), null, Values, i, y, offset)

    @Test
    def testBasics(): Unit = {
        assertEq(constraint.toString, "y = element(i, [4, 7, 2], %d)".format(offset))
        assertEq(constraint.inVariables.size, 1)
        assertEq(constraint.inVariables.head, i)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, y)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, List(i << indexRange, y << IntegerDomain(Values))),
                PropagateAndRollback("reduce domain of i", List(i << List(offset, offset + 2)), List(y << List(2, 4))),
                PropagateAndRollback("reduce domain of y", List(y << List(7)), List(i << List(offset + 1))),
                Propagate(
                    "reduce domains of i and y",
                    List(i << (offset, offset + 1), y << List(2, 7)),
                    List(i << List(offset + 1), y << List(7)))))
    }

    @Test
    def testArrayAccess(): Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("offset - 1", i << offset - 1, y << 4),
                Initialize("offset",     i << offset,     y << 4),
                Initialize("offset + 1", i << offset + 1, y << 7),
                Initialize("offset + 3", i << offset + 2, y << 2),
                Initialize("offset + 3", i << offset + 3, y << 2)))
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
