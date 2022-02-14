package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.*
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class ElementVarTest(offset: Int) extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val indexRange = IntegerRange(offset, offset + 2)
    private val i = new IntegerVariable(space.nextVariableId, "i", CompleteIntegerRange)
    private val y = new IntegerVariable(space.nextVariableId, "y", CompleteIntegerRange)
    private val constraint = new ElementVar(space.nextConstraintId, null, xs, i, y, offset)

    @Test
    def testBasics: Unit = {
        assertEq(constraint.toString, "y = element(i, [x1, x2, x3], %d)".format(offset))
        assertEq(constraint.inVariables.size, 4)
        assertEq(constraint.inVariables.toSet, Set(x1, x2, x3, i))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, y)
    }

    @Test
    def testPropagation: Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, List(i << indexRange, y << baseDomain)),
                Propagate("reduce domains of x1, x2, and x3", List(x1 << (0, 3), x2 << (3, 6), x3 << (6, 9)), Nil),
                PropagateAndRollback("reduce domains of x1 and x3", List(x1 << (1, 2), x3 << (7, 8)), List(y << (1, 8))),
                PropagateAndRollback(
                    "reduce domain of i",
                    List(i << List(offset, offset + 2)),
                    List(y << IntegerRange(0, 3).union(IntegerRange(6, 9)))),
                PropagateAndRollback(
                    "reduce domain of y",
                    List(y << List(1, 8)),
                    List(i << List(offset, offset + 2))),
                PropagateAndRollback(
                    "reduce domains of i and y",
                    List(i << (offset, offset + 1), y << (5, 9)),
                    List(i << List(offset + 1), y << (5, 6))),
                PropagateAndRollback(
                    "reduce domains of x1 and i",
                    List(x1 << (1, 3), x2 << (3, 5), i << (offset, offset + 1)),
                    List(y << (1, 5))),
                Propagate("reduce domains of x2 and y",
                    List(x2 << (3, 4), y << (5, 9)),
                    List(i << List(offset + 2), y << (6, 9)))))
    }

    @Test
    def testArrayAccess: Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("i = offset - 1", x1 << 4, x2 << 7, x3 << 2, i << offset - 1, y << 4),
                Initialize("i = offset",     i << offset,     y << 4),
                Initialize("i = offset + 1", i << offset + 1, y << 7),
                Initialize("i = offset + 3", i << offset + 2, y << 2),
                Initialize("i = offset + 3", i << offset + 3, y << 2)))
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
