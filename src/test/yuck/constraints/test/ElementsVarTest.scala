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
final class ElementsVarTest(offset: Int) extends UnitTest with ConstraintTestTooling {

    private val BaseDomain = IntegerRange(0, 9)

    private val space = new Space(logger, sigint)

    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), BaseDomain)
    private val Seq(x1, x2, x3) = xs
    private val indexRange = IntegerRange(offset, offset + 2)
    private val extendedIndexRange = IntegerRange(offset, offset + 3)
    private val is = for (i <- 1 to 2) yield new IntegerVariable(space.nextVariableId(), "i%d".format(i), extendedIndexRange)
    private val Seq(i1, i2) = is
    private val ys = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "y%d".format(i), BaseDomain)
    private val Seq(y1, y2, y3) = ys
    private val i = new IntegerVariable(space.nextVariableId(), "i", CompleteIntegerRange)
    private val y = new IntegerVariable(space.nextVariableId(), "y", CompleteIntegerRange)

    @Test
    def testBasics(): Unit = {
        val constraint = new ElementsVar(space.nextConstraintId(), null, xs, is, Vector(y1, y2), offset)
        assertEq(constraint.toString, "[y1, y2] = elements([x1, x2, x3], [i1, i2], %d)".format(offset))
        assertEq(constraint.inVariables.size, 5)
        assertEq(constraint.inVariables.toSet, Set(x1, x2, x3, i1, i2))
        assertEq(constraint.outVariables.size, 2)
        assertEq(constraint.outVariables.toSet, Set(y1, y2))
    }

    @Test
    def testPropagation(): Unit = {
        val constraint = new ElementsVar(space.nextConstraintId(), null, xs, is, Vector(y1, y2), offset)
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, List(i1 << indexRange, i2 << indexRange)),
                Propagate("reduce domains of x1, x2, and x3", List(x1 << (0, 3), x2 << (3, 6), x3 << (6, 9)), Nil),
                PropagateAndRollback("reduce domains of x1 and x3", List(x1 << (1, 2), x3 << (7, 8)), List(y1 << (1, 8), y2 << (1, 8))),
                PropagateAndRollback(
                    "reduce domain of i1",
                    List(i1 << List(offset, offset + 2)),
                    List(y1 << IntegerRange(0, 3).union(IntegerRange(6, 9)))),
                PropagateAndRollback(
                    "reduce domain of y2",
                    List(y2 << List(1, 8)),
                    List(i2 << List(offset, offset + 2))),
                PropagateAndRollback(
                    "reduce domains of i1 and y1",
                    List(i1 << (offset, offset + 1), y1 << (5, 9)),
                    List(i1 << List(offset + 1), y1 << (5, 6))),
                PropagateAndRollback(
                    "reduce domains of x1 and i2",
                    List(x1 << (1, 3), x2 << (3, 5), i2 << (offset, offset + 1)),
                    List(y1 << (1, 9), y2 << (1, 5))),
                Propagate("reduce domains of x2 and y1",
                    List(x2 << (3, 4), y1 << (5, 9)),
                    List(i1 << List(offset + 2), y1 << (6, 9), y2 << IntegerRange(0, 4).union(IntegerRange(6, 9))))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInPropagation(): Unit = {
        val constraint = new ElementsVar(space.nextConstraintId(), null, Vector(x1, x2, x1, x3), Vector(i1, i2, i1), Vector(y1, y2, y3), offset)
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, List(i1 << extendedIndexRange, i2 << extendedIndexRange)),
                Propagate("reduce domains of x1, x2, and x3", List(x1 << (0, 3), x2 << (3, 6), x3 << (6, 9)), Nil),
                PropagateAndRollback("reduce domains of x1 and x3", List(x1 << (1, 2), x3 << (7, 8)), List(y1 << (1, 8), y2 << (1, 8), y3 << (1, 8))),
                PropagateAndRollback(
                    "reduce domain of i1",
                    List(i1 << List(offset, offset + 2)),
                    List(y1 << IntegerRange(0, 3), y3 << IntegerRange(0, 3))),
                PropagateAndRollback(
                    "reduce domain of y2",
                    List(y2 << List(1, 8)),
                    List(i2 << List(offset, offset + 2, offset + 3))),
                PropagateAndRollback(
                    "reduce domains of i1 and y1",
                    List(i1 << (offset, offset + 1), y1 << (5, 9)),
                    List(i1 << List(offset + 1), y1 << (5, 6), y3 << (3, 6))),
                PropagateAndRollback(
                    "reduce domains of x1 and i2",
                    List(x1 << (1, 3), x2 << (3, 5), i2 << (offset, offset + 1)),
                    List(y1 << (1, 9), y2 << (1, 5), y3 << (1, 9))),
                Propagate("reduce domains of x2 and y1",
                    List(x2 << (3, 4), y1 << (5, 9)),
                    List(i1 << List(offset + 3), y1 << (6, 9), y2 << IntegerRange(0, 4).union(IntegerRange(6, 9)), y3 << (6, 9)))))
    }

    @Test
    def testCostComputation(): Unit = {
        val constraint = new ElementsVar(space.nextConstraintId(), null, xs, is, Vector(y1, y2), offset)
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("i1 = offset - 1", List(x1 << 4, x2 << 7, x3 << 2, i1 << offset - 1, i2 << offset + 2), List(y1 << 4, y2 << 2)),
                Initialize("i1 = offset", i1 << offset, y1 << 4),
                Initialize("i1 = offset + 1", i1 << offset + 1, y1 << 7),
                Initialize("i1 = offset + 2", i1 << offset + 2, y1 << 2),
                Initialize("i1 = offset + 3", i1 << offset + 3, y1 << 2),
                Consult("x3 = 8", List(x3 << 8), List(y1 << 8, y2 << 8)),
                Consult("x1 = 8, x3 = 3", List(x1 << 8, x3 << 3), List(y1 << 3, y2 << 3)),
                Consult("i2 = offset", List(i2 << offset), List(y2 << 4)),
                Consult("i1 = offset + 1, i2 = offset", List(i1 << offset + 1, i2 << offset), List(y1 << 7, y2 << 4)),
                Consult("i2 = offset + 1, x2 = 9", List(i2 << offset + 1, x2 << 9), List(y2 << 9)),
                ConsultAndCommit("x3 = 8", List(x3 << 8), List(y1 << 8, y2 << 8)),
                ConsultAndCommit("x1 = 8, x3 = 3", List(x1 << 8, x3 << 3), List(y1 << 3, y2 << 3)),
                ConsultAndCommit("i1 = offset", List(i1 << offset), List(y1 << 8)),
                ConsultAndCommit("i1 = offset + 1, i2 = offset", List(i1 << offset + 1, i2 << offset), List(y1 << 7, y2 << 8)),
                ConsultAndCommit("i1 = offset + 2, x3 = 1", List(i1 << offset + 2, x3 << 1), List(y1 << 1))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation(): Unit = {
        val constraint = new ElementsVar(space.nextConstraintId(), null, Vector(x1, x2, x1, x3), Vector(i1, i2, i1), Vector(y1, y2, y3), offset)
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("i1 = offset - 1", List(x1 << 4, x2 << 7, x3 << 2, i1 << offset - 1, i2 << offset + 2), List(y1 << 4, y2 << 4, y3 << 4)),
                Initialize("i1 = offset", List(i1 << offset), Nil),
                Initialize("i1 = offset + 1", List(i1 << offset + 1), List(y1 << 7, y3 << 7)),
                Initialize("i1 = offset + 2", List(i1 << offset + 2), List(y1 << 4, y3 << 4)),
                Initialize("i1 = offset + 3", List(i1 << offset + 3), List(y1 << 2, y3 << 2)),
                Initialize("i1 = offset + 4", List(i1 << offset + 4), Nil),
                Consult("x1 = 8", List(x1 << 8), List(y2 << 8)),
                Consult("x1 = 8, x3 = 5", List(x1 << 8, x3 << 5), List(y1 << 5, y2 << 8, y3 << 5)),
                Consult("i2 = offset", List(i2 << offset), List(y2 << 4)),
                Consult("i1 = offset + 1, i2 = offset ", List(i1 << offset + 1, i2 << offset), List(y1 << 7, y2 << 4, y3 << 7)),
                Consult("i1 = offset + 3, x3 = 5", List(i1 << offset + 3, x3 << 5), List(y1 << 5, y3 << 5)),
                ConsultAndCommit("x1 = 8", List(x1 << 8), List(y2 << 8)),
                ConsultAndCommit("x1 = 3, x3 = 5", List(x1 << 3, x3 << 5), List(y1 << 5, y2 << 3, y3 << 5)),
                ConsultAndCommit("i2 = offset + 1", List(i2 << offset + 1), List(y2 << 7)),
                ConsultAndCommit("i1 = offset + 1, i2 = offset", List(i1 << offset + 1, i2 << offset), List(y1 << 7, y2 << 3, y3 << 7)),
                ConsultAndCommit("i1 = offset + 3, x3 = 6", List(i1 << offset + 3, x3 << 6), List(y1 << 6, y3 << 6))))
    }

}

/**
 * @author Michael Marte
 *
 */
object ElementsVarTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(-1, 0, 1).asJava

}
