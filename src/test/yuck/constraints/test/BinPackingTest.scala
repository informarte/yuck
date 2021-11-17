package yuck.constraints.test

import org.junit._

import scala.collection.immutable.TreeMap

import yuck.constraints._
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BinPackingTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    // We start with 1 because we want to test the handling of a bin range that does not start at 0.
    private val binDomain = IntegerRange(1, 3)
    private val items =
        (for (i <- 1 to 5) yield {
            val bin = new IntegerVariable(space.nextVariableId, "bin%d".format(i), binDomain)
            i -> new BinPackingItem(bin, IntegerValue(i))
        })
        .to(TreeMap)
    private val loads =
        (for (i <- binDomain.values) yield
            i.value -> new IntegerVariable(space.nextVariableId, "load%d".format(i.value), CompleteIntegerRange))
        .to(TreeMap)

    @Test
    def testBasics: Unit = {
        val constraint = new BinPacking(space.nextConstraintId, null, items.values.toVector, loads)
        assertEq(
            constraint.toString,
            "bin_packing([(bin1, 1), (bin2, 2), (bin3, 3), (bin4, 4), (bin5, 5)], [(1, load1), (2, load2), (3, load3)])")
        assertEq(constraint.inVariables.size, items.size)
        assertEq(constraint.inVariables.toSet, items.values.map(_.bin).toSet)
        assertEq(constraint.outVariables.size, loads.size)
        assertEq(constraint.outVariables.toSet, loads.values.toSet)
    }

    @Test
    def testCostComputation: Unit = {
        space.post(new BinPacking(space.nextConstraintId, null, items.values.toVector, loads))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "some items assigned to non-existing bins",
                    List(items(1).bin << 1, items(2).bin << 0, items(3).bin << 3, items(4).bin << 4, items(5).bin << 2),
                    List(loads(1) << 1, loads(2) << 5, loads(3) << 3)),
                Initialize(
                    "all items assigned to existing bins",
                    List(items(1).bin << 1, items(2).bin << 2, items(3).bin << 3, items(4).bin << 1, items(5).bin << 2),
                    List(loads(1) << 5, loads(2) << 7, loads(3) << 3)),
                Consult(
                    "move item 5 from bin 2 to non-existing bin 0",
                    List(items(5).bin << 0),
                    List(loads(2) << 2)),
                Consult(
                    "exchange items 2 and 4",
                    List(items(2).bin << 1, items(4).bin << 2),
                    List(loads(1) << 3, loads(2) << 9)),
                ConsultAndCommit(
                    "move item 1 from bin 1 to 3",
                    List(items(1).bin << 3),
                    List(loads(1) << 4, loads(3) << 4)),
                ConsultAndCommit(
                    "move item 2 from bin 2 to 1, move item 5 from bin 2 to 3",
                    List(items(2).bin << 1, items(5).bin << 3),
                    List(loads(1) << 6, loads(2) << 0, loads(3) << 9))))
    }

}
