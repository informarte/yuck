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
final class BinPackingTest extends UnitTest {

    @Test
    def testBinPacking: Unit = {
        val space = new Space(logger, sigint)
        // We start with 1 because we want to test the handling of a bin range that does not start at 0.
        val binDomain = new IntegerRange(One, Three)
        val items =
            (for (i <- 1 to 5) yield {
                val bin = new IntegerVariable(space.nextVariableId, "bin%d".format(i), binDomain)
                i -> new BinPackingItem(bin, IntegerValue.get(i))
            }).toMap
        val loads =
            (for (i <- binDomain.values) yield
                i.value -> space.createVariable("load%d".format(i.value), CompleteIntegerRange)).toMap
        space
            .post(new BinPacking(space.nextConstraintId, null, items.valuesIterator.toIndexedSeq, loads))
            .setValue(items(1).bin, One)
            .setValue(items(2).bin, Two)
            .setValue(items(3).bin, Three)
            .setValue(items(4).bin, One)
            .setValue(items(5).bin, Two)
            .initialize
        assertEq(space.searchVariables, items.valuesIterator.map(_.bin).toSet)
        val now = space.searchState
        assertEq(now.value(loads(1)), Five)
        assertEq(now.value(loads(2)), Seven)
        assertEq(now.value(loads(3)), Three)
        if (true) {
            // move item 1 from bin 1 to 3
            val move = new ChangeValue(space.nextMoveId, items(1).bin, Three)
            val after = space.consult(move)
            assertEq(after.value(loads(1)), Four)
            assertEq(after.value(loads(2)), Seven)
            assertEq(after.value(loads(3)), Four)
            space.commit(move)
            assertEq(now.value(loads(1)), Four)
            assertEq(now.value(loads(2)), Seven)
            assertEq(now.value(loads(3)), Four)
        }
        if (true) {
            // move item 2 from bin 2 to 1
            // move item 5 from bin 2 to 3
            val move =
            new ChangeValues(
                space.nextMoveId,
                List(new ImmutableMoveEffect(items(2).bin, One), new ImmutableMoveEffect(items(5).bin, Three)))
            val after = space.consult(move)
            assertEq(after.value(loads(1)), Six)
            assertEq(after.value(loads(2)), Zero)
            assertEq(after.value(loads(3)), Nine)
            space.commit(move)
            assertEq(now.value(loads(1)), Six)
            assertEq(now.value(loads(2)), Zero)
            assertEq(now.value(loads(3)), Nine)
        }
        space.initialize
        assertEq(now.value(loads(1)), Six)
        assertEq(now.value(loads(2)), Zero)
        assertEq(now.value(loads(3)), Nine)
    }

}
