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
final class InverseTest extends UnitTest {

    @Test
    def testInverseWithIdenticalOffsets {
        val space = new Space(logger)
        val d = new IntegerRange(One, Three)
        val f1 = space.createVariable("f1", d)
        val f2 = space.createVariable("f2", d)
        val f3 = space.createVariable("f3", d)
        val g1 = space.createVariable("g1", d)
        val g2 = space.createVariable("g2", d)
        val g3 = space.createVariable("g3", d)
        val costs = space.createVariable("costs", CompleteBooleanDomain)
        val f = immutable.IndexedSeq(f1, f2, f3)
        val g = immutable.IndexedSeq(g1, g2, g3)
        val c = new Inverse(space.constraintIdFactory.nextId, null, new InverseFunction(f, 1), new InverseFunction(g, 1), costs)
        // 3 1 2
        // 3 1 2
        space
            .post(c)
            .setValue(f1, Three).setValue(f2, One).setValue(f3, Two)
            .setValue(g1, Three).setValue(g2, One).setValue(g3, Two)
            .initialize
        assertEq(space.searchVariables, Set(f1, f2, f3, g1, g2, g3))
        val now = space.searchState
        assertEq(now.value(costs), False8)
        if (true) {
            // swap values of g1 and g3:
            // 3 1 2
            // 2 1 3
            // This move checks the functioning of the logic that avoids revisiting the same node twice
            // when computing the cost delta.
            // Here f2 is the node in question because it is referenced by before(g3) and after(g1).
            val move =
            new ChangeValues(
                space.moveIdFactory.nextId,
                List(new ImmutableEffect(g1, Two), new ImmutableEffect(g3, Three)))
            val after = space.consult(move)
            assertEq(now.value(g1), Three)
            assertEq(now.value(g3), Two)
            assertEq(now.value(costs), False8)
            assertEq(after.value(g1), Two)
            assertEq(after.value(g3), Three)
            assertEq(after.value(costs), False6)
            space.commit(move)
            assertEq(now.value(g1), Two)
            assertEq(now.value(g3), Three)
            assertEq(now.value(costs), False6)
        }
        if (true) {
            // swap values of f1 and f3:
            // 2 1 3
            // 2 1 3
            val move =
            new ChangeValues(
                space.moveIdFactory.nextId,
                List(new ImmutableEffect(f1, Two), new ImmutableEffect(f3, Three)))
            val after = space.consult(move)
            assertEq(now.value(f1), Three)
            assertEq(now.value(f3), Two)
            assertEq(now.value(costs), False6)
            assertEq(after.value(f1), Two)
            assertEq(after.value(f3), Three)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(f1), Two)
            assertEq(now.value(f3), Three)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // swap values of f2 and g1:
            // 2 2 3
            // 1 1 3
            val move =
            new ChangeValues(
                space.moveIdFactory.nextId,
                List(new ImmutableEffect(f2, Two), new ImmutableEffect(g1, One)))
            val after = space.consult(move)
            assertEq(now.value(f2), One)
            assertEq(now.value(g1), Two)
            assertEq(now.value(costs), True)
            assertEq(after.value(f2), Two)
            assertEq(after.value(g1), One)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(f2), Two)
            assertEq(now.value(g1), One)
            assertEq(now.value(costs), False2)
        }
        if (true) {
            // reverting previous move in two steps, this is step one:
            // 2 1 3
            // 1 1 3
            val move = new ChangeValue(space.moveIdFactory.nextId, f2, One)
            val after = space.consult(move)
            assertEq(now.value(f2), Two)
            assertEq(now.value(costs), False2)
            assertEq(after.value(f2), One)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(f2), One)
            assertEq(now.value(costs), False2)
        }
        if (true) {
            // ... this is step two:
            // 2 1 3
            // 2 1 3
            val move = new ChangeValue(space.moveIdFactory.nextId, g1, Two)
            val after = space.consult(move)
            assertEq(now.value(g1), One)
            assertEq(now.value(costs), False2)
            assertEq(after.value(g1), Two)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(g1), Two)
            assertEq(now.value(costs), True)
        }
        space.initialize
        assertEq(now.value(costs), True)
    }

    @Test
    def testInverseWithDifferentOffsets {
        val space = new Space(logger)
        val fd = new IntegerRange(Zero, Two)
        val gd = new IntegerRange(One, Three)
        val f1 = space.createVariable("f1", fd)
        val f2 = space.createVariable("f2", fd)
        val f3 = space.createVariable("f3", fd)
        val g1 = space.createVariable("g1", gd)
        val g2 = space.createVariable("g2", gd)
        val g3 = space.createVariable("g3", gd)
        val costs = space.createVariable("costs", CompleteBooleanDomain)
        val f = immutable.IndexedSeq(f1, f2, f3)
        val g = immutable.IndexedSeq(g1, g2, g3)
        val c = new Inverse(space.constraintIdFactory.nextId, null, new InverseFunction(f, 1), new InverseFunction(g, 0), costs)
        // 2 0 1
        // 3 1 2
        space
            .post(c)
            .setValue(f1, Two).setValue(f2, Zero).setValue(f3, One)
            .setValue(g1, Three).setValue(g2, One).setValue(g3, Two)
            .initialize
        assertEq(space.searchVariables, Set(f1, f2, f3, g1, g2, g3))
        val now = space.searchState
        assertEq(now.value(costs), False8)
        if (true) {
            // swap values of g1 and g3:
            // 2 0 1
            // 2 1 3
            // This move checks the functioning of the logic that avoids revisiting the same node twice
            // when computing the cost delta.
            // Here f2 is the node in question because it is referenced by before(g3) and after(g1).
            val move =
            new ChangeValues(
                space.moveIdFactory.nextId,
                List(new ImmutableEffect(g1, Two), new ImmutableEffect(g3, Three)))
            val after = space.consult(move)
            assertEq(now.value(g1), Three)
            assertEq(now.value(g3), Two)
            assertEq(now.value(costs), False8)
            assertEq(after.value(g1), Two)
            assertEq(after.value(g3), Three)
            assertEq(after.value(costs), False6)
            space.commit(move)
            assertEq(now.value(g1), Two)
            assertEq(now.value(g3), Three)
            assertEq(now.value(costs), False6)
        }
        if (true) {
            // swap values of f1 and f3:
            // 1 0 2
            // 2 1 3
            val move =
            new ChangeValues(
                space.moveIdFactory.nextId,
                List(new ImmutableEffect(f1, One), new ImmutableEffect(f3, Two)))
            val after = space.consult(move)
            assertEq(now.value(f1), Two)
            assertEq(now.value(f3), One)
            assertEq(now.value(costs), False6)
            assertEq(after.value(f1), One)
            assertEq(after.value(f3), Two)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(f1), One)
            assertEq(now.value(f3), Two)
            assertEq(now.value(costs), True)
        }
        space.initialize
        assertEq(now.value(costs), True)
    }

}
