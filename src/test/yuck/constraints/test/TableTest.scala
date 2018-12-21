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
final class TableTest extends UnitTest {

    @Test
    def testIntegerTable1 {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.variableIdFactory.nextId, "s", d)
        val t = new IntegerVariable(space.variableIdFactory.nextId, "t", d)
        val u = new IntegerVariable(space.variableIdFactory.nextId, "u", d)
        val costs = new BooleanVariable(space.variableIdFactory.nextId, "costs", CompleteBooleanDomain)
        val rows = immutable.IndexedSeq(immutable.IndexedSeq(0, 0, 0), immutable.IndexedSeq(1, 2, 3))
        val c = new IntegerTable(space.constraintIdFactory.nextId, null, immutable.IndexedSeq(s, t, u), rows, costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), False3)
        if (true) {
            // move away from the first row and approach the second row
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Two)
            val after = space.consult(move)
            assertEq(after.value(t), Two)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(t), Two)
            assertEq(now.value(costs), False2)
        }
        if (true) {
            // change two values at once
            val move =
                new ChangeValues(
                    space.moveIdFactory.nextId,
                    List(new ImmutableEffect(s, Zero), new ImmutableEffect(u, Three)))
            val after = space.consult(move)
            assertEq(after.value(s), Zero)
            assertEq(after.value(u), Three)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(u), Three)
            assertEq(now.value(costs), False)
        }
        space.initialize
        assertEq(now.value(costs), False)
    }

    // This test considers a situation that caused problems due to a bug in the
    // deletion of infeasible table rows.
    @Test
    def testIntegerTable2 {
        val space = new Space(logger)
        val s = new IntegerVariable(space.variableIdFactory.nextId, "s", new IntegerRange(Two, Five))
        val t = new IntegerVariable(space.variableIdFactory.nextId, "t", new IntegerRange(Two, Three))
        val costs = new BooleanVariable(space.variableIdFactory.nextId, "costs", TrueDomain)
        val rows =
            immutable.IndexedSeq(
                0, 0,
                1, 0, 1, 1, 1, 4,
                2, 0, 2, 2,
                3, 0, 3, 3,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
                .grouped(2).toIndexedSeq
        space.post(new IntegerTable(space.constraintIdFactory.nextId, null, immutable.IndexedSeq(s, t), rows, costs))
        if (true) {
            space.prune
            assertEq(s.domain, IntegerDomain.createDomain(List(Two, Three, Five)))
            assertEq(t.domain, IntegerDomain.createDomain(List(Two, Three)))
        }
        if (true) {
            t.pruneDomain(new IntegerRange(Two, Two))
            space.prune
            assertEq(s.domain, IntegerDomain.createDomain(List(Two, Five)))
        }
    }

}
