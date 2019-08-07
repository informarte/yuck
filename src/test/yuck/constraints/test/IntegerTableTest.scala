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
final class IntegerTableTest extends UnitTest {

    @Test
    def testConsultAndCommit: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val rows = immutable.IndexedSeq(immutable.IndexedSeq(0, 0, 0), immutable.IndexedSeq(1, 2, 3))
        space
            .post(new IntegerTable(space.nextConstraintId, null, immutable.IndexedSeq(s, t, u), rows, costs))
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), False3)
        if (true) {
            // move away from the first row and approach the second row
            val move = new ChangeValue(space.nextMoveId, t, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(costs), False2)
        }
        if (true) {
            // change two values at once
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableMoveEffect(s, Zero), new ImmutableMoveEffect(u, Three)))
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        space.initialize
        assertEq(now.value(costs), False)
    }

    @Test
    def testConsultAndCommitWithDuplicateVariables: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val rows = immutable.IndexedSeq(immutable.IndexedSeq(0, 0, 0), immutable.IndexedSeq(1, 2, 3), immutable.IndexedSeq(2, 2, 3))
        space
            .post(new IntegerTable(space.nextConstraintId, null, immutable.IndexedSeq(s, s, u), rows, costs))
            .setValue(s, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, u))
        val now = space.searchState
        assertEq(now.value(costs), False3)
        if (true) {
            // fix first two columns
            val move = new ChangeValue(space.nextMoveId, s, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(costs), False2)
        }
        if (true) {
            // fix last column
            val move = new ChangeValue(space.nextMoveId, u, Three)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // change two values at once
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableMoveEffect(s, Zero), new ImmutableMoveEffect(u, Two)))
            val after = space.consult(move)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(costs), False2)
        }
        space.initialize
        assertEq(now.value(costs), False2)
    }

    @Test
    def testPropagation: Unit = {
        val space = new Space(logger, sigint)
        val s = new IntegerVariable(space.nextVariableId, "s", new IntegerRange(Two, Five))
        val t = new IntegerVariable(space.nextVariableId, "t", new IntegerRange(Two, Three))
        val costs = new BooleanVariable(space.nextVariableId, "costs", TrueDomain)
        val rows =
            immutable.IndexedSeq(
                0, 0,
                1, 0, 1, 1, 1, 4,
                2, 0, 2, 2,
                3, 0, 3, 3,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
                .grouped(2).toIndexedSeq
        space.post(new IntegerTable(space.nextConstraintId, null, immutable.IndexedSeq(s, t), rows, costs))
        if (true) {
            space.propagate
            assertEq(s.domain, IntegerDomain.createDomain(List(Two, Three, Five)))
            assertEq(t.domain, IntegerDomain.createDomain(List(Two, Three)))
        }
        if (true) {
            t.pruneDomain(new IntegerRange(Two, Two))
            space.propagate
            assertEq(s.domain, IntegerDomain.createDomain(List(Two, Five)))
        }
    }

    @Test
    def testPropagationWithDuplicateVariables: Unit = {
        val space = new Space(logger, sigint)
        val s = new IntegerVariable(space.nextVariableId, "s", new IntegerRange(Two, Five))
        val costs = new BooleanVariable(space.nextVariableId, "costs", TrueDomain)
        val rows =
            immutable.IndexedSeq(
                0, 0,
                1, 0, 1, 4,
                2, 0, 2, 2,
                3, 0,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
                .grouped(2).toIndexedSeq
        space.post(new IntegerTable(space.nextConstraintId, null, immutable.IndexedSeq(s, s), rows, costs))
        if (true) {
            space.propagate
            assertEq(s.domain, IntegerDomain.createDomain(List(Two, Four)))
        }
    }

}
