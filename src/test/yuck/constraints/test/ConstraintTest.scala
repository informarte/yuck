package yuck.constraints.test

import scala.collection._
import org.junit._
import yuck.annealing._
import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class ConstraintTest extends UnitTest {

    @Test
    def testAlldistinct {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Alldistinct(space.constraintIdFactory.nextId, null, List(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(costs), One)
        }
        space.initialize
        assertEq(now.value(costs), One)
    }

    @Test
    def testAlldistinctWithAVariableOccuringTwice {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Alldistinct(space.constraintIdFactory.nextId, null, List(s, t, t), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(costs), One)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Two)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(t), Two)
            assertEq(after.value(costs), Two)
            space.commit(move)
            assertEq(now.value(t), Two)
            assertEq(now.value(costs), Two)
        }
        space.initialize
        assertEq(now.value(costs), Two)
    }

    @Test
    def testAlldistinctWithImplicitSolving {
        val space = new Space
        val d = new IntegerDomain(Zero, Two)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Alldistinct(space.constraintIdFactory.nextId, null, List(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), Two)
        val maybeMoveGenerator =
            c.prepareForImplicitSolving(space, new JavaRandomGenerator, DEFAULT_MOVE_SIZE_DISTRIBUTION, _ => None, 0)
        assert(maybeMoveGenerator.isDefined)
        val moveGenerator = maybeMoveGenerator.get
        assertNe(now.value(s), now.value(t))
        assertNe(now.value(s), now.value(u))
        assertNe(now.value(t), now.value(u))
        assertEq(now.value(costs), Zero)
        space.initialize
        for (i <- 1 to 10000) {
            val move = moveGenerator.nextMove
            val after = space.consult(move, false)
            assert(List(s, t, u).exists(x => now.value(x) != after.value(x)))
            assertNe(after.value(s), after.value(t))
            assertNe(after.value(s), after.value(u))
            assertNe(after.value(t), after.value(u))
        }
    }

    @Test
    def testAlldistinctExceptZero {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new AlldistinctExceptZero(space.constraintIdFactory.nextId, null, List(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), One)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Zero)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(t), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(t), Zero)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, u, Zero)
            val after = space.consult(move)
            assertEq(now.value(u), One)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(u), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(u), Zero)
            assertEq(now.value(costs), Zero)
        }
        space.initialize
        assertEq(now.value(costs), Zero)
    }

    @Test
    def testAlldistinctExceptZeroWithAVariableOccuringTwice {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new AlldistinctExceptZero(space.constraintIdFactory.nextId, null, List(s, t, t), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), One)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Zero)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(t), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(t), Zero)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, One)
            val after = space.consult(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(s), One)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Two)
            val after = space.consult(move)
            assertEq(now.value(t), Zero)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(t), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(t), Two)
            assertEq(now.value(costs), One)
        }
        space.initialize
        assertEq(now.value(costs), One)
    }

    @Test
    def testElement {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val xs = IndexedSeq(s, t, u)
        val i = space.createVariable("i", d)
        val y = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Element(space.constraintIdFactory.nextId, null, xs, i, y, 0)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, Two)
            .setValue(u, Three)
            .setValue(i, Zero)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u, i))
        val now = space.searchState
        assertEq(now.value(y), One)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(y), One)
            assertEq(after.value(s), Zero)
            assertEq(after.value(y), Zero)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(y), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, i, One)
            val after = space.consult(move)
            assertEq(now.value(i), Zero)
            assertEq(now.value(y), Zero)
            assertEq(after.value(i), One)
            assertEq(after.value(y), Two)
            space.commit(move)
            assertEq(now.value(i), One)
            assertEq(now.value(y), Two)
        }
        space.initialize
        assertEq(now.value(y), Two)
    }

    @Test
    def testCountConst {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val n = space.createVariable("n", NonNegativeIntegerDomain)
        val c = new CountConst(space.constraintIdFactory.nextId, null, List(s, t, u), One, n)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(n), Three)
        val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
        val after = space.consult(move)
        assertEq(now.value(s), One)
        assertEq(now.value(n), Three)
        assertEq(after.value(s), Two)
        assertEq(after.value(n), Two)
        space.commit(move)
        assertEq(now.value(s), Two)
        assertEq(now.value(n), Two)
        space.initialize
        assertEq(now.value(n), Two)
    }

    @Test
    def testCountVar {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val what = space.createVariable("what", d)
        val n = space.createVariable("n", NonNegativeIntegerDomain)
        val c = new CountVar(space.constraintIdFactory.nextId, null, List(s, t, u), what, n)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .setValue(what, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u, what))
        val now = space.searchState
        assertEq(now.value(n), Three)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(n), Three)
            assertEq(after.value(s), Two)
            assertEq(after.value(n), Two)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(n), Two)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, what, Two)
            val after = space.consult(move)
            assertEq(now.value(what), One)
            assertEq(now.value(n), Two)
            assertEq(after.value(what), Two)
            assertEq(after.value(n), One)
            space.commit(move)
            assertEq(now.value(what), Two)
            assertEq(now.value(n), One)
        }
        space.initialize
        assertEq(now.value(n), One)
    }

    @Test
    def testMinimum {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val min = space.createVariable("costs", UnboundedIntegerDomain)
        val c = new Minimum(space.constraintIdFactory.nextId, null, List(s, t, u), min)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, Two)
            .setValue(u, Three)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(min), One)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(min), One)
            assertEq(after.value(s), Two)
            assertEq(after.value(min), Two)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(min), Two)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, u, One)
            val after = space.consult(move)
            assertEq(now.value(u), Three)
            assertEq(now.value(min), Two)
            assertEq(after.value(u), One)
            assertEq(after.value(min), One)
            space.commit(move)
            assertEq(now.value(u), One)
            assertEq(now.value(min), One)
        }
        space.initialize
        assertEq(now.value(min), One)
    }

    @Test
    def testMaximum {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val max = space.createVariable("costs", UnboundedIntegerDomain)
        val c = new Maximum(space.constraintIdFactory.nextId, null, List(s, t, u), max)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, Two)
            .setValue(u, Three)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(max), Three)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, u, Two)
            val after = space.consult(move)
            assertEq(now.value(u), Three)
            assertEq(now.value(max), Three)
            assertEq(after.value(u), Two)
            assertEq(after.value(max), Two)
            space.commit(move)
            assertEq(now.value(u), Two)
            assertEq(now.value(max), Two)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Three)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(max), Two)
            assertEq(after.value(s), Three)
            assertEq(after.value(max), Three)
            space.commit(move)
            assertEq(now.value(s), Three)
            assertEq(now.value(max), Three)
        }
        space.initialize
        assertEq(now.value(max), Three)
    }

    @Test
    def testReification {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val x = space.createVariable("x", new IntegerDomain(Zero, Nine))
        val b = space.createVariable("b", new BooleanDomain(true, true))
        val costs = space.createVariable("costs", UnboundedIntegerDomain)
        val c = new Reification(space.constraintIdFactory.nextId, null, x, b, costs)
        space.post(c)
        val now = space.searchState
        space.setValue(x, Zero).setValue(b, True).initialize
        assertEq(now.value(costs), Zero)
        space.setValue(x, Five).setValue(b, True).initialize
        assertEq(now.value(costs), Five)
        val move1 = new ChangeValue(space.moveIdFactory.nextId, x, Zero)
        val after1 = space.consult(move1)
        assertEq(now.value(costs), Five)
        assertEq(after1.value(costs), Zero)
        space.setValue(x, Zero).setValue(b, False).initialize
        assertEq(now.value(costs), One)
        space.setValue(x, Five).setValue(b, False).initialize
        assertEq(now.value(costs), Zero)
        val move2 = new ChangeValue(space.moveIdFactory.nextId, x, Zero)
        val after2 = space.consult(move2)
        assertEq(now.value(costs), Zero)
        assertEq(after2.value(costs), One)
    }

    @Test
    def testPlus {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val v = space.createVariable("v", d)
        val w = space.createVariable("w", d)
        // u = s + t
        space
            .post(new Plus(space.constraintIdFactory.nextId, null, s, t, u))
            .setValue(s, new IntegerValue(1))
            .setValue(t, new IntegerValue(2))
            .post(new Plus(space.constraintIdFactory.nextId, null, u, v, w))
            .setValue(v, new IntegerValue(1))
            .initialize
        assertEq(Set(s, t, v), space.searchVariables)
        val now = space.searchState
        assertEq(now.value(u), Three)
        assertEq(now.value(w), Four)
        val move = new ChangeValue(space.moveIdFactory.nextId, s, new IntegerValue(2))
        val after = space.consult(move)
        assertEq(now.value(u), Three)
        assertEq(now.value(w), Four)
        assertEq(after.value(u), Four)
        assertEq(after.value(w), Five)
        space.commit(move)
        assertEq(now.value(u), Four)
        assertEq(now.value(w), Five)
        space.initialize
        assertEq(now.value(w), Five)
    }

    @Test
    def testLinearCombination {
        val space = new Space
        val d = new IntegerDomain(Zero, new IntegerValue(100))
        val x1 = space.createVariable("x1", d)
        val x2 = space.createVariable("x2", d)
        val x3 = space.createVariable("x3", d)
        val y = space.createVariable("y", d)
        val c =
            new LinearCombination(
                space.constraintIdFactory.nextId, null,
                List(new AX(Zero, x1), new AX(One, x2), new AX(One, x3)), y)
        space
            .post(c)
            .setValue(x1, One)
            .setValue(x2, Two)
            .setValue(x3, Three)
            .initialize
        assertEq(space.searchVariables, Set(x1, x2, x3))
        val now = space.searchState
        assertEq(now.value(y), Five)
        val move = new ChangeValue(space.moveIdFactory.nextId, x2, Three)
        val after = space.consult(move)
        assertEq(now.value(y), Five)
        assertEq(after.value(y), Six)
        space.commit(move)
        assertEq(now.value(y), Six)
        space.initialize
        assertEq(now.value(y), Six)
    }

    @Test
    def testSum {
        val space = new Space
        val d = new IntegerDomain(Zero, new IntegerValue(100))
        val x1 = space.createVariable("x1", d)
        val x2 = space.createVariable("x2", d)
        val x3 = space.createVariable("x3", d)
        val y = space.createVariable("y", d)
        val c = new Sum(space.constraintIdFactory.nextId, null, List(x1, x2, x3), y)
        space
            .post(c)
            .setValue(x1, One)
            .setValue(x2, Two)
            .setValue(x3, Three)
            .initialize
        assertEq(space.searchVariables, Set(x1, x2, x3))
        val now = space.searchState
        assertEq(now.value(y), Six)
        val move = new ChangeValue(space.moveIdFactory.nextId, x2, Three)
        val after = space.consult(move)
        assertEq(now.value(y), Six)
        assertEq(after.value(y), Seven)
        space.commit(move)
        assertEq(now.value(y), Seven)
        space.initialize
        assertEq(now.value(y), Seven)
    }

    @Test
    def testCumulative {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s0 = space.createVariable("s0", d)
        val d0 = space.createVariable("d0", d)
        val c0 = space.createVariable("c0", d)
        val s1 = space.createVariable("s1", d)
        val d1 = space.createVariable("d1", d)
        val c1 = space.createVariable("c1", d)
        val t0 = new CumulativeTask(s0, d0, c0)
        val t1 = new CumulativeTask(s1, d1, c1)
        val ub = space.createVariable("ub", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Cumulative(space.constraintIdFactory.nextId, null, List(t0, t1), ub, costs)
        // initial conflict: two tasks with same start time and duration
        space
            .post(c)
            .setValue(s0, Zero)
            .setValue(d0, Three)
            .setValue(c0, Two)
            .setValue(s1, Zero)
            .setValue(d1, Three)
            .setValue(c1, Two)
            .setValue(ub, Three)
            .initialize
        assertEq(space.searchVariables, Set(s0, d0, c0, s1, d1, c1, ub))
        val now = space.searchState
        assertEq(now.value(costs), Three)
        if (true) {
            // move second task to resolve conflict
            val move = new ChangeValue(space.moveIdFactory.nextId, s1, Three)
            val after = space.consult(move)
            assertEq(now.value(s1), Zero)
            assertEq(now.value(costs), Three)
            assertEq(after.value(s1), Three)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(s1), Three)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            // make first task longer to create overlap
            val move = new ChangeValue(space.moveIdFactory.nextId, d0, Four)
            val after = space.consult(move)
            assertEq(now.value(d0), Three)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(d0), Four)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(d0), Four)
            assertEq(now.value(costs), One)
        }
        if (true) {
            // resolve conflict by increasing resource supply
            val move = new ChangeValue(space.moveIdFactory.nextId, ub, Four)
            val after = space.consult(move)
            assertEq(now.value(ub), Three)
            assertEq(now.value(costs), One)
            assertEq(after.value(ub), Four)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(ub), Four)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            // change duration and resource consumption in one move
            val move =
                new ChangeValues(
                    space.moveIdFactory.nextId,
                    List(new ImmutableEffect(d0, Three), new ImmutableEffect(c0, Five)))
            val after = space.consult(move)
            assertEq(now.value(d0), Four)
            assertEq(now.value(c0), Two)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(d0), Three)
            assertEq(after.value(c0), Five)
            assertEq(after.value(costs), Three)
            space.commit(move)
            assertEq(now.value(d0), Three)
            assertEq(now.value(c0), Five)
            assertEq(now.value(costs), Three)
        }
        space.initialize
        assertEq(now.value(costs), Three)
    }

    @Test
    def testIntegerTable1 {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
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
        assertEq(now.value(costs), Three)
        if (true) {
            // move away from the first row and approach the second row
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Two)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), Three)
            assertEq(after.value(t), Two)
            assertEq(after.value(costs), Two)
            space.commit(move)
            assertEq(now.value(t), Two)
            assertEq(now.value(costs), Two)
        }
        if (true) {
            // change two values at once
            val move =
                new ChangeValues(
                    space.moveIdFactory.nextId,
                    List(new ImmutableEffect(s, Zero), new ImmutableEffect(u, Three)))
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(u), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Zero)
            assertEq(after.value(u), Three)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(u), Three)
            assertEq(now.value(costs), One)
        }
        space.initialize
        assertEq(now.value(costs), One)
    }

    // This test considers a situation that caused problems due to a bug in the
    // deletion of infeasible table rows.
    @Test
    def testIntegerTable2 {
        val space = new Space
        val s = space.createVariable("s", new IntegerDomain(Two, Five))
        val t = space.createVariable("t", new IntegerDomain(Two))
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val rows =
            immutable.IndexedSeq(
                0, 0,
                1, 0, 1, 1, 1, 4,
                2, 0, 2, 2,
                3, 0, 3, 3,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
            .grouped(2).toIndexedSeq
        val c = new IntegerTable(space.constraintIdFactory.nextId, null, immutable.IndexedSeq(s, t), rows, costs)
        space.post(c).setValue(s, Two).setValue(t, Two).initialize
        assertEq(space.searchVariables, Set(s))
        val now = space.searchState
        assertEq(now.value(costs), Zero)
        space.setValue(s, Three).initialize
        assertEq(now.value(costs), One)
        space.setValue(s, Four).initialize
        assertEq(now.value(costs), One)
        space.setValue(s, Five).initialize
        assertEq(now.value(costs), Zero)
    }

    @Test
    def testLexLess {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val v = space.createVariable("v", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c =
            new LexLess(
                space.constraintIdFactory.nextId, null,
                immutable.IndexedSeq(s, t), immutable.IndexedSeq(u, v), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .setValue(v, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u, v))
        val now = space.searchState
        assertEq(now.value(costs), One)
        if (true) {
            // t = 5
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Five)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(t), Five)
            assertEq(after.value(costs), Five)
            space.commit(move)
            assertEq(now.value(t), Five)
            assertEq(now.value(costs), Five)
        }
        if (true) {
            // s = 0
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Five)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), Zero)
        }
        space.initialize
        assertEq(now.value(costs), Zero)
    }

    @Test
    def testLexLessEq {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val v = space.createVariable("v", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c =
            new LexLessEq(
                space.constraintIdFactory.nextId, null,
                immutable.IndexedSeq(s, t), immutable.IndexedSeq(u, v), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .setValue(v, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u, v))
        val now = space.searchState
        assertEq(now.value(costs), Zero)
        if (true) {
            // t = 5
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Five)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(t), Five)
            assertEq(after.value(costs), Four)
            space.commit(move)
            assertEq(now.value(t), Five)
            assertEq(now.value(costs), Four)
        }
        if (true) {
            // s = 0
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Four)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), Zero)
        }
        space.initialize
        assertEq(now.value(costs), Zero)
    }

    @Test
    def testInverseWithIdenticalOffsets {
        val space = new Space
        val d = new IntegerDomain(One, Three)
        val f1 = space.createVariable("f1", d)
        val f2 = space.createVariable("f2", d)
        val f3 = space.createVariable("f3", d)
        val g1 = space.createVariable("g1", d)
        val g2 = space.createVariable("g2", d)
        val g3 = space.createVariable("g3", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val f = immutable.IndexedSeq(f1, f2, f3)
        val g = immutable.IndexedSeq(g1, g2, g3)
        val c = new Inverse(space.constraintIdFactory.nextId, null, f, 1, g, 1, costs)
        // 3 1 2
        // 3 1 2
        space
            .post(c)
            .setValue(f1, Three).setValue(f2, One).setValue(f3, Two)
            .setValue(g1, Three).setValue(g2, One).setValue(g3, Two)
            .initialize
        assertEq(space.searchVariables, Set(f1, f2, f3, g1, g2, g3))
        val now = space.searchState
        assertEq(now.value(costs), Eight)
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
            assertEq(now.value(costs), Eight)
            assertEq(after.value(g1), Two)
            assertEq(after.value(g3), Three)
            assertEq(after.value(costs), Six)
            space.commit(move)
            assertEq(now.value(g1), Two)
            assertEq(now.value(g3), Three)
            assertEq(now.value(costs), Six)
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
            assertEq(now.value(costs), Six)
            assertEq(after.value(f1), Two)
            assertEq(after.value(f3), Three)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(f1), Two)
            assertEq(now.value(f3), Three)
            assertEq(now.value(costs), Zero)
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
            assertEq(now.value(costs), Zero)
            assertEq(after.value(f2), Two)
            assertEq(after.value(g1), One)
            assertEq(after.value(costs), Two)
            space.commit(move)
            assertEq(now.value(f2), Two)
            assertEq(now.value(g1), One)
            assertEq(now.value(costs), Two)
        }
        if (true) {
            // reverting previous move in two steps, this is step one:
            // 2 1 3
            // 1 1 3
            val move = new ChangeValue(space.moveIdFactory.nextId, f2, One)
            val after = space.consult(move)
            assertEq(now.value(f2), Two)
            assertEq(now.value(costs), Two)
            assertEq(after.value(f2), One)
            assertEq(after.value(costs), Two)
            space.commit(move)
            assertEq(now.value(f2), One)
            assertEq(now.value(costs), Two)
        }
        if (true) {
            // ... this is step two:
            // 2 1 3
            // 2 1 3
            val move = new ChangeValue(space.moveIdFactory.nextId, g1, Two)
            val after = space.consult(move)
            assertEq(now.value(g1), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(g1), Two)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(g1), Two)
            assertEq(now.value(costs), Zero)
        }
        space.initialize
        assertEq(now.value(costs), Zero)
    }

    @Test
    def testInverseWithDifferentOffsets {
        val space = new Space
        val fd = new IntegerDomain(Zero, Two)
        val gd = new IntegerDomain(One, Three)
        val f1 = space.createVariable("f1", fd)
        val f2 = space.createVariable("f2", fd)
        val f3 = space.createVariable("f3", fd)
        val g1 = space.createVariable("g1", gd)
        val g2 = space.createVariable("g2", gd)
        val g3 = space.createVariable("g3", gd)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val f = immutable.IndexedSeq(f1, f2, f3)
        val g = immutable.IndexedSeq(g1, g2, g3)
        val c = new Inverse(space.constraintIdFactory.nextId, null, f, 1, g, 0, costs)
        // 2 0 1
        // 3 1 2
        space
            .post(c)
            .setValue(f1, Two).setValue(f2, Zero).setValue(f3, One)
            .setValue(g1, Three).setValue(g2, One).setValue(g3, Two)
            .initialize
        assertEq(space.searchVariables, Set(f1, f2, f3, g1, g2, g3))
        val now = space.searchState
        assertEq(now.value(costs), Eight)
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
            assertEq(now.value(costs), Eight)
            assertEq(after.value(g1), Two)
            assertEq(after.value(g3), Three)
            assertEq(after.value(costs), Six)
            space.commit(move)
            assertEq(now.value(g1), Two)
            assertEq(now.value(g3), Three)
            assertEq(now.value(costs), Six)
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
            assertEq(now.value(costs), Six)
            assertEq(after.value(f1), One)
            assertEq(after.value(f3), Two)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(f1), One)
            assertEq(now.value(f3), Two)
            assertEq(now.value(costs), Zero)
        }
        space.initialize
        assertEq(now.value(costs), Zero)
    }

    @Test
    def testRegular {
        val space = new Space
        val d = new IntegerDomain(One, Two)
        val xs = for (i <- 1 to 10) yield space.createVariable("x[%d]".format(i), d)
        val Q = 6
        val S = 2
        val delta = immutable.IndexedSeq(1, 2, 3, 0, 3, 4, 0, 5, 0, 6, 6, 0).grouped(2).toIndexedSeq
        val q0 = 1
        val F = new IntegerDomain(Six, Six)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Regular(space.constraintIdFactory.nextId, null, xs, Q, S, delta, q0, F, costs)
        space
            .post(c)
            // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
            // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
            .setValue(xs(0), One).setValue(xs(1), One).setValue(xs(2), One).setValue(xs(3), Two)
            .setValue(xs(4), One).setValue(xs(5), Two).setValue(xs(6), Two).setValue(xs(7), Two)
            .setValue(xs(8), One).setValue(xs(9), One)
            .initialize
        assertEq(space.searchVariables, xs.toSet)
        val now = space.searchState
        assertEq(now.value(costs), Zero)
        if (true) {
            // input:  1, 1, 1, 2, 2, 2, 2, 2, 1, 1
            // states: 1, 1, 1, 2, 0, 0, 0, 0, 0, 0
            val move = new ChangeValue(space.moveIdFactory.nextId, xs(4), Two)
            val after = space.consult(move)
            assertEq(now.value(xs(4)), One)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(xs(4)), Two)
            assertEq(after.value(costs), Six)
            space.commit(move)
            assertEq(now.value(xs(4)), Two)
            assertEq(now.value(costs), Six)
        }
        if (true) {
            // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 2
            // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 0
            val move =
                new ChangeValues(
                    space.moveIdFactory.nextId,
                    List(new ImmutableEffect(xs(4), One), new ImmutableEffect(xs(9), Two)))
            val after = space.consult(move)
            assertEq(now.value(xs(4)), Two)
            assertEq(now.value(xs(9)), One)
            assertEq(now.value(costs), Six)
            assertEq(after.value(xs(4)), One)
            assertEq(after.value(xs(9)), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(xs(4)), One)
            assertEq(now.value(xs(9)), Two)
            assertEq(now.value(costs), One)
        }
        if (true) {
            // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
            // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
            val move = new ChangeValue(space.moveIdFactory.nextId, xs(9), One)
            val after = space.consult(move)
            assertEq(now.value(xs(9)), Two)
            assertEq(now.value(costs), One)
            assertEq(after.value(xs(9)), One)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(xs(9)), One)
            assertEq(now.value(costs), Zero)
        }
        space.initialize
        assertEq(now.value(costs), Zero)
    }

    @Test
    def testNumberOfDistinctValues {
        val space = new Space
        val d = new IntegerDomain(One, Two)
        val xs = for (i <- 1 to 3) yield space.createVariable("x[%d]".format(i), d)
        val m = space.createVariable("m", NonNegativeIntegerDomain)
        val c = new NumberOfDistinctValues(space.constraintIdFactory.nextId, null, xs, m)
        space
            .post(c)
            .setValue(xs(0), One).setValue(xs(1), One).setValue(xs(2), One)
            .initialize
        val now = space.searchState
        assertEq(now.value(m), One)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, xs(0), Two)
            val after = space.consult(move)
            assertEq(now.value(xs(0)), One)
            assertEq(now.value(m), One)
            assertEq(after.value(xs(0)), Two)
            assertEq(after.value(m), Two)
            space.commit(move)
            assertEq(now.value(xs(0)), Two)
            assertEq(now.value(m), Two)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, xs(1), Two)
            val after = space.consult(move)
            assertEq(now.value(xs(1)), One)
            assertEq(now.value(m), Two)
            assertEq(after.value(xs(1)), Two)
            assertEq(after.value(m), Two)
            space.commit(move)
            assertEq(now.value(xs(1)), Two)
            assertEq(now.value(m), Two)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, xs(2), Two)
            val after = space.consult(move)
            assertEq(now.value(xs(2)), One)
            assertEq(now.value(m), Two)
            assertEq(after.value(xs(2)), Two)
            assertEq(after.value(m), One)
            space.commit(move)
            assertEq(now.value(xs(2)), Two)
            assertEq(now.value(m), One)
        }
        space.initialize
        assertEq(now.value(m), One)
    }

    @Test
    def testCostsOr {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", UnboundedIntegerDomain)
        val c = new Disjunction(space.constraintIdFactory.nextId, null, List(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, Two)
            .setValue(u, Three)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, u, Two)
            val after = space.consult(move)
            assertEq(now.value(u), Three)
            assertEq(now.value(costs), Two)
            assertEq(after.value(u), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(u), Two)
            assertEq(now.value(costs), One)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Three)
            val after = space.consult(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(s), Three)
            assertEq(after.value(costs), Two)
            space.commit(move)
            assertEq(now.value(s), Three)
            assertEq(now.value(costs), Two)
        }
        space.initialize
        assertEq(now.value(costs), Two)
    }

    @Test
    def testBinPacking {
        val space = new Space
        // We start with 1 because we want to test the handling of a bin range that does not start at 0.
        val binDomain = new IntegerDomain(One, Three)
        val items =
            (for (i <- (1 to 5).toIterator) yield {
                val bin = space.createVariable("bin%d".format(i), binDomain)
                i -> new BinPackingItem(bin, IntegerValue.get(i))
            }).toMap
        val loads =
            (for (i <- binDomain.values) yield
                i.value -> space.createVariable("load%d".format(i.value), UnboundedIntegerDomain)).toMap
        space
            .post(new BinPacking(space.constraintIdFactory.nextId, null, items.valuesIterator.to, loads))
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
            val move = new ChangeValue(space.moveIdFactory.nextId, items(1).bin, Three)
            val after = space.consult(move)
            assertEq(now.value(items(1).bin), One)
            assertEq(now.value(loads(1)), Five)
            assertEq(now.value(loads(2)), Seven)
            assertEq(now.value(loads(3)), Three)
            assertEq(after.value(items(1).bin), Three)
            assertEq(after.value(loads(1)), Four)
            assertEq(after.value(loads(2)), Seven)
            assertEq(after.value(loads(3)), Four)
            space.commit(move)
            assertEq(now.value(items(1).bin), Three)
            assertEq(now.value(loads(1)), Four)
            assertEq(now.value(loads(2)), Seven)
            assertEq(now.value(loads(3)), Four)
        }
        if (true) {
            // move item 2 from bin 2 to 1
            // move item 5 from bin 2 to 3
            val move =
                new ChangeValues(
                    space.moveIdFactory.nextId,
                    List(new ImmutableEffect(items(2).bin, One), new ImmutableEffect(items(5).bin, Three)))
            val after = space.consult(move)
            assertEq(now.value(items(2).bin), Two)
            assertEq(now.value(items(5).bin), Two)
            assertEq(now.value(loads(1)), Four)
            assertEq(now.value(loads(2)), Seven)
            assertEq(now.value(loads(3)), Four)
            assertEq(after.value(items(2).bin), One)
            assertEq(after.value(items(5).bin), Three)
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
