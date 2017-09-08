package yuck.core.test

import scala.collection._

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerDomainTest extends UnitTest {

    private val helper = new DomainTestHelper[IntegerValue]

    private def testSetSize(d: IntegerDomain) {
        assertEq(d.size, d.values.size)
    }

    private def testSubsetRelation(d: IntegerDomain, e: IntegerDomain) {
        assertEq(d.isSubsetOf(e), d.values.toSet.subsetOf(e.values.toSet))
    }

    private def testSetIntersection(d: IntegerDomain, e: IntegerDomain) {
        val i = new IntegerDomain(d.values.toSet.intersect(e.values.toSet))
        assertEq(d.intersect(e), i)
        assertEq(d.maybeIntersectionSize(e), Some(i.size))
        assertEq(d.intersects(e), ! i.isEmpty)
    }

    private def testSetSubtraction(d: IntegerDomain, e: IntegerDomain) {
        val r = new IntegerDomain(d.values.toSet.diff(e.values.toSet))
        val f = d.subtract(e)
        assertEq(f, r)
        assertEq(d.maybeResidueSize(e), Some(f.size))
    }

    private def testSetContainment(d: IntegerDomain, a: IntegerValue) {
        assertEq(d.contains(a), d.values.contains(a))
    }

    private def testDistanceToSet(d: IntegerDomain, a: IntegerValue) {
        assertEq(d.distance(a), if (d.isEmpty) 1 else (d.values.map(b => (a - b).abs).min).value)
    }

    private def testEq(d: IntegerDomain, e: IntegerDomain) {
        assertEq(IntegerDomainPruner.eq(d, e), d.intersect(e))
    }

    private def testLe(d: IntegerDomain, e: IntegerDomain) {
        val (f, g) = IntegerDomainPruner.le(d, e)
        assert(f.isSubsetOf(d))
        assert(g.isSubsetOf(e))
        d.values.foreach(a => assertEq(e.values.exists(b => a <= b), f.contains(a)))
        e.values.foreach(b => assertEq(d.values.exists(a => a <= b), g.contains(b)))
    }

    private def testLt(d: IntegerDomain, e: IntegerDomain) {
        val (f, g) = IntegerDomainPruner.lt(d, e)
        assert(f.isSubsetOf(d))
        assert(g.isSubsetOf(e))
        d.values.foreach(a => assertEq(e.values.exists(b => a < b), f.contains(a)))
        e.values.foreach(b => assertEq(d.values.exists(a => a < b), g.contains(b)))
    }

    private def testPruning(d: IntegerDomain, e: IntegerDomain) {
        testEq(d, e)
        testLe(d, e)
        testLt(d, e)
    }

    private def testEq(d: IntegerDomain, a: IntegerValue) {
        assertEq(IntegerDomainPruner.eq(d, a), d.intersect(new IntegerDomain(a)))
    }

    private def testNe(d: IntegerDomain, a: IntegerValue) {
        assertEq(IntegerDomainPruner.ne(d, a), d.subtract(new IntegerDomain(a)))
    }

    private def testLe(d: IntegerDomain, a: IntegerValue) {
        assertEq(IntegerDomainPruner.le(d, a), d.intersect(new IntegerDomain(null, a)))
    }

    private def testLt(d: IntegerDomain, a: IntegerValue) {
        assertEq(IntegerDomainPruner.lt(d, a), d.intersect(new IntegerDomain(null, a - One)))
    }

    private def testPruning(d: IntegerDomain, a: IntegerValue) {
        testEq(d, a)
        testNe(d, a)
        testLe(d, a)
        testLt(d, a)
    }

    private def testBounding(d: IntegerDomain, a: IntegerValue) {
        val d1 = d.boundFromBelow(a)
        val d2 = d.boundFromAbove(a)
        if (! d1.isEmpty) {
            assertGe(d1.lb, a)
        }
        if (! d2.isEmpty) {
            assertLe(d2.ub, a)
        }
        if (d.contains(a)) {
            assertEq(d1.unite(new IntegerDomain(a)).unite(d2), d)
        } else if (d.maybeLb.isDefined && a < d.lb) {
            assertEq(d1, d)
            assert(d2.isEmpty)
        } else if (d.maybeUb.isDefined && a > d.ub) {
            assert(d1.isEmpty)
            assertEq(d2, d)
        }
    }

    private def testBisecting(d: IntegerDomain) {
        if (d.isEmpty || d.isInfinite) {
            assertEx(d.bisect)
        } else {
            val (d1, d2) = d.bisect
            assert(d1.isSubsetOf(d))
            assert(d2.isSubsetOf(d))
            assert(! d1.intersects(d2))
            assertEq(d1.unite(d2), d)
            if (d.size > 1) {
                assert(! d1.isEmpty)
                assert(! d2.isEmpty)
                assertLt(d1.ub, d2.lb)
            }
            if ((d.ub - d.lb + One).value == d.size) {
                assertLe(scala.math.abs(d1.size - d2.size), 1)
            }
        }
    }

    private def testOperationsOnFiniteIntegerDomains(rg: RandomGenerator, dl: Seq[IntegerDomain], vl: Seq[IntegerValue]) {
        for (d <- dl) {
            require(d.isFinite)
            testSetSize(d)
            for (e <- dl) {
                require(e.isFinite)
                testSubsetRelation(d, e)
                testSetIntersection(d, e)
                testSetSubtraction(d, e)
                testPruning(d, e)
            }
            for (a <- vl) {
                testSetContainment(d, a)
                testDistanceToSet(d, a)
                testPruning(d, a)
            }
            if (d.isEmpty) {
                assert(d.boundFromBelow(Zero).isEmpty)
                assert(d.boundFromAbove(Zero).isEmpty)
            } else {
                testBounding(d, d.randomValue(rg))
                testBounding(d, d.lb)
                testBounding(d, d.ub)
                testBounding(d, d.lb - One)
                testBounding(d, d.ub + One)
                if (! d.isSingleton) {
                    helper.testUniformityOfDistribution(rg, d)
                }
            }
            testBisecting(d)
        }
    }

    private def createSubdomains(rg: RandomGenerator, d: IntegerDomain, n: Int): Seq[IntegerDomain] =
        for (i <- 0 until n) yield {
            val choice = new mutable.HashSet[IntegerValue]
            for (a <- d.values) {
                if (rg.nextDecision) {
                    choice += a
                }
            }
            new IntegerDomain(choice)
        }

    @Test
    def testRepresentationOfFiniteIntegerDomains {
        val rg = new JavaRandomGenerator

        // {}
        val a = new IntegerDomain
        assertNe(a, "")
        assertEq(a, a)
        assertEq(a, new IntegerDomain(One, Zero))
        assertEq(a, EmptyIntegerDomain)
        assertEq(a.toString, "{}")
        assert(a.isEmpty)
        assertEq(a.size, 0)
        assert(a.isFinite)
        assert(! a.isInfinite)
        assert(! a.isSingleton)
        assert(! a.isSparse)
        assert(a.isDense)
        assert(! a.contains(Zero))
        assertEx(a.singleValue)
        assertEx(a.randomValue(rg))
        assertEx(a.nextRandomValue(rg, Zero))
        assertEq(a.values.toList, Nil)
        assert(a.isBounded)
        assert(! a.isUnbounded)
        assert(a.maybeLb.isDefined)
        assert(a.maybeUb.isDefined)
        assertEq(a.maybeLb.get, a.lb)
        assertEq(a.maybeUb.get, a.ub)
        assertLt(a.ub, a.lb)
        assert(a.hull.isEmpty)

        // {0}
        val b = new IntegerDomain(Zero)
        assertNe(b, a)
        assertEq(b, b)
        assertEq(b, new IntegerDomain(Zero, Zero))
        assertEq(b.toString, "{0}")
        assert(! b.isEmpty)
        assertEq(b.size, 1)
        assert(b.isFinite)
        assert(! b.isInfinite)
        assert(b.isSingleton)
        assert(b.isSparse)
        assert(! b.isDense)
        assert(b.contains(Zero))
        assert(! b.contains(One))
        assertEq(b.singleValue, Zero)
        assertEq(b.randomValue(rg), Zero)
        assertEq(b.nextRandomValue(rg, Zero), Zero)
        assertEq(b.values.toList, List(Zero))
        assert(b.isBounded)
        assert(! b.isUnbounded)
        assertEq(b.maybeLb.get, Zero)
        assertEq(b.maybeUb.get, Zero)
        assertEq(b.lb, Zero)
        assertEq(b.ub, Zero)
        assertEq(b.hull.lb, Zero)
        assertEq(b.hull.ub, Zero)
        assert(b.hull.isSingleton)

        // [0, 9]
        val c = new IntegerDomain(Zero, Nine)
        assertNe(c, a)
        assertNe(c, b)
        assertEq(c, c)
        assertEq(c, new IntegerDomain(Zero, Nine))
        assertEq(c.toString, "0..9")
        assert(! c.isEmpty)
        assertEq(c.size, 10)
        assert(c.isFinite)
        assert(! c.isInfinite)
        assert(! c.isSingleton)
        assert(! c.isSparse)
        assert(c.isDense)
        (0 to 9).foreach(i => assert(c.contains(IntegerValue.get(i))))
        assertEx(c.singleValue)
        assertEq(c.values.size, 10)
        assertEq(c.values.toList, List(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine))
        assert(c.isBounded)
        assert(! c.isUnbounded)
        assertEq(c.maybeLb.get, Zero)
        assertEq(c.maybeUb.get, Nine)
        assertEq(c.lb, Zero)
        assertEq(c.ub, Nine)
        assertEq(c.hull.lb, Zero)
        assertEq(c.hull.ub, Nine)
        assertEq(c.hull.size, 10)

        // [0, 9] \ {5}
        val d = IntegerDomainPruner.ne(c, Five)
        assertEq(d.size, 9)
        assertEq(d.toString, "0..4 union 6..9")
        assert(! d.contains(Five))
        assert(! d.isSparse)
        assert(d.isDense)
        assertEq(d.values.size, 9)
        assertEq(d.values.toList, List(Zero, One, Two, Three, Four, Six, Seven, Eight, Nine))
        assert(d.isBounded)
        assert(! d.isUnbounded)
        assertEq(d.maybeLb.get, Zero)
        assertEq(d.maybeUb.get, Nine)
        assertEq(d.lb, Zero)
        assertEq(d.ub, Nine)
        assertEq(d.hull.lb, Zero)
        assertEq(d.hull.ub, Nine)
        assertEq(d.hull.size, 10)

        // [0, 9] \ {5, 7}
        val e = IntegerDomainPruner.ne(d, Seven)
        assertEq(e.size, 8)
        assertEq(e.toString, "0..4 union {6} union 8..9")
        assert(! e.contains(Seven))
        assert(! e.isSparse)
        assert(e.isDense)
        assertEq(e.values.size, 8)
        assertEq(e.values.toList, List(Zero, One, Two, Three, Four, Six, Eight, Nine))
        assert(e.isBounded)
        assert(! e.isUnbounded)
        assertEq(e.maybeLb.get, Zero)
        assertEq(e.maybeUb.get, Nine)
        assertEq(e.lb, Zero)
        assertEq(e.ub, Nine)
        assertEq(e.hull.lb, Zero)
        assertEq(e.hull.ub, Nine)
        assertEq(e.hull.size, 10)

        // [0, 9] \ {5, 6, 7}
        val f = IntegerDomainPruner.ne(e, Six)
        assertEq(f.size, 7)
        assertEq(f.toString, "0..4 union 8..9")
        assert(! f.contains(Six))
        assert(! f.isSparse)
        assert(f.isDense)
        assertEq(f.values.size, 7)
        assertEq(f.values.toList, List(Zero, One, Two, Three, Four, Eight, Nine))
        assert(f.isBounded)
        assert(! f.isUnbounded)
        assert(f.maybeLb.get == Zero)
        assert(f.maybeUb.get == Nine)
        assertEq(f.lb, Zero)
        assertEq(f.ub, Nine)
        assertEq(f.hull.lb, Zero)
        assertEq(f.hull.ub, Nine)
        assertEq(f.hull.size, 10)

    }

    @Test
    def testOperationsOnRandomFiniteIntegerDomains {
        val rg = new JavaRandomGenerator
        val d = new IntegerDomain(Zero, Nine)
        val SAMPLE_SIZE = 100
        val randomSample = createSubdomains(rg, d, SAMPLE_SIZE).toSet
        val edgeCases = Set(EmptyIntegerDomain, d) ++ d.values.map(a => new IntegerDomain(a))
        testOperationsOnFiniteIntegerDomains(
            rg,
            (randomSample ++ edgeCases).toSeq,
            new IntegerDomain(MinusTwo, Eleven).values.toSeq)
    }

    @Test
    def testRepresentationOfInfiniteIntegerDomains {
        val rg = new JavaRandomGenerator

        // ]-inf, +inf[
        val a = UnboundedIntegerDomain
        assertNe(a, "")
        assertEq(a, a)
        assertEq(a, new IntegerDomain(null, null))
        assertEq(a.toString, "-inf..+inf")
        assert(! a.isEmpty)
        assertEx(a.size)
        assert(! a.isFinite)
        assert(a.isInfinite)
        assert(! a.isSingleton)
        assert(! a.isSparse)
        assert(a.isDense)
        assert(a.contains(Zero))
        assertEx(a.singleValue)
        assertEx(a.randomValue(rg))
        assertEx(a.nextRandomValue(rg, Zero))
        assertEx(a.values)
        assert(! a.isBounded)
        assert(a.isUnbounded)
        assert(a.maybeLb.isEmpty)
        assert(a.maybeUb.isEmpty)
        assertEq(a.lb, null)
        assertEq(a.ub, null)

        // [0, +inf[
        val b = NonNegativeIntegerDomain
        assertNe(b, "")
        assertEq(b, b)
        assertEq(b, new IntegerDomain(Zero, null))
        assertEq(b.toString, "0..+inf")
        assert(! b.isEmpty)
        assertEx(b.size)
        assert(! b.isFinite)
        assert(b.isInfinite)
        assert(! b.isSingleton)
        assert(! b.isSparse)
        assert(b.isDense)
        assert(b.contains(Zero))
        assertEx(b.singleValue)
        assertEx(b.randomValue(rg))
        assertEx(b.nextRandomValue(rg, Zero))
        assertEx(b.values)
        assert(b.isBounded)
        assert(! b.isUnbounded)
        assertEq(b.maybeLb.get, Zero)
        assert(b.maybeUb.isEmpty)
        assertEq(b.lb, Zero)
        assertEq(b.ub, null)

    }

    @Test
    def testOperationsOnInfiniteIntegerDomains() {

        assert(UnboundedIntegerDomain.contains(MinusOne))
        assert(NegativeIntegerDomain.contains(MinusOne))
        assert(! NegativeIntegerDomain.contains(Zero))
        assert(! NonNegativeIntegerDomain.contains(MinusOne))
        assert(NonNegativeIntegerDomain.contains(Zero))

        for (d <- List(UnboundedIntegerDomain, NegativeIntegerDomain, NonNegativeIntegerDomain)) {
            assert(EmptyIntegerDomain.isSubsetOf(d))
            assert(! d.isSubsetOf(EmptyIntegerDomain))
            assert(! EmptyIntegerDomain.intersects(d))
            assert(! d.intersects(EmptyIntegerDomain))
            assertEq(EmptyIntegerDomain.intersect(d), EmptyIntegerDomain)
            assertEq(d.intersect(EmptyIntegerDomain), EmptyIntegerDomain)
            assertEq(EmptyIntegerDomain.maybeIntersectionSize(d), Some(0))
            assertEq(d.maybeIntersectionSize(EmptyIntegerDomain), Some(0))
            assertEq(EmptyIntegerDomain.unite(d), d)
            assertEq(d.unite(EmptyIntegerDomain), d)
            assertEq(EmptyIntegerDomain.subtract(d), EmptyIntegerDomain)
            assertEq(d.subtract(EmptyIntegerDomain), d)
            assertEq(EmptyIntegerDomain.maybeResidueSize(d), Some(0))
            assertEq(d.maybeResidueSize(EmptyIntegerDomain), None)
            assertEq(IntegerDomainPruner.eq(d, EmptyIntegerDomain), EmptyIntegerDomain)
            assertEq(IntegerDomainPruner.eq(EmptyIntegerDomain, d), EmptyIntegerDomain)
            assertEq(IntegerDomainPruner.le(d, EmptyIntegerDomain), (EmptyIntegerDomain, EmptyIntegerDomain))
            assertEq(IntegerDomainPruner.le(EmptyIntegerDomain, d), (EmptyIntegerDomain, EmptyIntegerDomain))
            assertEq(IntegerDomainPruner.lt(d, EmptyIntegerDomain), (EmptyIntegerDomain, EmptyIntegerDomain))
            assertEq(IntegerDomainPruner.lt(EmptyIntegerDomain, d), (EmptyIntegerDomain, EmptyIntegerDomain))
        }

        for (d <- List(NegativeIntegerDomain, NonNegativeIntegerDomain)) {
            assert(d.isSubsetOf(UnboundedIntegerDomain))
            assert(! UnboundedIntegerDomain.isSubsetOf(d))
            assert(d.intersects(UnboundedIntegerDomain))
            assert(UnboundedIntegerDomain.intersects(d))
            assertEq(d.intersect(UnboundedIntegerDomain), d)
            assertEq(UnboundedIntegerDomain.intersect(d), d)
            assertEq(d.maybeIntersectionSize(UnboundedIntegerDomain), None)
            assertEq(UnboundedIntegerDomain.maybeIntersectionSize(d), None)
            assertEq(d.unite(UnboundedIntegerDomain), UnboundedIntegerDomain)
            assertEq(UnboundedIntegerDomain.unite(d), UnboundedIntegerDomain)
            assertEq(d.subtract(UnboundedIntegerDomain), EmptyIntegerDomain)
            assertEq(d.maybeResidueSize(UnboundedIntegerDomain), Some(0))
            assertEq(IntegerDomainPruner.eq(d, UnboundedIntegerDomain), d)
            assertEq(IntegerDomainPruner.eq(UnboundedIntegerDomain, d), d)
        }

        assert(! NegativeIntegerDomain.intersects(NonNegativeIntegerDomain))
        assert(! NonNegativeIntegerDomain.intersects(NegativeIntegerDomain))
        assertEq(NegativeIntegerDomain.intersect(NonNegativeIntegerDomain), EmptyIntegerDomain)
        assertEq(NonNegativeIntegerDomain.intersect(NegativeIntegerDomain), EmptyIntegerDomain)
        assertEq(NegativeIntegerDomain.maybeIntersectionSize(NonNegativeIntegerDomain), Some(0))
        assertEq(NonNegativeIntegerDomain.maybeIntersectionSize(NegativeIntegerDomain), Some(0))
        assertEq(UnboundedIntegerDomain.subtract(NegativeIntegerDomain), NonNegativeIntegerDomain)
        assertEq(UnboundedIntegerDomain.subtract(NonNegativeIntegerDomain), NegativeIntegerDomain)
        assertEq(UnboundedIntegerDomain.maybeResidueSize(NegativeIntegerDomain), None)
        assertEq(UnboundedIntegerDomain.maybeResidueSize(NonNegativeIntegerDomain), None)
        assertEq(NegativeIntegerDomain.unite(NonNegativeIntegerDomain), UnboundedIntegerDomain)
        assertEq(NonNegativeIntegerDomain.unite(NegativeIntegerDomain), UnboundedIntegerDomain)
        assertEq(
            IntegerDomainPruner.eq(NonNegativeIntegerDomain, NegativeIntegerDomain),
            EmptyIntegerDomain)
        assertEq(
            IntegerDomainPruner.eq(NegativeIntegerDomain, NonNegativeIntegerDomain),
            EmptyIntegerDomain)
        assertEq(
            IntegerDomainPruner.le(NonNegativeIntegerDomain, NegativeIntegerDomain),
            (EmptyIntegerDomain, EmptyIntegerDomain))
        assertEq(
            IntegerDomainPruner.le(NegativeIntegerDomain, NonNegativeIntegerDomain),
            (NegativeIntegerDomain, NonNegativeIntegerDomain))
        assertEq(
            IntegerDomainPruner.lt(NonNegativeIntegerDomain, NegativeIntegerDomain),
            (EmptyIntegerDomain, EmptyIntegerDomain))
        assertEq(
            IntegerDomainPruner.lt(NegativeIntegerDomain, NonNegativeIntegerDomain),
            (NegativeIntegerDomain, NonNegativeIntegerDomain))
        assertEq(
            IntegerDomainPruner.le(NonNegativeIntegerDomain, UnboundedIntegerDomain),
            (NonNegativeIntegerDomain, NonNegativeIntegerDomain))
        assertEq(
            IntegerDomainPruner.le(UnboundedIntegerDomain, NonNegativeIntegerDomain),
            (UnboundedIntegerDomain, NonNegativeIntegerDomain))
        assertEq(
            IntegerDomainPruner.lt(NonNegativeIntegerDomain, UnboundedIntegerDomain),
            (NonNegativeIntegerDomain, PositiveIntegerDomain))
        assertEq(
            IntegerDomainPruner.lt(UnboundedIntegerDomain, NonNegativeIntegerDomain),
            (UnboundedIntegerDomain, NonNegativeIntegerDomain))

        for (d <- List(UnboundedIntegerDomain, NegativeIntegerDomain, NonNegativeIntegerDomain)) {
            testBounding(d, Zero)
            testBisecting(d)
        }

    }

    @Test
    def testCasting {
        IntegerValueTraits.staticDowncast(UnboundedIntegerDomain)
        assertEx(IntegerValueTraits.dynamicDowncast(UnboundedBooleanDomain))
        IntegerValueTraits.dynamicDowncast(UnboundedIntegerDomain)
    }

}
