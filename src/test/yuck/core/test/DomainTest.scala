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
class DomainTest extends UnitTest {

    // checks that values are chosen uniformly from the given domain
    private def testUniformityOfDistribution[Value <: OrderedValue[Value]](
        rg: RandomGenerator,
        d: Domain[Value])
    {
        val SAMPLE_SIZE = 100000
        val MAX_ERROR = 0.05
        def checkDistribution(f: Map[Value, Int]) {
            for (a <- d.values) {
                assertGt(f.getOrElse(a, 0), SAMPLE_SIZE / d.size * (1 - MAX_ERROR))
                assertLt(f.getOrElse(a, 0), SAMPLE_SIZE / d.size * (1 + MAX_ERROR))
            }
        }
        val f1 = new mutable.HashMap[Value, Int]
        val f2 = new mutable.HashMap[Value, Int]
        for (i <- 1 to SAMPLE_SIZE) {
            val a = d.randomValue(rg)
            assert(d.contains(a))
            f1.put(a, f1.getOrElse(a, 0) + 1)
            val b = d.nextRandomValue(rg, a)
            assert(d.contains(b))
            assertNe(a, b)
            f2.put(b, f2.getOrElse(b, 0) + 1)
        }
        checkDistribution(f1)
        checkDistribution(f2)
    }

    @Test
    def testBooleanDomain {
        val rg = new JavaRandomGenerator
        for ((f, t) <- List((false, false), (true, false), (false, true), (true, true))) {
            val d = new BooleanDomain(f, t)
            if (f && t) {
                assertEq(d.toString, "{false, true}")
            } else if (f) {
                assertEq(d.toString, "{false}")
            } else if (t) {
                assertEq(d.toString, "{true}")
            } else {
                assertEq(d.toString, "{}")
            }
            assertNe(d, "")
            assertEq(d, d)
            assertNe(d, new BooleanDomain(! f, t))
            assert(if (f || t) ! d.isEmpty else d.isEmpty)
            assertEq(d.size, (if (f) 1 else 0) + (if (t) 1 else 0))
            assertEq(d.size == 1, d.isSingleton)
            assert(d.isFinite)
            assert(! d.isInfinite)
            assertEq(f, d.contains(False))
            assertEq(t, d.contains(True))
            assert(d.isBounded)
            assert(! d.isUnbounded)
            assertEq(d.maybeLb.get, d.lb)
            assertEq(d.maybeUb.get, d.ub)
            assertEq(d.hull, d)
            if (d.isEmpty) {
                assertEx(d.singleValue)
                assertEx(d.randomValue(rg))
                assertEx(d.nextRandomValue(rg, False))
                assertLt(d.ub, d.lb)
            } else if (d.isSingleton) {
                assertEq(d.singleValue, if (f) False else True)
                assertEq(d.randomValue(rg), d.singleValue)
                assertEq(d.nextRandomValue(rg, False), d.singleValue)
                assertEq(d.nextRandomValue(rg, True), d.singleValue)
                assertEq(d.lb, d.singleValue)
                assertEq(d.ub, d.singleValue)
            } else {
                assertEx(d.singleValue)
                assertEq(d.nextRandomValue(rg, False), True)
                assertEq(d.nextRandomValue(rg, True), False)
                testUniformityOfDistribution(rg, d)
                assertEq(d.lb, False)
                assertEq(d.ub, True)
            }
        }
        assert(! UnboundedBooleanDomain.isSubsetOf(EmptyBooleanDomain))
        assert(EmptyBooleanDomain.isSubsetOf(UnboundedBooleanDomain))
    }

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
                    testUniformityOfDistribution(rg, d)
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
    def testIntegerPowersetDomain {
        val rg = new JavaRandomGenerator

        // base domains
        val ebd  = EmptyIntegerDomain
        val ubd  = UnboundedIntegerDomain
        val bd0  = new IntegerDomain(Zero)
        val bd1  = new IntegerDomain(One)
        val bd2  = new IntegerDomain(Two)
        val bd01 = new IntegerDomain(Zero, One)
        val bd02 = new IntegerDomain(Set(Zero, Two))

        // set domains
        val esd = new IntegerPowersetDomain(ebd)
        val usd = new IntegerPowersetDomain(ubd)
        val sd0 = new IntegerPowersetDomain(bd0)
        val sd01 = new IntegerPowersetDomain(bd01)
        val sd02 = new IntegerPowersetDomain(bd02)

        // set values
        val es  = EmptyIntegerSet
        val us  = UnboundedIntegerSet
        val s0  = new IntegerSetValue(bd0)
        val s1  = new IntegerSetValue(bd1)
        val s2  = new IntegerSetValue(bd2)
        val s01 = new IntegerSetValue(bd01)
        val s02 = new IntegerSetValue(bd02)

        // {}
        assertNe(esd, "")
        assertNe(esd, Zero)
        assertNe(esd, False)
        assertEq(esd, esd)
        assertEq(esd.toString, "P({})")
        assert(! esd.isEmpty)
        assertEq(esd.size, 1)
        assert(esd.isFinite)
        assert(! esd.isInfinite)
        assert(esd.isSingleton)
        assert(esd.contains(es))
        assert(! esd.contains(s0))
        assertEq(esd.singleValue, es)
        assertEq(esd.randomValue(rg), es)
        assertEx(esd.nextRandomValue(rg, es))
        assertEq(esd.values.toList, List(es))
        assert(esd.isBounded)
        assert(! esd.isUnbounded)
        assert(esd.maybeLb.isDefined)
        assert(esd.maybeUb.isDefined)
        assertEq(esd.maybeLb.get, esd.lb)
        assertEq(esd.maybeUb.get, esd.ub)
        assertEq(esd.lb, es)
        assertEq(esd.ub, es)
        assertEq(esd.hull, esd)

        // {0}
        assertEq(sd0, sd0)
        assertEq(sd0.toString, "P({0})")
        assert(! sd0.isEmpty)
        assertEq(sd0.size, 2)
        assert(sd0.isFinite)
        assert(! sd0.isInfinite)
        assert(! sd0.isSingleton)
        assert(sd0.contains(es))
        assert(sd0.contains(s0))
        assert(! sd0.contains(s1))
        assert(! sd0.contains(s01))
        assertEx(sd0.singleValue)
        assertEq(sd0.values.toList, List(es, s0))
        assert(sd0.isBounded)
        assert(! sd0.isUnbounded)
        assert(sd0.maybeLb.isDefined)
        assert(sd0.maybeUb.isDefined)
        assertEq(sd0.maybeLb.get, sd0.lb)
        assertEq(sd0.maybeUb.get, sd0.ub)
        assertEq(sd0.lb, es)
        assertEq(sd0.ub, s0)
        assertEq(sd0.hull, sd0)
        testUniformityOfDistribution(rg, sd0)

        // {0, 1}
        List(esd, sd0).foreach(s => assertNe(sd01, s))
        assertEq(sd01, sd01)
        assertEq(sd01.toString, "P(0..1)")
        assert(! sd01.isEmpty)
        assertEq(sd01.size, 4)
        assert(sd01.isFinite)
        assert(! sd01.isInfinite)
        assert(! sd01.isSingleton)
        List(es, s0, s1, s01).foreach(s => assert(sd01.contains(s)))
        List(s2, s02).foreach(s => assert(! sd01.contains(s)))
        assertEx(sd01.singleValue)
        assertEq(sd01.values.toList, List(es, s0, s1, s01))
        assert(sd01.isBounded)
        assert(! sd01.isUnbounded)
        assert(sd01.maybeLb.isDefined)
        assert(sd01.maybeUb.isDefined)
        assertEq(sd01.maybeLb.get, sd01.lb)
        assertEq(sd01.maybeUb.get, sd01.ub)
        assertEq(sd01.lb, es)
        assertEq(sd01.ub, s01)
        assertEq(sd01.hull, sd01)
        testUniformityOfDistribution(rg, sd01)

        // {0, 2}
        List(esd, sd0, sd01).foreach(s => assertNe(sd02, s))
        assertEq(sd02, sd02)
        assertEq(sd02.toString, "P({0} union {2})")
        assert(! sd02.isEmpty)
        assertEq(sd02.size, 4)
        assert(sd02.isFinite)
        assert(! sd02.isInfinite)
        assert(! sd02.isSingleton)
        List(es, s0, s2, s02).foreach(s => assert(sd02.contains(s)))
        List(s1, s01).foreach(s => assert(! sd02.contains(s)))
        assertEx(sd02.singleValue)
        assertEq(sd02.values.toList, List(es, s0, s2, s02))
        assert(sd02.isBounded)
        assert(! sd02.isUnbounded)
        assert(sd02.maybeLb.isDefined)
        assert(sd02.maybeUb.isDefined)
        assertEq(sd02.maybeLb.get, sd02.lb)
        assertEq(sd02.maybeUb.get, sd02.ub)
        assertEq(sd02.lb, es)
        assertEq(sd02.ub, s02)
        assertEq(sd02.hull, sd02)
        testUniformityOfDistribution(rg, sd02)

        // infinite domain
        List(esd, sd0, sd01, sd02).foreach(s => assertNe(usd, s))
        assertEq(usd, usd)
        assertEq(usd.toString, "P(-inf..+inf)")
        assert(! usd.isEmpty)
        assertEx(usd.size)
        assert(! usd.isFinite)
        assert(usd.isInfinite)
        assert(! usd.isSingleton)
        List(es, us, s0, s1, s2, s01, s02).foreach(s => assert(usd.contains(s)))
        assertEx(usd.singleValue)
        assertEx(usd.values)
        assertEx(usd.randomValue(rg))
        assertEx(usd.nextRandomValue(rg, es))
        assert(! usd.isBounded)
        assert(usd.isUnbounded)
        assert(usd.maybeLb.isDefined)
        assert(usd.maybeUb.isDefined)
        assertEq(usd.maybeLb.get, usd.lb)
        assertEq(usd.maybeUb.get, usd.ub)
        assertEq(usd.lb, es)
        assertEq(usd.ub, us)
        assertEq(usd.hull, usd)

    }

    @Test
    def testSingletonIntegerSetDomain {
        val rg = new JavaRandomGenerator

        // base domains
        val ebd  = EmptyIntegerDomain
        val ubd  = UnboundedIntegerDomain

        // set domains
        val esd = new SingletonIntegerSetDomain(ebd)
        val usd = new SingletonIntegerSetDomain(ubd)

        // set values
        val es  = EmptyIntegerSet
        val us  = UnboundedIntegerSet

        // {}
        assertNe(esd, "")
        assertNe(esd, Zero)
        assertNe(esd, False)
        assertEq(esd, esd)
        assertEq(esd.toString, "{{}}")
        assert(! esd.isEmpty)
        assertEq(esd.size, 1)
        assert(esd.isFinite)
        assert(! esd.isInfinite)
        assert(esd.isSingleton)
        assert(esd.contains(es))
        assert(! esd.contains(us))
        assertEq(esd.singleValue, es)
        assertEq(esd.randomValue(rg), es)
        assertEx(esd.nextRandomValue(rg, es))
        assertEq(esd.values.toList, List(es))
        assert(esd.isBounded)
        assert(! esd.isUnbounded)
        assert(esd.maybeLb.isDefined)
        assert(esd.maybeUb.isDefined)
        assertEq(esd.maybeLb.get, esd.lb)
        assertEq(esd.maybeUb.get, esd.ub)
        assertEq(esd.lb, es)
        assertEq(esd.ub, es)
        assertEq(esd.hull, esd)

        // infinite domain
        assertNe(usd, esd)
        assertEq(usd, usd)
        assertEq(usd.toString, "{-inf..+inf}")
        assert(! usd.isEmpty)
        assertEq(usd.size, 1)
        assert(usd.isFinite)
        assert(! usd.isInfinite)
        assert(usd.isSingleton)
        assert(usd.contains(us))
        assert(! usd.contains(es))
        assertEq(usd.singleValue, us)
        assertEq(usd.values.toList, List(us))
        assertEq(usd.randomValue(rg), us)
        assertEx(usd.nextRandomValue(rg, es))
        assert(usd.isBounded)
        assert(! usd.isUnbounded)
        assert(usd.maybeLb.isDefined)
        assert(usd.maybeUb.isDefined)
        assertEq(usd.maybeLb.get, usd.lb)
        assertEq(usd.maybeUb.get, usd.ub)
        assertEq(usd.lb, us)
        assertEq(usd.ub, us)
        assertEq(usd.hull, usd)

    }

    @Test
    def testDomainCasting {
        BooleanValue.Traits.staticCast(UnboundedBooleanDomain)
        BooleanValue.Traits.staticCast(UnboundedIntegerDomain)
        BooleanValue.Traits.dynamicCast(UnboundedBooleanDomain)
        assertEx(BooleanValue.Traits.dynamicCast(UnboundedIntegerDomain))
        assertEx(IntegerValue.Traits.dynamicCast(UnboundedBooleanDomain))
        IntegerValue.Traits.dynamicCast(UnboundedIntegerDomain)
        IntegerSetValue.Traits.dynamicCast(new IntegerPowersetDomain(UnboundedIntegerDomain))
        IntegerSetValue.Traits.dynamicCast(new SingletonIntegerSetDomain(UnboundedIntegerDomain))
    }

}
