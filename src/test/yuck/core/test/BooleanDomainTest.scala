package yuck.core.test

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}

import yuck.core.*
import yuck.core.test.util.{OrderingTestTooling, RandomValueSelectionTestTooling}
import yuck.test.util.UnitTest

@Execution(ExecutionMode.CONCURRENT)
final class BooleanDomainTest
    extends UnitTest
       with OrderingTestTooling[BooleanDomain]
       with RandomValueSelectionTestTooling[BooleanValue, BooleanDomain]
{

    private val testData1 = List((false, false), (true, false), (false, true), (true, true))
    private val testData2 = List(EmptyBooleanDomain, FalseDomain, TrueDomain, CompleteBooleanDomain)

    override protected val randomGenerator = new JavaRandomGenerator

    @Test
    def testBasics(): Unit = {
        for (f, t) <- testData1 do {
            val d = new BooleanDomain(f, t)
            assertEq(f, d.containsFalse)
            assertEq(t, d.containsTrue)
            assertEq(d, BooleanDomain(f, t))
            assertEq(d, BooleanDomain(if t then True else False, if f then False else True))
            assertEq(d.toString, if f && t then "{false, true}" else if f then "{false}" else if t then "{true}" else "{}")
            assertEq(d.values, if f && t then List(False, True) else if f then List(False) else if t then List(True) else Nil)
            assertEq(d.valuesIterator.toList, d.values)
            assertNe(d, "")
            assertEq(d, d)
            assertNe(d, new BooleanDomain(! f, t))
            assertNe(d, new BooleanDomain(f, ! t))
            assert(if f || t then (! d.isEmpty) else d.isEmpty)
            assertEq(d.size, (if f then 1 else 0) + (if t then 1 else 0))
            assertEq(d.size == 1, d.isSingleton)
            assert(d.isFinite)
            assertEq(f, d.contains(False))
            assertEq(t, d.contains(True))
            assertEq(d.isComplete, false)
            assert(d.isBounded)
            assert(d.hasLb)
            assert(d.hasUb)
            assertEq(d.maybeLb.get, d.lb)
            assertEq(d.maybeUb.get, d.ub)
            assertEq(d.hull, d)
            if d.isEmpty then {
                assertThrows(d.singleValue)
                assertThrows(d.randomValue(randomGenerator))
                assertThrows(d.nextRandomValue(randomGenerator, False))
                assertLt(d.ub, d.lb)
            } else if d.isSingleton then {
                assertEq(d.singleValue, if f then False else True)
                assertEq(d.randomValue(randomGenerator), d.singleValue)
                assertEq(d.nextRandomValue(randomGenerator, False), d.singleValue)
                assertEq(d.nextRandomValue(randomGenerator, True), d.singleValue)
                assertEq(d.lb, d.singleValue)
                assertEq(d.ub, d.singleValue)
            } else {
                assertThrows(d.singleValue)
                assertEq(d.nextRandomValue(randomGenerator, False), True)
                assertEq(d.nextRandomValue(randomGenerator, True), False)
                testUniformityOfDistribution(d)
                assertEq(d.lb, True)
                assertEq(d.ub, False)
            }
        }
    }

    @Test
    def testEquality(): Unit = {
        testEquality(testData2)
        for d <- testData2 do {
            val e = new BooleanDomain(d.containsFalse, d.containsTrue)
            assertEq(d, e)
            assertEq(e, d)
            assertNe(d, False)
            assertNe(False, d)
            for e <- testData2 do {
                assert(if d.eq(e) then d == e else d != e)
            }
        }
    }

    @Test
    def testOrdering(): Unit = {
        testOrdering(testData2)
    }

    @Test
    def testSetOperations(): Unit = {
        for (f1, t1) <- testData1 do {
            val d1 = new BooleanDomain(f1, t1)
            for (f2, t2) <- testData1 do {
                val d2 = new BooleanDomain(f2, t2)
                assertEq(d1.isSubsetOf(d2), (! f1 || f2) && (! t1 || t2))
                assertEq(d1.intersects(d2), (f1 && f2) || (t1 && t2))
                assertEq(d1.intersect(d2), new BooleanDomain(f1 && f2, t1 && t2))
                assertEq(d1.union(d2), new BooleanDomain(f1 || f2, t1 || t2))
                assertEq(d1.diff(d2), new BooleanDomain(f1 && ! f2, t1 && ! t2))
                assertEq(d1.symdiff(d2), d1.union(d2).diff(d1.intersect(d2)))
            }
        }
    }

    @Test
    def testRandomSubdomainCreation(): Unit = {
        for a <- testData2 do {
            assertThrows(a.randomSubdomain(randomGenerator), classOf[NotImplementedError])
        }
    }

    @Test
    def testConfiguration(): Unit = {
        import BooleanDomain.given
        def testOrdering()(using ordering: Ordering[BooleanDomain]) = {
            assertEq(ordering, BooleanDomainOrdering)
        }
        testOrdering()
    }

}
