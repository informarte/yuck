package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanDecisionDomainTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new OrderedDomainTestHelper[BooleanValue](logger, randomGenerator)
    private val testData1 = List((false, false), (true, false), (false, true), (true, true))
    private val testData2 = List(EmptyBooleanDomain, FalseDomain, TrueDomain, CompleteBooleanDecisionDomain)

    @Test
    def testBasics: Unit = {
        val randomGenerator = new JavaRandomGenerator
        for ((f, t) <- testData1) {
            val d = new BooleanDecisionDomain(f, t)
            assertEq(f, d.containsFalse)
            assertEq(t, d.containsTrue)
            assertEq(d, BooleanDecisionDomain(f, t))
            assertEq(d, BooleanDecisionDomain(if (t) True else False, if (f) False else True))
            assertEq(d.toString, if (f && t) "{false, true}" else if (f) "{false}" else if (t) "{true}" else "{}")
            assertEq(d.values, if (f && t) List(False, True) else if (f) List(False) else if (t) List(True) else Nil)
            assertEq(d.valuesIterator.toList, d.values)
            assertNe(d, "")
            assertEq(d, d)
            assertNe(d, new BooleanDecisionDomain(! f, t))
            assertNe(d, new BooleanDecisionDomain(f, ! t))
            assert(if (f || t) ! d.isEmpty else d.isEmpty)
            assertEq(d.size, (if (f) 1 else 0) + (if (t) 1 else 0))
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
            if (d.isEmpty) {
                assertEx(d.singleValue)
                assertEx(d.randomValue(randomGenerator))
                assertEx(d.nextRandomValue(randomGenerator, False))
                assertLt(d.ub, d.lb)
            } else if (d.isSingleton) {
                assertEq(d.singleValue, if (f) False else True)
                assertEq(d.randomValue(randomGenerator), d.singleValue)
                assertEq(d.nextRandomValue(randomGenerator, False), d.singleValue)
                assertEq(d.nextRandomValue(randomGenerator, True), d.singleValue)
                assertEq(d.lb, d.singleValue)
                assertEq(d.ub, d.singleValue)
            } else {
                assertEx(d.singleValue)
                assertEq(d.nextRandomValue(randomGenerator, False), True)
                assertEq(d.nextRandomValue(randomGenerator, True), False)
                helper.testUniformityOfDistribution(randomGenerator, d)
                assertEq(d.lb, True)
                assertEq(d.ub, False)
            }
        }
    }

    @Test
    def testEquality: Unit = {
        helper.testEquality(testData2)
        for (a <- testData2) {
            val b = new BooleanDecisionDomain(a.containsFalse, a.containsTrue)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, False)
            assertNe(False, a)
        }
    }

    @Test
    def testOrdering: Unit = {
        helper.testOrdering(testData2)
    }

    @Test
    def testSetOperations: Unit = {
        for ((f1, t1) <- testData1) {
            val d1 = new BooleanDecisionDomain(f1, t1)
            for ((f2, t2) <- testData1) {
                val d2 = new BooleanDecisionDomain(f2, t2)
                assertEq(d1.isSubsetOf(d2), (! f1 || f2) && (! t1 || t2))
                assertEq(d1.intersects(d2), (f1 && f2) || (t1 && t2))
                assertEq(d1.intersect(d2), new BooleanDecisionDomain(f1 && f2, t1 && t2))
                assertEq(d1.union(d2), new BooleanDecisionDomain(f1 || f2, t1 || t2))
                assertEq(d1.diff(d2), new BooleanDecisionDomain(f1 && ! f2, t1 && ! t2))
                assertEq(d1.symdiff(d2), d1.union(d2).diff(d1.intersect(d2)))
            }
        }
    }

}
