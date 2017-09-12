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
final class BooleanDomainTest extends UnitTest {

    val helper = new DomainTestHelper[BooleanValue]

    @Test
    def testBasics {
        val randomGenerator = new JavaRandomGenerator
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
            assertEq(f, d.contains(False))
            assertEq(t, d.contains(True))
            assertEq(d.isComplete, f && t)
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
                assertEq(d.lb, False)
                assertEq(d.ub, True)
            }
        }
    }

    @Test
    def testOrdering {
        val helper = new OrderingTestHelper[BooleanDomain] {}
        val testData = List(EmptyBooleanDomain, FalseDomain, TrueDomain, CompleteBooleanDomain)
        val sortedTestData1 = helper.testOrdering(testData, BooleanValueTraits.domainOrdering)
        val sortedTestData2 = helper.testOrdering(testData, BooleanDomain.ordering)
        assertEq(sortedTestData1, sortedTestData2)
    }

    @Test
    def testSetOperations {
        for ((f1, t1) <- List((false, false), (true, false), (false, true), (true, true))) {
            val d1 = new BooleanDomain(f1, t1)
            for ((f2, t2) <- List((false, false), (true, false), (false, true), (true, true))) {
                val d2 = new BooleanDomain(f2, t2)
                assertEq(d1.isSubsetOf(d2), (! f1 || f2) && (! t1 || t2))
                assertEq(d1.intersects(d2), (f1 && f2) || (t1 && t2))
                assertEq(d1.union(d2), new BooleanDomain(f1 || f2, t1 || t2))
                assertEq(d1.diff(d2), new BooleanDomain(f1 && ! f2, t1 && ! t2))
                assertEq(d1.symdiff(d2), d1.union(d2).diff(d1.intersect(d2)))
            }
        }
    }

}
