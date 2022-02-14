package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanDomainTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new OrderedDomainTestHelper[BooleanValue](logger, randomGenerator)
    private val testData =
        List(EmptyBooleanDomain, FalseDomain, TrueDomain, CompleteBooleanDecisionDomain, BooleanChannelDomain)

    @Test
    def testEquality: Unit = {
        helper.testEquality(testData)
        for (d <- testData) {
            for (e <- testData) {
                assert(if (d.eq(e)) d == e else d != e)
            }
        }
    }

    @Test
    def testOrdering: Unit = {
        helper.testOrdering(testData)
    }

    @Test
    def testSetOperations: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(a.asInstanceOf[BooleanDomain].isSubsetOf(b), a.isSubsetOf(b))
                assertEq(a.asInstanceOf[BooleanDomain].intersects(b), a.intersects(b))
                assertEq(a.asInstanceOf[BooleanDomain].intersect(b), a.intersect(b))
                assertEq(a.asInstanceOf[BooleanDomain].union(b), a.union(b))
                if (! a.isComplete || b.isEmpty) {
                    assertEq(a.asInstanceOf[BooleanDomain].diff(b), a.diff(b))
                }
                if (! a.union(b).isComplete || ! a.intersects(b)) {
                    assertEq(a.asInstanceOf[BooleanDomain].symdiff(b), a.symdiff(b))
                }
                if (a.isComplete) {
                    if (! b.isComplete) {
                        assert(! a.isSubsetOf(b))
                    }
                    assert(a.intersects(b))
                    assertEq(a.intersect(b), b)
                    assert(a.union(b).isComplete)
                    if (b.isEmpty) {
                        assert(a.diff(b).isComplete)
                        assert(a.symdiff(b).isComplete)
                    } else if (b.isComplete) {
                        assert(a.diff(b).isEmpty)
                    } else {
                        assertEx(a.diff(b))
                        assertEx(a.symdiff(b))
                    }
                } else if (b.isComplete) {
                    assert(a.isSubsetOf(b))
                    assert(a.intersects(b))
                    assertEq(a.intersect(b), a)
                    assert(a.union(b).isComplete)
                    assert(a.diff(b).isEmpty)
                    if (a.isEmpty) {
                        assert(a.symdiff(b).isComplete)
                    } else {
                        assertEx(a.symdiff(b))
                    }
                } else {
                    // see BooleanDecisionDomainTest
                }
            }
        }
    }

    @Test
    def testRandomSubdomainCreation: Unit = {
        for (a <- testData) {
            assertEx(a.randomSubdomain(randomGenerator), classOf[NotImplementedError])
        }
    }

    @Test
    def testProjection: Unit = {
        for (a <- testData) {
            val b = BooleanDomain.ensureDecisionDomain(a)
            if (a.isComplete) {
                assertEq(b, CompleteBooleanDecisionDomain)
            } else {
                assertEq(b, a)
            }
        }
    }

}
