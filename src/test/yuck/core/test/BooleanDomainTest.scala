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

    private val helper = new OrderedDomainTestHelper[BooleanValue](logger)
    private val testData =
        List(EmptyBooleanDomain, FalseDomain, TrueDomain, CompleteBooleanDecisionDomain, BooleanChannelDomain)

    @Test
    def testEquality: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                if ((a.isComplete && b.isComplete) || (a.isFinite && b.isFinite && a.values == b.values)) {
                    assertEq(a.asInstanceOf[BooleanDomain], b)
                } else {
                    assertNe(a.asInstanceOf[BooleanDomain], b)
                }
            }
        }
    }

    @Test
    def testOrdering: Unit = {
        helper.testOrdering(testData, BooleanDomain.ordering)
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
