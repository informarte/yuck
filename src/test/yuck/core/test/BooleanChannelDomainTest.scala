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
final class BooleanChannelDomainTest extends UnitTest {

    @Test
    def testEverything: Unit = {
        val randomGenerator = new JavaRandomGenerator
        val d = BooleanChannelDomain
        assertEq(d, d)
        assertNe(d, CompleteBooleanDecisionDomain)
        assertEq(d.toString, "{true, false, false(2), ...}")
        assertEx(d.size)
        assert(d.isComplete)
        assert(! d.isFinite)
        assert(! d.isEmpty)
        assert(! d.isSingleton)
        assertEx(d.values)
        assertEx(d.singleValue)
        assert(d.contains(True))
        assert(d.contains(False))
        assert(d.contains(False2))
        assertEx(d.randomValue(randomGenerator))
        assertEx(d.nextRandomValue(randomGenerator, True))
        assert(d.isSubsetOf(d))
        assert(d.intersects(d))
        assertEq(d.intersect(d), d)
        assertEq(d.union(d), d)
        assertEq(d.diff(d), EmptyBooleanDomain)
        assertEq(d.symdiff(d), EmptyBooleanDomain)
        assertEq(d.compare(d), 0)
        assert(d.isBounded)
        assert(d.hasLb)
        assert(! d.hasUb)
        assertEq(d.maybeLb.get, True)
        assert(d.maybeUb.isEmpty)
        assertEq(d.lb, True)
        assertEq(d.ub, null)
        assertEq(d.hull, d)
        assertEx(d.boundFromBelow(False))
        assertEx(d.boundFromAbove(False))
        assertEx(d.bisect)
    }

}
