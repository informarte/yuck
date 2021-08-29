package yuck.core.test

import org.junit._

import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class EmptyIntegerSetDomainTest extends UnitTest {

    @Test
    def testEverything: Unit = {
        val randomGenerator = new JavaRandomGenerator
        val d = EmptyIntegerSetDomain
        assertEq(d, d)
        assertNe(d, CompleteIntegerSetDomain)
        assertEq(d.toString, "{}")
        assertEq(d.size, 0)
        assert(! d.isComplete)
        assert(d.isFinite)
        assert(d.isEmpty)
        assert(! d.isSingleton)
        assert(d.values.isEmpty)
        assert(! d.valuesIterator.hasNext)
        assertEx(d.singleValue)
        assert(! d.contains(EmptyIntegerSetValue))
        assert(! d.contains(CompleteIntegerSetValue))
        assertEx(d.randomValue(randomGenerator))
        assertEx(d.nextRandomValue(randomGenerator, EmptyIntegerSetValue))
        assert(d.isSubsetOf(d))
        assert(! d.intersects(d))
        assertEq(d.intersect(d), d)
        assertEq(d.union(d), d)
        assertEq(d.diff(d), d)
        assertEq(d.symdiff(d), d)
        assertEq(d.compare(d), 0)
        assert(d.isBounded)
        assert(d.hasLb)
        assert(d.hasUb)
        assertEq(d.maybeLb.get, CompleteIntegerSetValue)
        assertEq(d.maybeUb.get, EmptyIntegerSetValue)
        assertEq(d.lb, CompleteIntegerSetValue)
        assertEq(d.ub, EmptyIntegerSetValue)
        assertEq(d.hull, d)
    }

}
