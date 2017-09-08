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
final class SingletonIntegerSetDomainTest extends UnitTest {

    @Test
    def testBasics {
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
    def testCasting {
        IntegerSetValueTraits.staticDowncast(new SingletonIntegerSetDomain(UnboundedIntegerDomain))
        IntegerSetValueTraits.dynamicDowncast(new SingletonIntegerSetDomain(UnboundedIntegerDomain))
    }

}
