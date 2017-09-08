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
                helper.testUniformityOfDistribution(rg, d)
                assertEq(d.lb, False)
                assertEq(d.ub, True)
            }
        }
    }

    @Test
    def testSetOperations {
        assert(! FalseDomain.isSubsetOf(EmptyBooleanDomain))
        assert(! TrueDomain.isSubsetOf(EmptyBooleanDomain))
        assert(! UnboundedBooleanDomain.isSubsetOf(EmptyBooleanDomain))
        assert(! FalseDomain.isSubsetOf(TrueDomain))
        assert(! TrueDomain.isSubsetOf(FalseDomain))
        assert(EmptyBooleanDomain.isSubsetOf(UnboundedBooleanDomain))
        assert(FalseDomain.isSubsetOf(UnboundedBooleanDomain))
        assert(TrueDomain.isSubsetOf(UnboundedBooleanDomain))
    }

    @Test
    def testCasting {
        BooleanValueTraits.staticDowncast(UnboundedBooleanDomain)
        BooleanValueTraits.dynamicDowncast(UnboundedBooleanDomain)
        assertEx(BooleanValueTraits.dynamicDowncast(UnboundedIntegerDomain))
    }

}
