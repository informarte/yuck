package yuck.core.test

import org.junit.*

import scala.collection.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerPowersetDomainTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerSetDomainTestHelper(randomGenerator, logger)
    private val baseRange = IntegerRange(IntegerValue(-5), Five)

    @Test
    def testBasics: Unit = {
        val randomGenerator = new JavaRandomGenerator

        // base domains
        val ebd  = EmptyIntegerRange
        val ubd  = CompleteIntegerRange
        val bd0  = IntegerRange(Zero, Zero)
        val bd1  = IntegerRange(One, One)
        val bd2  = IntegerRange(Two, Two)
        val bd01 = IntegerRange(Zero, One)
        val bd02 = IntegerDomain(Set(Zero, Two))

        // set domains
        val esd = new IntegerPowersetDomain(ebd)
        val usd = new IntegerPowersetDomain(ubd)
        val sd0 = new IntegerPowersetDomain(bd0)
        val sd01 = new IntegerPowersetDomain(bd01)
        val sd02 = new IntegerPowersetDomain(bd02)

        // set values
        val es  = EmptyIntegerSetValue
        val us  = CompleteIntegerSetValue
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
        assert(! esd.isComplete)
        assert(esd.isFinite)
        assert(esd.isSingleton)
        assert(esd.contains(es))
        assert(! esd.contains(s0))
        assertEq(esd.singleValue, es)
        assertEq(esd.randomValue(randomGenerator), es)
        assertEx(esd.nextRandomValue(randomGenerator, es))
        assertEq(esd.values.toList, List(es))
        assertEq(esd.valuesIterator.toList, esd.values.toList)
        assert(esd.isBounded)
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
        assert(! sd0.isComplete)
        assert(sd0.isFinite)
        assert(! sd0.isSingleton)
        assert(sd0.contains(es))
        assert(sd0.contains(s0))
        assert(! sd0.contains(s1))
        assert(! sd0.contains(s01))
        assertEx(sd0.singleValue)
        assertEq(sd0.values.toList, List(es, s0))
        assertEq(sd0.valuesIterator.toList, sd0.values.toList)
        assert(sd0.isBounded)
        assert(sd0.maybeLb.isDefined)
        assert(sd0.maybeUb.isDefined)
        assertEq(sd0.maybeLb.get, sd0.lb)
        assertEq(sd0.maybeUb.get, sd0.ub)
        assertEq(sd0.lb, es)
        assertEq(sd0.ub, s0)
        assertEq(sd0.hull, sd0)
        helper.testUniformityOfDistribution(randomGenerator, sd0)

        // {0, 1}
        List(esd, sd0).foreach(s => assertNe(sd01, s))
        assertEq(sd01, sd01)
        assertEq(sd01.toString, "P(0..1)")
        assert(! sd01.isEmpty)
        assertEq(sd01.size, 4)
        assert(! sd01.isComplete)
        assert(sd01.isFinite)
        assert(! sd01.isSingleton)
        List(es, s0, s1, s01).foreach(s => assert(sd01.contains(s)))
        List(s2, s02).foreach(s => assert(! sd01.contains(s)))
        assertEx(sd01.singleValue)
        assertEq(sd01.values.toList, List(es, s0, s1, s01))
        assertEq(sd01.valuesIterator.toList, sd01.values.toList)
        assert(sd01.isBounded)
        assert(sd01.maybeLb.isDefined)
        assert(sd01.maybeUb.isDefined)
        assertEq(sd01.maybeLb.get, sd01.lb)
        assertEq(sd01.maybeUb.get, sd01.ub)
        assertEq(sd01.lb, es)
        assertEq(sd01.ub, s01)
        assertEq(sd01.hull, sd01)
        helper.testUniformityOfDistribution(randomGenerator, sd01)

        // {0, 2}
        List(esd, sd0, sd01).foreach(s => assertNe(sd02, s))
        assertEq(sd02, sd02)
        assertEq(sd02.toString, "P({0} union {2})")
        assert(! sd02.isEmpty)
        assertEq(sd02.size, 4)
        assert(! sd02.isComplete)
        assert(sd02.isFinite)
        assert(! sd02.isSingleton)
        List(es, s0, s2, s02).foreach(s => assert(sd02.contains(s)))
        List(s1, s01).foreach(s => assert(! sd02.contains(s)))
        assertEx(sd02.singleValue)
        assertEq(sd02.values.toList, List(es, s0, s2, s02))
        assertEq(sd02.valuesIterator.toList, sd02.values.toList)
        assert(sd02.isBounded)
        assert(sd02.maybeLb.isDefined)
        assert(sd02.maybeUb.isDefined)
        assertEq(sd02.maybeLb.get, sd02.lb)
        assertEq(sd02.maybeUb.get, sd02.ub)
        assertEq(sd02.lb, es)
        assertEq(sd02.ub, s02)
        assertEq(sd02.hull, sd02)
        helper.testUniformityOfDistribution(randomGenerator, sd02)

        // infinite domain
        List(esd, sd0, sd01, sd02).foreach(s => assertNe(usd, s))
        assertEq(usd, usd)
        assertEq(usd.toString, "P(-inf..+inf)")
        assert(! usd.isEmpty)
        assertEx(usd.size)
        assert(usd.isComplete)
        assert(! usd.isFinite)
        assert(! usd.isSingleton)
        List(es, us, s0, s1, s2, s01, s02).foreach(s => assert(usd.contains(s)))
        assertEx(usd.singleValue)
        assertEx(usd.values)
        assertEx(usd.valuesIterator)
        assertEx(usd.randomValue(randomGenerator))
        assertEx(usd.nextRandomValue(randomGenerator, es))
        assert(! usd.isBounded)
        assert(usd.maybeLb.isDefined)
        assert(usd.maybeUb.isDefined)
        assertEq(usd.maybeLb.get, usd.lb)
        assertEq(usd.maybeUb.get, usd.ub)
        assertEq(usd.lb, es)
        assertEq(usd.ub, us)
        assertEq(usd.hull, usd)

    }

    @Test
    def testEquality: Unit = {
        val sampleSize = 16
        val testData =
            helper.createTestData(baseRange, sampleSize)
                .filter(_.isInstanceOf[IntegerPowersetDomain]).map(_.asInstanceOf[IntegerPowersetDomain])
        helper.testEquality(testData)
        for (d <- testData) {
            val e = IntegerPowersetDomain(d.base)
            assertEq(d, e)
            assertEq(e, d)
            assertNe(d, False)
            assertNe(False, d)
            for (e <- testData) {
                assert(if (d.eq(e)) d == e else d != e)
            }
        }
    }

    @Test
    def testOrdering: Unit = {
        val sampleSize = 8
        val testData =
            helper.createTestData(baseRange, sampleSize)
                .filter(_.isInstanceOf[IntegerPowersetDomain]).map(_.asInstanceOf[IntegerPowersetDomain])
        helper.testOrdering(testData)
    }

    @Test
    def testSetOperations: Unit = {
        // All operations forward to their counterparts in IntegerDomain.
        // Hence we skip the testing.
    }

}
