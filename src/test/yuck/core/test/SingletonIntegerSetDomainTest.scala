package yuck.core.test

import org.junit.*

import scala.collection.*

import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class SingletonIntegerSetDomainTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerSetDomainTestHelper(randomGenerator, logger)
    private val baseRange = IntegerRange(IntegerValue(-5), Five)

    private def createTestData(sampleSize: Int): Seq[SingletonIntegerSetDomain] =
        helper.createTestData(baseRange, sampleSize)
            .filter(_.isInstanceOf[SingletonIntegerSetDomain])
            .map(_.asInstanceOf[SingletonIntegerSetDomain])

    @Test
    def testBasics(): Unit = {

        val randomGenerator = new JavaRandomGenerator

        // set domains
        val esd = new SingletonIntegerSetDomain(EmptyIntegerRange)
        val usd = new SingletonIntegerSetDomain(CompleteIntegerRange)

        // set values
        val es  = EmptyIntegerSetValue
        val us  = CompleteIntegerSetValue

        // {}
        assertNe(esd, "")
        assertNe(esd, Zero)
        assertNe(esd, False)
        assertEq(esd, esd)
        assertEq(esd.toString, "{{}}")
        assert(! esd.isEmpty)
        assertEq(esd.size, 1)
        assert(! esd.isComplete)
        assert(esd.isFinite)
        assert(esd.isSingleton)
        assert(esd.contains(es))
        assert(! esd.contains(us))
        assertEq(esd.singleValue, es)
        assertEq(esd.randomValue(randomGenerator), es)
        assertEq(esd.nextRandomValue(randomGenerator, es), es)
        assertEq(esd.values.toList, List(es))
        assertEq(esd.valuesIterator.toList, esd.values.toList)
        assert(esd.isBounded)
        assert(esd.hasLb)
        assert(esd.hasUb)
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
        assert(! usd.isComplete)
        assert(usd.isFinite)
        assert(usd.isSingleton)
        assert(usd.contains(us))
        assert(! usd.contains(es))
        assertEq(usd.singleValue, us)
        assertEq(usd.values.toList, List(us))
        assertEq(usd.valuesIterator.toList, usd.values.toList)
        assertEq(usd.randomValue(randomGenerator), us)
        assertEq(usd.nextRandomValue(randomGenerator, us), us)
        assert(usd.isBounded)
        assert(usd.hasLb)
        assert(usd.hasUb)
        assert(usd.maybeLb.isDefined)
        assert(usd.maybeUb.isDefined)
        assertEq(usd.maybeLb.get, usd.lb)
        assertEq(usd.maybeUb.get, usd.ub)
        assertEq(usd.lb, us)
        assertEq(usd.ub, us)
        assertEq(usd.hull, usd)

    }

    @Test
    def testEquality(): Unit = {
        val sampleSize = 16
        val testData = createTestData(sampleSize).distinct
        helper.testEquality(testData)
        for (d <- testData) {
            val e = SingletonIntegerSetDomain(d.base)
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
    def testOrdering(): Unit = {
        val sampleSize = 8
        val testData = createTestData(sampleSize)
        helper.testOrdering(testData)
    }

    @Test
    def testSetOperations(): Unit = {
        val esd = new SingletonIntegerSetDomain(EmptyIntegerRange)
        val usd = new SingletonIntegerSetDomain(CompleteIntegerRange)
        assert(esd.isSubsetOf(esd))
        assert(usd.isSubsetOf(usd))
        assert(! esd.isSubsetOf(usd))
        assert(! usd.isSubsetOf(esd))
        assert(esd.intersects(esd))
        assert(usd.intersects(usd))
        assert(! esd.intersects(usd))
        assert(! usd.intersects(esd))
    }

}
