package yuck.core.test

import org.junit._

import yuck.core._
import yuck.test.util.{OrderingTestHelper, UnitTest}

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetDomainTest extends UnitTest {

    // Test strategy:
    // * Test the type-based dispatching in IntegerSetDomain.
    // * Do not retest the IntegerSetDomain implementations.
    // * For each combination of IntegerSetDomain implementations, test the operation on two non-trivial examples
    //   with different results.
    // * Test that set operations return instances of OrderedDomain[IntegerSetValue].

    private val randomGenerator = new JavaRandomGenerator
    private val helper1 = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = IntegerRange(IntegerValue(-5), Five)
    private def createTestData(sampleSize: Int) =
        helper1
            .createTestData(baseRange, sampleSize)
            .flatMap(r => List(new SingletonIntegerSetDomain(r), new IntegerPowersetDomain(r)))

    @Test
    def testEquality: Unit = {
        assertEq(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain], new SingletonIntegerSetDomain(EmptyIntegerRange))
        assertNe(new SingletonIntegerSetDomain(PositiveIntegerRange).asInstanceOf[IntegerSetDomain], new SingletonIntegerSetDomain(NegativeIntegerRange))
        assertEq(new SingletonIntegerSetDomain(EmptyIntegerRange), new IntegerPowersetDomain(EmptyIntegerRange))
        assertNe(new SingletonIntegerSetDomain(CompleteIntegerRange), CompleteIntegerSetDomain)
        assertEq(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain], CompleteIntegerSetDomain)
        assertNe(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain], new IntegerPowersetDomain(EmptyIntegerRange))
    }

    // The ordering is implemented in IntegerSetDomain and works for all its subclasses,
    // so we test the ordering only once and here.
    @Test
    def testOrdering: Unit = {
        val sampleSize = 8
        val testData = createTestData(sampleSize)
        val helper2 = new OrderingTestHelper[OrderedDomain[IntegerSetValue]](randomGenerator)
        helper2.testOrdering(testData, IntegerSetDomainOrdering)
    }

    @Test
    def testSubsetRelation: Unit = {
        assert(EmptyIntegerSetDomain.isSubsetOf(EmptyIntegerSetDomain))
        assert(EmptyIntegerSetDomain.isSubsetOf(CompleteIntegerSetDomain))
        assert(! CompleteIntegerSetDomain.isSubsetOf(EmptyIntegerSetDomain))
        assert(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].isSubsetOf(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assert(! new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].isSubsetOf(new SingletonIntegerSetDomain(CompleteIntegerRange)))
        assert(new SingletonIntegerSetDomain(EmptyIntegerRange).isSubsetOf(CompleteIntegerSetDomain))
        assert(! new SingletonIntegerSetDomain(PositiveIntegerRange).isSubsetOf(new IntegerPowersetDomain(NegativeIntegerRange)))
        assert(new IntegerPowersetDomain(PositiveIntegerRange).asInstanceOf[IntegerSetDomain].isSubsetOf(new IntegerPowersetDomain(NonNegativeIntegerRange)))
        assert(! new IntegerPowersetDomain(NonNegativeIntegerRange).asInstanceOf[IntegerSetDomain].isSubsetOf(new IntegerPowersetDomain(PositiveIntegerRange)))
        assert(new IntegerPowersetDomain(EmptyIntegerRange).isSubsetOf(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assert(! CompleteIntegerSetDomain.isSubsetOf(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testSetIntersectionRelation: Unit = {
        assert(! EmptyIntegerSetDomain.intersects(EmptyIntegerSetDomain))
        assert(! EmptyIntegerSetDomain.intersects(CompleteIntegerSetDomain))
        assert(! CompleteIntegerSetDomain.intersects(EmptyIntegerSetDomain))
        assert(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].intersects(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assert(! new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].intersects(new SingletonIntegerSetDomain(CompleteIntegerRange)))
        assert(new SingletonIntegerSetDomain(EmptyIntegerRange).intersects(CompleteIntegerSetDomain))
        assert(! new SingletonIntegerSetDomain(NonPositiveIntegerRange).intersects(new SingletonIntegerSetDomain(NonNegativeIntegerRange)))
        assert(new IntegerPowersetDomain(NonPositiveIntegerRange).asInstanceOf[IntegerSetDomain].intersects(new IntegerPowersetDomain(NegativeIntegerRange)))
        assert(! new IntegerPowersetDomain(PositiveIntegerRange).asInstanceOf[IntegerSetDomain].intersects(new IntegerPowersetDomain(NegativeIntegerRange)))
        assert(CompleteIntegerSetDomain.intersects(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assert(! new IntegerPowersetDomain(NonPositiveIntegerRange).intersects(new SingletonIntegerSetDomain(NonNegativeIntegerRange)))
    }

    @Test
    def testSetIntersection: Unit = {
        assertEq(EmptyIntegerSetDomain.intersect(EmptyIntegerSetDomain), EmptyIntegerSetDomain)
        assertEq(EmptyIntegerSetDomain.intersect(CompleteIntegerSetDomain), EmptyIntegerSetDomain)
        assertEq(CompleteIntegerSetDomain.intersect(EmptyIntegerSetDomain), EmptyIntegerSetDomain)
        assertEq(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].intersect(new SingletonIntegerSetDomain(EmptyIntegerRange)), new SingletonIntegerSetDomain(EmptyIntegerRange))
        assertEq(new SingletonIntegerSetDomain(EmptyIntegerRange).intersect(CompleteIntegerSetDomain), new SingletonIntegerSetDomain(EmptyIntegerRange))
        assertEq(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].intersect(new IntegerPowersetDomain(EmptyIntegerRange)), new IntegerPowersetDomain(EmptyIntegerRange))
        assertEq(new IntegerPowersetDomain(NonPositiveIntegerRange).asInstanceOf[IntegerSetDomain].intersect(new IntegerPowersetDomain(NonNegativeIntegerRange)), new IntegerPowersetDomain(ZeroToZeroIntegerRange))
        assertEq(CompleteIntegerSetDomain.intersect(new SingletonIntegerSetDomain(EmptyIntegerRange)), new SingletonIntegerSetDomain(EmptyIntegerRange))
        assertEq(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].intersect(CompleteIntegerSetDomain), CompleteIntegerSetDomain)
    }

    @Test
    def testSetUnion: Unit = {
        assertEq(EmptyIntegerSetDomain.union(EmptyIntegerSetDomain), EmptyIntegerSetDomain)
        assertEq(EmptyIntegerSetDomain.union(CompleteIntegerSetDomain), CompleteIntegerSetDomain)
        assertEq(CompleteIntegerSetDomain.union(EmptyIntegerSetDomain), CompleteIntegerSetDomain)
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].union(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).union(CompleteIntegerSetDomain))
        assertNie(new IntegerPowersetDomain(NonPositiveIntegerRange).asInstanceOf[IntegerSetDomain].union(new IntegerPowersetDomain(NonNegativeIntegerRange)))
        assertNie(CompleteIntegerSetDomain.union(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testSetDifference: Unit = {
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].diff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).diff(CompleteIntegerSetDomain))
        assertNie(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].diff(CompleteIntegerSetDomain))
        assertNie(CompleteIntegerSetDomain.diff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testSymmetricalSetDifference: Unit = {
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].symdiff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).symdiff(CompleteIntegerSetDomain))
        assertNie(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].symdiff(CompleteIntegerSetDomain))
        assertNie(CompleteIntegerSetDomain.symdiff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testRandomSubdomainCreation: Unit = {
        val sampleSize = 8
        val testData = createTestData(sampleSize)
        for (a <- testData) {
            assertEx(a.randomSubdomain(randomGenerator), classOf[NotImplementedError])
        }
    }

}
