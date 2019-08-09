package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.{OrderingTestHelper, UnitTest}

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetDomainTest extends UnitTest {

    // Test strategy:
    // * Test the type-based dispatching in IntegerSetDomain.
    // * Do not retest the IntegerSetDomain implementations.
    // * For each combination of IntegerSetDomain implementations, test the operation on two non-trivial examples
    //   with different results.
    // * Test that set operations return instances of OrderedDomain[IntegerSetValue].

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
        val randomGenerator = new JavaRandomGenerator
        val helper1 = new IntegerDomainTestHelper(randomGenerator, logger)
        val baseRange = new IntegerRange(IntegerValue.get(-5), Five)
        val testData =
            helper1.createTestData(baseRange, sampleSize)
                .flatMap(r => List(new SingletonIntegerSetDomain(r), new IntegerPowersetDomain(r)))
        val helper2 = new OrderingTestHelper[IntegerSetDomain]
        helper2.testOrdering(testData, IntegerSetDomain.ordering)
    }

    @Test
    def testSubsetRelation: Unit = {
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
        assertEx(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].intersect(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertEx(new SingletonIntegerSetDomain(EmptyIntegerRange).intersect(CompleteIntegerSetDomain))
        assertEq(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].intersect(new IntegerPowersetDomain(EmptyIntegerRange)), new IntegerPowersetDomain(EmptyIntegerRange))
        assertEq(new IntegerPowersetDomain(NonPositiveIntegerRange).asInstanceOf[IntegerSetDomain].intersect(new IntegerPowersetDomain(NonNegativeIntegerRange)), new IntegerPowersetDomain(ZeroToZeroIntegerRange))
        assertEx(CompleteIntegerSetDomain.intersect(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].intersect(CompleteIntegerSetDomain).isBounded
    }

    @Test
    def testSetUnion: Unit = {
        assertEx(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].union(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertEx(new SingletonIntegerSetDomain(EmptyIntegerRange).union(CompleteIntegerSetDomain))
        assertEx(new IntegerPowersetDomain(NonPositiveIntegerRange).asInstanceOf[IntegerSetDomain].union(new IntegerPowersetDomain(NonNegativeIntegerRange)))
        assertEx(CompleteIntegerSetDomain.union(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testSetDifference: Unit = {
        assertEx(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].diff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertEx(new SingletonIntegerSetDomain(EmptyIntegerRange).diff(CompleteIntegerSetDomain))
        assertEx(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].diff(CompleteIntegerSetDomain))
        assertEx(CompleteIntegerSetDomain.diff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testSymmetricalSetDifference: Unit = {
        assertEx(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].symdiff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertEx(new SingletonIntegerSetDomain(EmptyIntegerRange).symdiff(CompleteIntegerSetDomain))
        assertEx(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].symdiff(CompleteIntegerSetDomain))
        assertEx(CompleteIntegerSetDomain.symdiff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

}
