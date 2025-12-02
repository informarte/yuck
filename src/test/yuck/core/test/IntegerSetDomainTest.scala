package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetDomainTest extends UnitTest {

    // Test strategy:
    // * Test the type-based dispatching in IntegerSetDomain.
    // * Do not retest the IntegerSetDomain implementations.
    // * For each combination of IntegerSetDomain implementations, test the operation on two non-trivial examples
    //   with different results.
    // * Test that set operations return instances of OrderedDomain[IntegerSetValue].

    private val baseRange = IntegerRange(-5, 5)

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerSetDomainTestHelper(randomGenerator, logger)

    @Test
    def testEquality(): Unit = {
        assertEq(new SingletonIntegerSetDomain(EmptyIntegerRange), new IntegerPowerSetDomain(EmptyIntegerRange))
        val testData = helper.createTestData(baseRange, 16).distinct
        helper.testEquality(testData)
        for d <- testData do {
            for e <- testData do {
                assert(if d.eq(e) then d == e else d != e)
            }
        }
    }

    // The ordering is implemented in IntegerSetDomain and works for all its subclasses,
    // so we test the ordering only once and here.
    @Test
    def testOrdering(): Unit = {
        val testData = helper.createTestData(baseRange, 8)
        helper.testOrdering(testData)
    }

    @Test
    def testSubsetRelation(): Unit = {
        assert(EmptyIntegerSetDomain.isSubsetOf(EmptyIntegerSetDomain))
        assert(EmptyIntegerSetDomain.isSubsetOf(CompleteIntegerSetDomain))
        assert(! CompleteIntegerSetDomain.isSubsetOf(EmptyIntegerSetDomain))
        assert(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].isSubsetOf(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assert(! new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].isSubsetOf(new SingletonIntegerSetDomain(CompleteIntegerRange)))
        assert(new SingletonIntegerSetDomain(EmptyIntegerRange).isSubsetOf(CompleteIntegerSetDomain))
        assert(! new SingletonIntegerSetDomain(PositiveIntegerRange).isSubsetOf(new IntegerPowerSetDomain(NegativeIntegerRange)))
        assert(new IntegerPowerSetDomain(PositiveIntegerRange).asInstanceOf[IntegerSetDomain].isSubsetOf(new IntegerPowerSetDomain(NonNegativeIntegerRange)))
        assert(! new IntegerPowerSetDomain(NonNegativeIntegerRange).asInstanceOf[IntegerSetDomain].isSubsetOf(new IntegerPowerSetDomain(PositiveIntegerRange)))
        assert(new IntegerPowerSetDomain(EmptyIntegerRange).isSubsetOf(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assert(! CompleteIntegerSetDomain.isSubsetOf(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testSetIntersectionRelation(): Unit = {
        assert(! EmptyIntegerSetDomain.intersects(EmptyIntegerSetDomain))
        assert(! EmptyIntegerSetDomain.intersects(CompleteIntegerSetDomain))
        assert(! CompleteIntegerSetDomain.intersects(EmptyIntegerSetDomain))
        assert(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].intersects(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assert(! new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].intersects(new SingletonIntegerSetDomain(CompleteIntegerRange)))
        assert(new SingletonIntegerSetDomain(EmptyIntegerRange).intersects(CompleteIntegerSetDomain))
        assert(! new SingletonIntegerSetDomain(NonPositiveIntegerRange).intersects(new SingletonIntegerSetDomain(NonNegativeIntegerRange)))
        assert(new IntegerPowerSetDomain(NonPositiveIntegerRange).asInstanceOf[IntegerSetDomain].intersects(new IntegerPowerSetDomain(NegativeIntegerRange)))
        assert(! new IntegerPowerSetDomain(PositiveIntegerRange).asInstanceOf[IntegerSetDomain].intersects(new IntegerPowerSetDomain(NegativeIntegerRange)))
        assert(CompleteIntegerSetDomain.intersects(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assert(! new IntegerPowerSetDomain(NonPositiveIntegerRange).intersects(new SingletonIntegerSetDomain(NonNegativeIntegerRange)))
    }

    @Test
    def testSetIntersection(): Unit = {
        assertEq(EmptyIntegerSetDomain.intersect(EmptyIntegerSetDomain), EmptyIntegerSetDomain)
        assertEq(EmptyIntegerSetDomain.intersect(CompleteIntegerSetDomain), EmptyIntegerSetDomain)
        assertEq(CompleteIntegerSetDomain.intersect(EmptyIntegerSetDomain), EmptyIntegerSetDomain)
        assertEq(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].intersect(new SingletonIntegerSetDomain(EmptyIntegerRange)), new SingletonIntegerSetDomain(EmptyIntegerRange))
        assertEq(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].intersect(new SingletonIntegerSetDomain(IntegerRange(0, 1))), EmptyIntegerSetDomain)
        assertEq(new SingletonIntegerSetDomain(EmptyIntegerRange).intersect(CompleteIntegerSetDomain), new SingletonIntegerSetDomain(EmptyIntegerRange))
        assertEq(new SingletonIntegerSetDomain(NegativeIntegerRange).intersect(new IntegerPowerSetDomain(PositiveIntegerRange)), EmptyIntegerSetDomain)
        assertEq(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].intersect(new IntegerPowerSetDomain(EmptyIntegerRange)), new IntegerPowerSetDomain(EmptyIntegerRange))
        assertEq(new IntegerPowerSetDomain(NonPositiveIntegerRange).asInstanceOf[IntegerSetDomain].intersect(new IntegerPowerSetDomain(NonNegativeIntegerRange)), new IntegerPowerSetDomain(IntegerRange(0, 0)))
        assertEq(CompleteIntegerSetDomain.intersect(new SingletonIntegerSetDomain(EmptyIntegerRange)), new SingletonIntegerSetDomain(EmptyIntegerRange))
        assertEq(new IntegerPowerSetDomain(NegativeIntegerRange).intersect(new SingletonIntegerSetDomain(PositiveIntegerRange)), EmptyIntegerSetDomain)
        assertEq(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].intersect(CompleteIntegerSetDomain), CompleteIntegerSetDomain)
    }

    @Test
    def testSetUnion(): Unit = {
        assertEq(EmptyIntegerSetDomain.union(EmptyIntegerSetDomain), EmptyIntegerSetDomain)
        assertEq(EmptyIntegerSetDomain.union(CompleteIntegerSetDomain), CompleteIntegerSetDomain)
        assertEq(CompleteIntegerSetDomain.union(EmptyIntegerSetDomain), CompleteIntegerSetDomain)
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].union(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).union(CompleteIntegerSetDomain))
        assertNie(new IntegerPowerSetDomain(NonPositiveIntegerRange).asInstanceOf[IntegerSetDomain].union(new IntegerPowerSetDomain(NonNegativeIntegerRange)))
        assertNie(CompleteIntegerSetDomain.union(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testSetDifference(): Unit = {
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].diff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).diff(CompleteIntegerSetDomain))
        assertNie(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].diff(CompleteIntegerSetDomain))
        assertNie(CompleteIntegerSetDomain.diff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testSymmetricalSetDifference(): Unit = {
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).asInstanceOf[IntegerSetDomain].symdiff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
        assertNie(new SingletonIntegerSetDomain(EmptyIntegerRange).symdiff(CompleteIntegerSetDomain))
        assertNie(CompleteIntegerSetDomain.asInstanceOf[IntegerSetDomain].symdiff(CompleteIntegerSetDomain))
        assertNie(CompleteIntegerSetDomain.symdiff(new SingletonIntegerSetDomain(EmptyIntegerRange)))
    }

    @Test
    def testRandomSubdomainCreation(): Unit = {
        val testData = helper.createTestData(baseRange, 8)
        for a <- testData do {
            assertEx(a.randomSubdomain(randomGenerator), classOf[NotImplementedError])
        }
    }

    @Test
    def testConfiguration(): Unit = {
        import IntegerSetDomain.given
        def testOrdering()(using ordering: Ordering[OrderedDomain[IntegerSetValue]]) = {
            assertEq(ordering, IntegerSetDomainOrdering)
        }
        testOrdering()
    }

}
