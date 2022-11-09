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
final class IntegerDomainTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = IntegerRange(IntegerValue(-5), Five)

    @Test
    def testEquality(): Unit = {
        val sampleSize = 16
        val testData = helper.createTestData(baseRange, sampleSize).distinct
        helper.testEquality(testData)
        for (d <- testData) {
            for (e <- testData) {
                assert(if (d.eq(e)) d == e else d != e)
            }
        }
        for (d <- testData if d.isInstanceOf[IntegerRange]) {
            val e = IntegerDomain.ensureRangeList(d)
            assertEq(d, e)
            assertEq(e, d)
        }
    }

    @Test
    def testOrdering(): Unit = {
        val sampleSize = 16
        val testData = helper.createTestData(baseRange, sampleSize)
        helper.testOrdering(testData)
    }

    @Test
    def testOperations(): Unit = {
        val sampleSize = 8
        val testData = helper.createTestData(baseRange, sampleSize)
        helper.testBinaryOperations(testData)
    }

    @Test
    def testConstructionFromRanges(): Unit = {
        assert(IntegerDomain(List[IntegerRange]()).isInstanceOf[IntegerRange])
        assert(IntegerDomain(List[IntegerRange]()).isEmpty)
        assert(IntegerDomain(List(CompleteIntegerRange)).isInstanceOf[IntegerRange])
        assert(IntegerDomain(List(CompleteIntegerRange)).isComplete)
        assertEq(
            IntegerDomain(List(NegativeIntegerRange, PositiveIntegerRange)),
            IntegerRangeList(Vector(NegativeIntegerRange, PositiveIntegerRange)))
        assertEq(
            IntegerDomain(NegativeIntegerRange, PositiveIntegerRange),
            IntegerRangeList(Vector(NegativeIntegerRange, PositiveIntegerRange)))
    }

    @Test
    def testConstructionFromValueSets(): Unit = {
        val testData = List(
            List(List()) -> EmptyIntegerRange,
            List(List(Zero), List(Zero, Zero)) -> ZeroToZeroIntegerRange,
            List(List(Zero, One), List(One, Zero)) -> ZeroToOneIntegerRange,
            List(List(Zero, One, Three, Four), List(Three, Zero, Four, One, One)) ->
                IntegerRangeList(Vector(ZeroToOneIntegerRange, IntegerRange(Three, Four))))
        for ((inputs, expectation) <- testData) {
            for (input <- inputs) {
                def check(result: IntegerDomain) = {
                    assertEq(result, expectation)
                    assertEq(result.getClass, expectation.getClass)
                }
                def check1a(values: Iterable[IntegerValue]) = check(IntegerDomain(values))
                def check1b(values: IntegerValue*) = check(IntegerDomain(values: _*))
                def check2a(values: Iterable[Long]) = check(IntegerDomain(values))
                def check2b(values: Long*) = check(IntegerDomain(values: _*))
                check1a(input.toList)
                check1a(input.toVector)
                check1a(input.toSet)
                check1a(immutable.TreeSet[IntegerValue]() ++ input)
                check1b(input.toList: _*)
                check1b(input.toVector: _*)
                val intView = input.view.map(_.value)
                check2a(intView)
                check2a(intView.toList)
                check2a(intView.toVector)
                check2a(intView.toSet)
                check2a(immutable.TreeSet[Long]() ++ intView)
                check2b(intView.toList: _*)
                check2b(intView.toVector: _*)
            }
        }
    }

    @Test
    def testConfiguration(): Unit = {
        import IntegerDomain.{given}
        assertEq(ordering, IntegerDomainOrdering)
    }

}
