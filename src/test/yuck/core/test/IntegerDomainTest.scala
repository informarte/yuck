package yuck.core.test

import org.junit.*

import scala.collection.*

import yuck.core.*
import yuck.test.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerDomainTest extends UnitTest {

    private val BaseRange = IntegerRange(-5, 5)

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)

    @Test
    def testEquality(): Unit = {
        val testData = helper.createTestData(BaseRange, 16).distinct
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
        val testData = helper.createTestData(BaseRange, 16)
        helper.testOrdering(testData)
    }

    @Test
    def testOperations(): Unit = {
        val testData = helper.createTestData(BaseRange, 8)
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
            List(List(Zero), List(Zero, Zero)) -> IntegerRange(0, 0),
            List(List(Zero, One), List(One, Zero)) -> IntegerRange(0, 1),
            List(List(Zero, One, Three, Four), List(Three, Zero, Four, One, One)) ->
                IntegerRangeList(Vector(IntegerRange(0, 1), IntegerRange(3, 4))))
        for ((inputs, expectation) <- testData) {
            for (input <- inputs) {
                def check(result: IntegerDomain) = {
                    assertEq(result, expectation)
                    assertEq(result.getClass, expectation.getClass)
                }
                def check1a(values: Iterable[IntegerValue]) = check(IntegerDomain(values))
                def check1b(values: IntegerValue*) = check(IntegerDomain(values*))
                def check2a(values: Iterable[Long]) = check(IntegerDomain(values))
                def check2b(values: Long*) = check(IntegerDomain(values*))
                check1a(input.toList)
                check1a(input.toVector)
                check1a(input.to(HashSet))
                check1a(input.to(TreeSet))
                check1b(input.toList*)
                check1b(input.toVector*)
                val intView = input.view.map(_.value)
                check2a(intView)
                check2a(intView.toList)
                check2a(intView.toVector)
                check2a(intView.to(HashSet))
                check2a(intView.to(TreeSet))
                check2b(intView.toList*)
                check2b(intView.toVector*)
            }
        }
    }

    @Test
    def testConfiguration(): Unit = {
        import IntegerDomain.{given}
        assertEq(ordering, IntegerDomainOrdering)
    }

}
