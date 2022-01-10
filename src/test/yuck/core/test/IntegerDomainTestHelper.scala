package yuck.core.test

import scala.collection._

import yuck.core.IntegerDomain.ensureRangeList
import yuck.core._
import yuck.util.logging.{FineLogLevel, LazyLogger}

/**
 * @author Michael Marte
 *
 */
final class IntegerDomainTestHelper
    (randomGenerator: RandomGenerator, logger: LazyLogger)
    extends OrderedDomainTestHelper[IntegerValue](logger, randomGenerator)
{

    val specialInfiniteRanges = IntegerDomainTestHelper.specialInfiniteRanges

    def createTestData(baseRange: IntegerRange, sampleSize: Int): Seq[IntegerDomain] =
        IntegerDomainTestHelper.createTestData(baseRange, sampleSize, randomGenerator)

    private def testEnsureRangeList(d: IntegerDomain): Unit = {
        val e = IntegerDomain.ensureRangeList(d)
        assert(e.isInstanceOf[IntegerRangeList])
        assertEq(e, d)
    }

    private def testSpatialRelations(d: IntegerDomain, e: IntegerDomain): Unit = {
        if (d.isEmpty || e.isEmpty) {
            assertEx(d.precedes(e))
            assertEx(d.precedesImmediately(e))
            assertEx(d.startsBefore(e))
            assertEx(d.startsAfter(e))
            assertEx(d.endsBefore(e))
            assertEx(d.endsAfter(e))
        } else {
            assertEq(d.precedes(e), d.hasUb && e.hasLb && d.ub < e.lb)
            assertEq(d.precedesImmediately(e), d.precedes(e) && d.ub + One == e.lb)
            assertEq(d.startsBefore(e), e.hasLb && (! d.hasLb || d.lb < e.lb))
            assertEq(d.startsAfter(e), d.hasLb && (! e.hasLb || d.lb > e.lb))
            assertEq(d.endsBefore(e), d.hasUb && (! e.hasUb || d.ub < e.ub))
            assertEq(d.endsAfter(e), e.hasUb && (! d.hasUb || d.ub > e.ub))
        }
    }

    // Test strategy for set operations:
    // - Try to avoid cyclic test dependencies by using more primitive functions only.
    // - If a function cannot be fully verified, issue a warning.

    private def testSetSize(d: IntegerDomain): Unit = {
        if (d.isFinite) {
            assertEq(d.size, d.values.size)
        } else {
            assertEx(d.size)
        }
    }

    private def testSubsetRelation(d: IntegerDomain, e: IntegerDomain): Unit = {
        val result = d.isSubsetOf(e)
        if (d.isEmpty) {
            assert(result)
        } else if (e.isEmpty) {
            assert(! result)
        } else if (d == e) {
            assert(result)
        } else if (d.isFinite && e.isFinite) {
            assertEq(result, d.values.toSet.subsetOf(e.values.toSet))
        } else if (! d.isFinite && e.isFinite) {
            assert(! result)
        } else if (e.isComplete) {
            assert(result)
        } else if (d.isComplete) {
            assert(! result)
        } else if (d.startsBefore(e) || d.endsAfter(e)) {
            assert(! result)
        } else if (! d.hasGaps && ! e.hasGaps && ! d.startsBefore(e) && ! d.endsAfter(e)) {
            assert(result)
        } else {
            if (d.isFinite) {
                assertEq(result, d.values.forall(a => e.contains(a)))
                // testSetContainment relies on intersects and testSetIntersectionRelation relies on isSubsetOf.
            }
            assertEq(result, e.union(d) == e)
            logger.logg("Operation subset(%s, %s) not fully verified".format(d, e))
        }
    }

    private def testSetIntersectionRelation(d: IntegerDomain, e: IntegerDomain): Unit = {
        val result = d.intersects(e)
        if (d.isEmpty || e.isEmpty) {
            assert(! result)
        } else if (d.precedes(e) || e.precedes(d)) {
            assert(! result)
        } else if (d.isSubsetOf(e) || e.isSubsetOf(d)) {
            assert(result)
        } else if (d.isFinite && e.isFinite) {
            assertEq(result, d.values.toSet.intersect(e.values.toSet).nonEmpty)
        } else if (d.isFinite) {
            assertEq(result, d.values.exists(a => e.contains(a)))
        } else if (e.isFinite) {
            assertEq(result, e.values.exists(a => d.contains(a)))
        } else if (d.isComplete || e.isComplete) {
            assert(result)
        } else if (
            Option(d.lb) == Option(e.lb) || Option(d.lb) == Option(e.ub) ||
            Option(d.ub) == Option(e.lb) || Option(d.ub) == Option(e.ub))
        {
            assert(result)
        } else {
            logger.logg("Operation intersects(%s, %s) not verified".format(d, e))
        }
    }

    private def testSetIntersection(d: IntegerDomain, e: IntegerDomain): Unit = {
        val result = d.intersect(e)
        val maybeResultSize = d.maybeIntersectionSize(e)
        assert(result.isSubsetOf(d))
        assert(result.isSubsetOf(e))
        assertEq(result.isFinite, maybeResultSize.isDefined)
        if (result.isFinite) {
            assertEq(result.size, maybeResultSize.get)
        }
        assertEq(! result.isEmpty, d.intersects(e))
        if (! result.isEmpty) {
            if (d.isSubsetOf(e)) {
                assertEq(result, d)
            } else if (e.isSubsetOf(d)) {
                assertEq(result, e)
            } else if (d.isFinite && e.isFinite) {
                assertEq(result.values.toSet, d.values.toSet.intersect(e.values.toSet))
            } else if (d.isFinite) {
                assertEq(result.values.toSet, d.values.filter(a => e.contains(a)).toSet)
            } else if (e.isFinite) {
                assertEq(result.values.toSet, e.values.filter(a => d.contains(a)).toSet)
            } else if (d.isComplete) {
                assertEq(result, e)
            } else if (e.isComplete) {
                assertEq(result, d)
            } else if (d.hasUb && Option(d.ub) == Option(e.lb)) {
                assert(result.isSingleton)
                assertEq(result.singleValue, d.ub)
            } else if (e.hasUb && Option(e.ub) == Option(d.lb)) {
                assert(result.isSingleton)
                assertEq(result.singleValue, e.ub)
            } else {
                assert(! d.diff(result).intersects(e.diff(result)))
                logger.logg("Operation intersect(%s, %s) not fully verified".format(d, e))
            }
        }
    }

    private def testSetUnion(d: IntegerDomain, e: IntegerDomain): Unit = {
        val result = d.union(e)
        assert(d.isSubsetOf(result))
        assert(e.isSubsetOf(result))
        if (d.isSubsetOf(e)) {
            assertEq(result, e)
        } else if (e.isSubsetOf(d)) {
            assertEq(result, d)
        } else if (d.isFinite && e.isFinite) {
            assertEq(d.union(e).values.toSet, d.values.toSet ++ e.values.toSet)
        } else if (! d.hasGaps && ! e.hasGaps) {
            if (d.intersects(e)) {
                assert(! result.hasGaps)
                if (d.startsBefore(e)) {
                    assertEq(result.lb, d.lb)
                    assertEq(result.ub, e.ub)
                } else {
                    assertEq(result.lb, e.lb)
                    assertEq(result.ub, d.ub)
                }
            } else if (d.precedesImmediately(e)) {
                assert(! result.hasGaps)
                assertEq(result.lb, d.lb)
                assertEq(result.ub, e.ub)
            } else if (d.precedes(e)) {
                assert(result.intersect(IntegerRange(d.ub + One, e.lb - One)).isEmpty)
            } else if (e.precedesImmediately(d)) {
                assert(! result.hasGaps)
                assertEq(result.lb, e.lb)
                assertEq(result.ub, d.ub)
            } else if (e.precedes(d)) {
                assert(result.intersect(IntegerRange(e.ub + One, d.lb - One)).isEmpty)
            } else {
                assert(false)
            }
        } else {
            assert(result.diff(d).isSubsetOf(e))
            assert(result.diff(e).isSubsetOf(d))
            logger.logg("Operation union(%s, %s) not fully verified".format(d, e))
        }
    }

    private val specialDifferenceCases =
        new immutable.HashMap[(IntegerDomain, IntegerDomain), IntegerDomain] ++
        List((CompleteIntegerRange, NegativeIntegerRange) -> NonNegativeIntegerRange,
             (CompleteIntegerRange, NonNegativeIntegerRange) -> NegativeIntegerRange,
             (CompleteIntegerRange, PositiveIntegerRange) -> NonPositiveIntegerRange,
             (CompleteIntegerRange, NonPositiveIntegerRange) -> PositiveIntegerRange,
             (NonNegativeIntegerRange, PositiveIntegerRange) -> IntegerRange(Zero, Zero),
             (NonPositiveIntegerRange, NegativeIntegerRange) -> IntegerRange(Zero, Zero),
             (NonNegativeIntegerRange, NonPositiveIntegerRange) -> PositiveIntegerRange,
             (NonPositiveIntegerRange, NonNegativeIntegerRange) -> NegativeIntegerRange)

    private def testSetDifference(d: IntegerDomain, e: IntegerDomain): Unit = {
        val result = d.diff(e)
        val maybeResultSize = d.maybeResidueSize(e)
        assert(result.isSubsetOf(d))
        assert(! result.intersects(e))
        assertEq(result.isFinite, maybeResultSize.isDefined)
        if (result.isFinite) {
            assertEq(result.size, maybeResultSize.get)
        }
        val maybeExpectedResult = specialDifferenceCases.get((d, e))
        if (maybeExpectedResult.isDefined) {
            assertEq(result, maybeExpectedResult.get)
        } else if (! d.intersects(e)) {
            assertEq(result, d)
        } else if (d.isSubsetOf(e)) {
            assert(result.isEmpty)
        } else if (d.isFinite) {
            assertEq(result.values.toSet, d.values.filter(a => ! e.contains(a)).toSet)
        } else if (d.isComplete && ! e.hasGaps) {
            if (! e.hasLb) {
                assertEq(result.lb, e.ub + One)
                assertEq(result.ub, d.ub)
            } else if (! e.hasUb) {
                assertEq(result.lb, d.lb)
                assertEq(result.ub, e.lb - One)
            } else {
                assertEq(result, IntegerRangeList(Vector(IntegerRange(d.lb, e.lb - One), IntegerRange(e.ub + One, d.ub))))
            }
        } else {
            assertEq(result.union(d.intersect(e)), d)
            logger.logg("Operation diff(%s, %s) not fully verified".format(d, e))
        }
    }

    private def testSymmetricalSetDifference(d: IntegerDomain, e: IntegerDomain): Unit = {
        assertEq(d.symdiff(e), d.union(e).diff(d.intersect(e)))
    }

    private def testSetContainment(d: IntegerDomain, a: IntegerValue): Unit = {
        val result = d.contains(a)
        if (d.isFinite) {
            assertEq(result, d.values.exists(_ == a))
            assertEq(result, d.valuesIterator.contains(a))
        } else {
            assertEq(result, d.intersects(IntegerRange(a, a)))
        }
    }

    private def testDistanceToSet(d: IntegerDomain, a: IntegerValue): Unit = {
        if (d.isEmpty) {
            assertEx(d.distanceTo(a))
        } else {
            val result = d.distanceTo(a)
            if (d.contains(a)) {
                assertEq(result, Zero)
            } else if (d.isFinite) {
                assertEq(result, d.valuesIterator.map(b => (a - b).abs).min)
            } else if (d.hasLb && a < d.lb) {
                assertEq(result, d.lb - a)
            } else if (d.hasUb && a > d.ub) {
                assertEq(result, a - d.ub)
            } else {
                assert(d.contains(a + result) || d.contains(a - result))
                assert(! d.intersects(IntegerRange(a - result + One, a + result - One)))
            }
        }
    }

    private def testBounding(d: IntegerDomain, a: IntegerValue): Unit = {
        val d1 = d.boundFromBelow(a)
        val d2 = d.boundFromAbove(a)
        if (! d1.isEmpty) {
            assertGe(d1.lb, a)
        }
        if (! d2.isEmpty) {
            assertLe(d2.ub, a)
        }
        if (d.contains(a)) {
            assertEq(d1.union(IntegerRange(a, a)).union(d2), d)
        } else if (d.maybeLb.isDefined && a < d.lb) {
            assertEq(d1, d)
            assert(d2.isEmpty)
        } else if (d.maybeUb.isDefined && a > d.ub) {
            assert(d1.isEmpty)
            assertEq(d2, d)
        }
    }

    private def testBisecting(d: IntegerDomain): Unit = {
        if (d.isEmpty || ! d.isFinite) {
            assertEx(d.bisect)
        } else {
            val (d1, d2) = d.bisect
            assert(d1.isSubsetOf(d))
            assert(d2.isSubsetOf(d))
            assert(! d1.intersects(d2))
            assertEq(d1.union(d2), d)
            if (d.size > 1) {
                assert(! d1.isEmpty)
                assert(! d2.isEmpty)
                assertLt(d1.ub, d2.lb)
            }
            if ((d.ub - d.lb + One).value == d.size) {
                assertLe(scala.math.abs(d1.size - d2.size), 1)
            }
        }
    }

    private def testMirroring(d: IntegerDomain): Unit = {
        val e = d.mirrored
        if (d.isFinite) {
            assert(e.isFinite)
            d.valuesIterator.forall(a => e.contains(a.negate))
            e.valuesIterator.forall(a => d.contains(a.negate))
        } else {
            d match {
                case _: IntegerRange =>
                    assertEq(e.lb, if (d.hasUb) d.ub.negate else null)
                    assertEq(e.ub, if (d.hasLb) d.lb.negate else null)
                case d: IntegerRangeList =>
                    assertEq(e, IntegerRangeList(d.ranges.reverseIterator.map(_.mirrored).toIndexedSeq))
            }
        }
    }

    def testRepresentation(createRange: (IntegerValue, IntegerValue) => IntegerDomain): Unit = {

        // {}
        val a = createRange(One, Zero)
        assertEq(a.toString, "{}")
        assert(a.isEmpty)
        assertEq(a.size, 0)
        assert(! a.isComplete)
        assert(a.isFinite)
        assert(! a.isSingleton)
        assert(! a.contains(Zero))
        assertEx(a.singleValue)
        assertEq(a.values.toList, Nil)
        assertEq(a.valuesIterator.toList, Nil)
        assert(a.isBounded)
        assert(a.hasLb)
        assert(a.hasUb)
        assert(a.maybeLb.isDefined)
        assert(a.maybeUb.isDefined)
        assertEq(a.maybeLb.get, a.lb)
        assertEq(a.maybeUb.get, a.ub)
        assertLt(a.ub, a.lb)
        assert(a.hull.isEmpty)
        assert(! a.hasGaps)

        // {0}
        val b = createRange(Zero, Zero)
        assertEq(b.toString, "{0}")
        assert(! b.isEmpty)
        assertEq(b.size, 1)
        assert(! b.isComplete)
        assert(b.isFinite)
        assert(b.isSingleton)
        assert(! b.contains(MinusOne))
        assert(b.contains(Zero))
        assert(! b.contains(One))
        assertEq(b.singleValue, Zero)
        assertEq(b.values.toList, List(Zero))
        assertEq(b.valuesIterator.toList, b.values.toList)
        assert(b.isBounded)
        assert(b.hasLb)
        assert(b.hasUb)
        assertEq(b.maybeLb.get, Zero)
        assertEq(b.maybeUb.get, Zero)
        assertEq(b.lb, Zero)
        assertEq(b.ub, Zero)
        assertEq(b.hull.lb, Zero)
        assertEq(b.hull.ub, Zero)
        assert(b.hull.isSingleton)
        assert(! b.hasGaps)

        // [0, 9]
        val c = createRange(Zero, Nine)
        assertEq(c.toString, "0..9")
        assert(! c.isEmpty)
        assertEq(c.size, 10)
        assert(! c.isComplete)
        assert(c.isFinite)
        assert(! c.isSingleton)
        assert(! c.contains(MinusOne))
        (0 to 9).foreach(i => assert(c.contains(IntegerValue(i))))
        assert(! c.contains(Ten))
        assertEx(c.singleValue)
        assertEq(c.values.size, 10)
        assertEq(c.values.toList, List(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine))
        assertEq(c.valuesIterator.toList, c.values.toList)
        assert(c.isBounded)
        assert(c.hasLb)
        assert(c.hasUb)
        assertEq(c.maybeLb.get, Zero)
        assertEq(c.maybeUb.get, Nine)
        assertEq(c.lb, Zero)
        assertEq(c.ub, Nine)
        assertEq(c.hull.lb, Zero)
        assertEq(c.hull.ub, Nine)
        assertEq(c.hull.size, 10)
        assert(! c.hasGaps)

        // ]-inf, +inf[
        val d = createRange(null, null)
        assertEq(d.toString, "-inf..+inf")
        assert(! d.isEmpty)
        assertEx(d.size)
        assert(d.isComplete)
        assert(! d.isFinite)
        assert(! d.isSingleton)
        assert(d.contains(Zero))
        assertEx(d.singleValue)
        assertEx(d.values)
        assertEx(d.valuesIterator)
        assert(! d.isBounded)
        assert(! d.hasLb)
        assert(! d.hasUb)
        assert(d.maybeLb.isEmpty)
        assert(d.maybeUb.isEmpty)
        assertEq(d.lb, null)
        assertEq(d.ub, null)
        assert(! d.hasGaps)

        // [-inf, 0[
        val e = createRange(null, Zero)
        assertEq(e.toString, "-inf..0")
        assert(! e.isEmpty)
        assertEx(e.size)
        assert(! e.isComplete)
        assert(! e.isFinite)
        assert(! e.isSingleton)
        assert(e.contains(MinusOne))
        assert(e.contains(Zero))
        assert(! e.contains(One))
        assertEx(e.singleValue)
        assertEx(e.values)
        assertEx(e.valuesIterator)
        assert(e.isBounded)
        assert(! e.hasLb)
        assert(e.hasUb)
        assert(e.maybeLb.isEmpty)
        assertEq(e.maybeUb.get, Zero)
        assertEq(e.lb, null)
        assertEq(e.ub, Zero)
        assert(! e.hasGaps)

        // [0, +inf[
        val f = createRange(Zero, null)
        assertEq(f.toString, "0..+inf")
        assert(! f.isEmpty)
        assertEx(f.size)
        assert(! f.isComplete)
        assert(! f.isFinite)
        assert(! f.isSingleton)
        assert(! f.contains(MinusOne))
        assert(f.contains(Zero))
        assert(f.contains(One))
        assertEx(f.singleValue)
        assertEx(f.values)
        assertEx(f.valuesIterator)
        assert(f.isBounded)
        assert(f.hasLb)
        assert(! f.hasUb)
        assertEq(f.maybeLb.get, Zero)
        assert(f.maybeUb.isEmpty)
        assertEq(f.lb, Zero)
        assertEq(f.ub, null)
        assert(! f.hasGaps)

    }

    def testOperations(dl: Seq[IntegerDomain], vl: Seq[IntegerValue]): Unit = {
        logger.withRootLogLevel(FineLogLevel) {
            logger.withLogScope("Test data") {
                dl.foreach(d => logger.log(d.toString))
            }
        }
        require(dl.size > 1)
        require(! vl.isEmpty)
        for (d <- dl) {
            testEnsureRangeList(d)
            testSetSize(d)
            for (e <- dl) {
                testSpatialRelations(d, e)
                testSubsetRelation(d, e)
                testSetIntersectionRelation(d, e)
                testSetIntersection(d, e)
                testSetUnion(d, e)
                testSetDifference(d, e)
                testSymmetricalSetDifference(d, e)
            }
            for (a <- vl) {
                testSetContainment(d, a)
                testDistanceToSet(d, a)
            }
            if (d.isEmpty) {
                assert(d.boundFromBelow(Zero).isEmpty)
                assert(d.boundFromAbove(Zero).isEmpty)
            } else {
                if (d.isFinite) {
                    testBounding(d, d.randomValue(randomGenerator))
                }
                if (d.hasLb) {
                    testBounding(d, d.lb)
                    testBounding(d, d.lb - One)
                }
                if (d.hasUb) {
                    testBounding(d, d.ub)
                    testBounding(d, d.ub + One)
                }
                if (d.isFinite) {
                    if (d.isSingleton) {
                        assertEq(d.randomValue(randomGenerator), d.singleValue)
                        assertEq(d.nextRandomValue(randomGenerator, Zero), d.singleValue)
                    } else {
                        testUniformityOfDistribution(randomGenerator, d)
                    }
                } else {
                    assertEx(d.randomValue(randomGenerator))
                    assertEx(d.nextRandomValue(randomGenerator, Zero))
                }
            }
            testBisecting(d)
            testMirroring(d)
        }
    }

    def testRandomSubrangeCreation(testData: Seq[IntegerDomain]): Unit = {
        logger.withRootLogLevel(FineLogLevel) {
            for (d <- testData) {
                logger.log(d.toString)
                if (d.isEmpty) {
                    assert(d.randomSubdomain(randomGenerator).isEmpty)
                } else if (d.isFinite) {
                    val sampleSize = ensureRangeList(d).ranges.iterator.map(r => r. size * (r.size + 1) / 2).sum
                    val sample = mutable.HashSet[IntegerRange]()
                    for (i <- 1 to sampleSize) {
                        val e = d.randomSubrange(randomGenerator)
                        assert(e.isInstanceOf[IntegerRange])
                        assert(e.isSubsetOf(d))
                        sample += e
                    }
                    assertGe(sample.size, sampleSize / 2)
                } else {
                    assertEx(d.randomSubdomain(randomGenerator))
                }
            }
        }
    }

    def testRandomSubdomainCreation(testData: Seq[IntegerDomain]): Unit = {
        logger.withRootLogLevel(FineLogLevel) {
            for (d <- testData) {
                logger.log(d.toString)
                if (d.isEmpty) {
                    assert(d.randomSubdomain(randomGenerator).isEmpty)
                } else if (d.isFinite) {
                    val sampleSize = 1 << d.size
                    val sample = new mutable.HashSet[IntegerDomain]
                    for (i <- 1 to sampleSize) {
                        val e = d.randomSubdomain(randomGenerator)
                        assert(e.isSubsetOf(d))
                        sample += e
                    }
                    assertGe(sample.size, sampleSize / 2)
                } else {
                    assertEx(d.randomSubdomain(randomGenerator))
                }
            }
        }
    }

}

/**
 * @author Michael Marte
 *
 */
object IntegerDomainTestHelper {

    val specialInfiniteRanges =
        List(CompleteIntegerRange,
            NegativeIntegerRange, NonNegativeIntegerRange,
            PositiveIntegerRange, NonPositiveIntegerRange)

    def createEdgeCases(baseRange: IntegerRange): Seq[IntegerRange] = {
        require(baseRange.isFinite)
        val singletonRanges = List(baseRange.lb, baseRange.ub).map(a => IntegerRange(a, a))
        List(EmptyIntegerRange, baseRange) ++ specialInfiniteRanges ++ singletonRanges
    }

    def createTestData(baseRange: IntegerRange, sampleSize: Int, randomGenerator: RandomGenerator): Seq[IntegerDomain] = {
        require(baseRange.isFinite)
        val randomFiniteRanges = for (i <- 1 to sampleSize) yield baseRange.randomSubrange(randomGenerator)
        val randomFiniteRangeLists = for (i <- 1 to sampleSize) yield baseRange.randomSubdomain(randomGenerator)
        val randomFiniteIntegerDomains = randomFiniteRanges ++ randomFiniteRangeLists
        val randomInfiniteRangeLists =
            for (infiniteRange <- specialInfiniteRanges;
                 finiteDomain <- randomFiniteIntegerDomains;
                 if infiniteRange.intersects(finiteDomain)) yield infiniteRange.diff(finiteDomain)
        val edgeCases = createEdgeCases(baseRange)
        val testData = (randomFiniteIntegerDomains ++ randomInfiniteRangeLists ++ edgeCases).distinct
        testData
    }

}
