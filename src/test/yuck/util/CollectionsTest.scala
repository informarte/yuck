package yuck.util

import org.junit.*

import yuck.util.Collections.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CollectionsTest extends UnitTest {

    @Test
    def testRange2IntStream(): Unit = {

        assertEq(Range.inclusive(1, 0, 1).stream.toArraySeq, IntArraySeq())
        assertEq(Range.inclusive(0, 1, -1).stream.toArraySeq, IntArraySeq())
        assertEq(Range(0, 0, 1).stream.toArraySeq, IntArraySeq())

        assertEq(Range.inclusive(-1, 1, 1).stream.toArraySeq, IntArraySeq(-1, 0, 1))
        assertEq(Range(-1, 1, 1).stream.toArraySeq, IntArraySeq(-1, 0))

        assertEq(Range.inclusive(1, -1, -1).stream.toArraySeq, IntArraySeq(1, 0, -1))
        assertEq(Range(1, -1, -1).stream.toArraySeq, IntArraySeq(1, 0))

        assertEq(Range.inclusive(-1, 1, 2).stream.toArraySeq, IntArraySeq(-1, 1))
        assertEq(Range(-1, 1, 2).stream.toArraySeq, IntArraySeq(-1))

        assertEq(Range.inclusive(1, -1, -2).stream.toArraySeq, IntArraySeq(1, -1))
        assertEq(Range(1, -1, -2).stream.toArraySeq, IntArraySeq(1))

    }

    @Test
    def testRangeInlineForall(): Unit = {

        assertEq(Range.inclusive(1, 0, 1).inlineForall(_ => false), true)
        assertEq(Range.inclusive(0, 1, -1).inlineForall(_ => false), true)
        assertEq(Range(0, 0, 1).inlineForall(_ => false), true)

        assertEq(Range.inclusive(-1, 1, 1).inlineForall(_ <= 1), true)
        assertEq(Range.inclusive(-1, 1, 1).inlineForall(_ >= 0), false)
        assertEq(Range(-1, 1, 1).inlineForall(_ < 1), true)
        assertEq(Range(-1, 1, 1).inlineForall(_ >= 0), false)

        assertEq(Range.inclusive(1, -1, -1).inlineForall(_ <= 1), true)
        assertEq(Range.inclusive(1, -1, -1).inlineForall(_ >= 0), false)
        assertEq(Range(1, -1, -1).inlineForall(_ >= 0), true)
        assertEq(Range(1, -1, -1).inlineForall(_ < 0), false)

        assertEq(Range.inclusive(-1, 1, 2).inlineForall(_ != 0), true)
        assertEq(Range.inclusive(-1, 1, 2).inlineForall(_ == 0), false)
        assertEq(Range(-1, 1, 2).inlineForall(_ == -1), true)
        assertEq(Range(-1, 1, 2).inlineForall(_ != -1), false)

        assertEq(Range.inclusive(1, -1, -2).inlineForall(_ != 0), true)
        assertEq(Range.inclusive(1, -1, -2).inlineForall(_ == 0), false)
        assertEq(Range(1, -1, -2).inlineForall(_ == 1), true)
        assertEq(Range(1, -1, -2).inlineForall(_ != 1), false)

    }

    @Test
    def testRangeInlineExists(): Unit = {

        assertEq(Range.inclusive(1, 0, 1).inlineExists(_ => true), false)
        assertEq(Range.inclusive(0, 1, -1).inlineExists(_ => true), false)
        assertEq(Range(0, 0, 1).inlineExists(_ => true), false)

        assertEq(Range.inclusive(-1, 1, 1).inlineExists(_ == 1), true)
        assertEq(Range.inclusive(-1, 1, 1).inlineExists(_ > 1), false)
        assertEq(Range(-1, 1, 1).inlineExists(_ == 0), true)
        assertEq(Range(-1, 1, 1).inlineExists(_ > 0), false)

        assertEq(Range.inclusive(1, -1, -1).inlineExists(_ == -1), true)
        assertEq(Range.inclusive(1, -1, -1).inlineExists(_ < -1), false)
        assertEq(Range(1, -1, -1).inlineExists(_ == 0), true)
        assertEq(Range(1, -1, -1).inlineExists(_ < 0), false)

        assertEq(Range.inclusive(-1, 1, 2).inlineExists(_ == 1), true)
        assertEq(Range.inclusive(-1, 1, 2).inlineExists(_ == 0), false)
        assertEq(Range(-1, 1, 2).inlineExists(_ == -1), true)
        assertEq(Range(-1, 1, 2).inlineExists(_ != -1), false)

        assertEq(Range.inclusive(1, -1, -2).inlineExists(_ == -1), true)
        assertEq(Range.inclusive(1, -1, -2).inlineExists(_ == 0), false)
        assertEq(Range(1, -1, -2).inlineExists(_ == 1), true)
        assertEq(Range(1, -1, -2).inlineExists(_ != 1), false)

    }

}
