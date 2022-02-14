package yuck.core.test

import org.junit.{FixMethodOrder, Test}

import yuck.core.FenwickTree
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
final class FenwickTreeTest extends UnitTest {

    @Test
    def testAdding: Unit = {
        val n = 256
        val ft = new FenwickTree(n)
        var sum = 0
        for (i <- 1 to n) {
            val delta = i % 8
            sum += delta
            ft.addDelta(i, delta)
            assertEq(delta, ft.value(i))
            assertEq(delta, ft.rangeSum(i, i))
            assertEq(sum, ft.rangeSum(1, i))
            assertEq(sum, ft.prefixSum(i))
            assertEq(sum, ft.rangeSum(1, n))
            assertEq(delta, ft.rangeSum(i, n))
        }
        for (i <- 1 to n) {
            val delta = i % 8
            sum -= delta
            ft.addDelta(i, -delta)
            assertEq(0, ft.value(i))
            assertEq(0, ft.rangeSum(i, i))
            assertEq(0, ft.rangeSum(1, i))
            assertEq(sum, ft.prefixSum(n))
            assertEq(sum, ft.rangeSum(1, n))
            assertEq(sum, ft.rangeSum(i, n))
        }
    }

    @Test
    def testIndexFinding: Unit = {

        val ft = new FenwickTree(6)
        ft.addDelta(2, 2)
        ft.addDelta(3, 4)
        ft.addDelta(4, 1)
        ft.addDelta(6, 3)

        /* The following facts show how to use the FT in optimization:
         * Given n = 5 variables and given above distribution d with volume v = 10,
         * we can choose a variable i in [0, n[ as follows:
         * val i = ft(d).find(randomGenerator.nextInt(v))
         * Next is how to update the frequency of variable i:
         * ft.set(i + 1, f')
         */
        assertEq(1, ft.index(0))
        assertEq(1, ft.index(1))
        assertEq(2, ft.index(2))
        assertEq(2, ft.index(3))
        assertEq(2, ft.index(4))
        assertEq(2, ft.index(5))
        assertEq(3, ft.index(6))
        assertEq(5, ft.index(7))
        assertEq(5, ft.index(8))
        assertEq(5, ft.index(9))
        assertEq(6, ft.index(10))

    }

    @Test
    def testScaling: Unit = {
        val ft = new FenwickTree(4)

        ft.addDelta(1, 1)
        ft.addDelta(2, 2)
        ft.addDelta(3, 4)
        ft.addDelta(4, 8)

        // before rescaling
        assertEq(1, ft.value(1))
        assertEq(2, ft.value(2))
        assertEq(4, ft.value(3))
        assertEq(8, ft.value(4))
        assertEq(1 + 2 + 4 + 8, ft.prefixSum(4))

        // rescale
        ft.scale(2)

        // check after rescaling
        assertEq(1, ft.value(1))
        assertEq(1, ft.value(2))
        assertEq(2, ft.value(3))
        assertEq(4, ft.value(4))
        assertEq(1 + 1 + 2 + 4, ft.prefixSum(4))

    }

}
