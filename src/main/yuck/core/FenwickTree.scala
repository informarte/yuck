package yuck.core

/**
 * Implements the Fenwick tree data structure.
 *
 * For details see:
 *
 * Peter M. Fenwick (1994).
 * "A new data structure for cumulative frequency tables".
 * Software: Practice and Experience 24 (3): 327–336.
 *
 * or
 *
 * http://en.wikipedia.org/wiki/Fenwick_tree.
 *
 * Notice that all indices are 1-based (as in the paper).
 *
 * @author Michael Marte
 */
class FenwickTree(val size: Int) {

    private val tree = new Array[Int](size + 1)

    // Throws when the given index is out-of-range.
    private def checkIndex(i: Int) {
        require(i >= 1 && i <= size)
    }

    /** Clears the Fenwick tree. */
    def clear {
        for (i <- 1 to size)
            tree(i) = 0
    }

    /** Computes the prefix sum for the given index. */
    def prefixSum(__i: Int): Int = {
        var i = __i
        checkIndex(i)
        var result = 0
        while (i > 0) {
            result = safeAdd(result, tree(i))
            i -= (i & -i)
        }
        result
    }

    /** Computes the sum of values in the given range [left, right]. */
    def rangeSum(left: Int, right: Int): Int = {
        checkIndex(left)
        checkIndex(right)
        require(left <= right)
        prefixSum(right) - (if (left == 1) 0 else prefixSum(left - 1))
    }

    /** Updates the value at the given index by adding the given delta to it. */
    def addDelta(__i: Int, delta: Int) {
        var i = __i
        checkIndex(i)
        while (i <= size) {
            tree(i) = safeAdd(tree(i), delta)
            i += (i & -i)
        }
    }

    /** Computes the value at the given index. */
    def value(__i: Int): Int = {
        var i = __i
        checkIndex(i)
        var value = tree(i)
        var parent = 0
        if (i > 0) {
            parent = i & (i - 1)
            i -= 1
            while (parent != i) {
                value -= tree(i)
                i &= i - 1
            }
        }
        value
    }

    /**
     * Finds the largest index i in the Fenwick tree such that the prefix sum up
     * to i is less than or equal to the given sum.
     */
    def index(__sum: Int): Int = {
        var sum = __sum
        var mask = 1
        while (mask <= size) mask *= 2
        var i = 0
        while (mask != 0) {
            if (i + mask <= size) {
                val j = i + mask
                if (sum >= tree(j)) {
                    i = j
                    sum -= tree(i)
                }
            }
            mask /= 2
        }
        i
    }

    /** Divides all values by the given scaling factor. */
    def scale(f: Int) {
        for (i <- size until 0 by -1) {
            addDelta(i, -value(i) / f)
        }
    }

}
