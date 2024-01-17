package yuck.core

import scala.collection.AbstractIterator

/**
 * Provides an interface for working with discrete distributions.
 *
 * All indices are 0-based.
 *
 * @author Michael Marte
 */
abstract class Distribution {

    /** The number of frequencies. */
    val size: Int

    /** Sets all frequencies to 0. */
    def clear(): Unit

    /** Sets the frequency of the given index to the given value. */
    def setFrequency(i: Int, f: Long): Unit

    /** Adds the given delta to the frequency of the given index. */
    def addFrequencyDelta(i: Int, delta: Long): Unit

    /** Returns the frequency associated with the given index. */
    def frequency(i: Int): Long

    /** Computes the probability associated with the given index. */
    final def probability(i: Int): Probability = Probability(frequency(i).toDouble / volume.toDouble)

    /** Returns the number of currently available alternatives. */
    def numberOfAlternatives: Int

    /** Returns the sum of all frequencies. */
    def volume: Long = cdf(size - 1)

    /** Implements the cumulative distribution function. */
    def cdf(i: Int): Long = {
        if (i < 0 || i > size) {
            throw new ArrayIndexOutOfBoundsException
        }
        var sum = 0L
        var j = 0
        while (j <= i) {sum = safeAdd(sum, frequency(j)); j += 1}
        sum
    }

    /**
     * Computes the smallest index i such that r < cdf(i).
     *
     * With this approach, indices with zero frequencies are ignored.
     */
    def inverseCdf(r: Long): Int = {
        require(r >= 0 && r < volume)
        var sum = 0L
        var i = -1
        while {i += 1; sum = safeAdd(sum, frequency(i)); r >= sum} do ()
        i
    }

    /** Randomly chooses an index according to the current distribution. */
    final def nextIndex(randomGenerator: RandomGenerator): Int =
        inverseCdf(randomGenerator.nextLong(volume))

    /**
     * Randomly chooses n indices according to the current distribution such that each index is chosen at most once.
     * To this end, it modifies the distribution, and it is the caller's duty to roll back the changes by closing the
     * given [[yuck.core.FrequencyRestorer]].
     */
    final def nextIndices(randomGenerator: RandomGenerator, n: Int, frequencyRestorer: FrequencyRestorer): Iterator[Int] =
        new AbstractIterator[Int] {
            var m = 0
            override def hasNext =  m < n && volume > 0
            override def next() = {
                require(hasNext)
                val i = nextIndex(randomGenerator)
                m += 1
                if (m < n) {
                    frequencyRestorer.memorize(Distribution.this, i)
                    setFrequency(i, 0)
                }
                i
            }
        }

    override def toString =
        "[%s]".format((0 until size).iterator.map(i => frequency(i)).mkString(", "))

}

/**
 * Provides methods for creating distributions.
 *
 * @author Michael Marte
 */
object Distribution {

    /**
     * Creates a distribution of the given size.
     *
     * For large distributions an implementation based on Fenwick trees will be provided,
     * for small distributions an array-based implementation will be used.
     */
    def apply(n: Int): Distribution = {
        if (n > 32) new FenwickTreeBackedDistribution(n)
        else new ArrayBackedDistribution(n)
    }

    /**
     * Creates a distribution from the given frequencies such that the first
     * frequency can be addressed with the given base index.
     */
    def apply(indexBase: Int, frequencies: Seq[Int]): Distribution = {
        val result = apply(indexBase + frequencies.size)
        for ((f, i) <- frequencies.view.zipWithIndex) {
            result.setFrequency(indexBase + i, f)
        }
        result
    }

}
