package yuck.core

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
    def clear: Unit

    /** Sets the frequency of the given index to the given value. */
    def setFrequency(i: Int, f: Long): Unit

    /** Adds the given delta to the frequency of the given index. */
    def addFrequencyDelta(i: Int, delta: Long): Unit

    /** Returns the frequency associated with the given index. */
    def frequency(i: Int): Long

    /** Computes the probability associated with the given index. */
    final def probability(i: Int): Probability = Probability.from(frequency(i).toDouble / volume.toDouble)

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
        do {i += 1; sum = safeAdd(sum, frequency(i))} while (r >= sum)
        i
    }

    /** Randomly chooses an index according to the current distribution. */
    def nextIndex(randomGenerator: RandomGenerator): Int =
        inverseCdf(randomGenerator.nextLong(volume))

    override def toString =
        (0 until size).map(i => frequency(i)).toString

}
