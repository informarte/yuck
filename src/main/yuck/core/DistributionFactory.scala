package yuck.core

/**
 * Provides methods for creating distributions.
 *
 * @author Michael Marte
 */
object DistributionFactory {

    /**
     * Creates a distribution of the given size.
     *
     * For large distributions an implementation based on Fenwick trees will be provided,
     * for small distributions an array-based implementation will be used.
     */
    def createDistribution(n: Int): Distribution = {
        if (n > 32) new FenwickTreeBackedDistribution(n)
        else new ArrayBackedDistribution(n)
    }

    /**
     * Creates a distribution from the given frequencies such that the first
     * frequency can be addressed with the given base index.
     */
    def createDistribution(indexBase: Int, frequencies: Seq[Int]): Distribution = {
        val d = createDistribution(indexBase + frequencies.size)
        var i = indexBase
        for (f <- frequencies) {
            d.setFrequency(i, f)
            i += 1
        }
        d
    }

}
