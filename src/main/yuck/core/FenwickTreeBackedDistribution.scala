package yuck.core

/**
 * Implements discrete distributions based on Fenwick trees.
 *
 * @see [[yuck.core.FenwickTree]]
 *
 * @author Michael Marte
 */
final class FenwickTreeBackedDistribution(override val size: Int) extends Distribution {
    require(size > 0)
    private val ft = new FenwickTree(size)
    private var frequencySum = 0L
    private var numberOfNonZeroFrequencies = 0
    override def clear() = {
        ft.clear()
        frequencySum = 0
        numberOfNonZeroFrequencies = 0
    }
    private def setFrequency(i: Int, f0: Long, f: Long): Unit = {
        require(f >= 0)
        val delta = f - f0
        ft.addDelta(i + 1, delta)
        frequencySum = safeAdd(frequencySum, delta)
        if (f0 == 0 && f > 0) numberOfNonZeroFrequencies += 1
        else if (f0 > 0 && f == 0) numberOfNonZeroFrequencies -= 1
    }
    override def setFrequency(i: Int, f: Long) = {
        setFrequency(i, frequency(i), f)
    }
    override def addFrequencyDelta(i: Int, delta: Long) = {
        val f0 = frequency(i)
        setFrequency(i, f0, safeAdd(f0, delta))
    }
    override def frequency(i: Int) = ft.value(i + 1)
    override def volume = frequencySum
    override def numberOfAlternatives = numberOfNonZeroFrequencies
    override def cdf(i: Int) = ft.prefixSum(i + 1)
    override def inverseCdf(r: Long) = {
        require(r >= 0 && r < volume)
        ft.index(r)
    }
}
