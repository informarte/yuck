package yuck.core

/**
 * Implements discrete distributions based on arrays.
 *
 * @author Michael Marte
 */
final class ArrayBackedDistribution(override val size: Int) extends Distribution {
    require(size > 0)
    private val frequencies = new Array[Int](size)
    private var frequencySum = 0
    private var numberOfNonZeroFrequencies = 0
    override def clear {
        for (i <- 0 until size) {
            frequencies.update(i, 0)
        }
        frequencySum = 0
        numberOfNonZeroFrequencies = 0
    }
    override def setFrequency(i: Int, f: Int) {
        require(f >= 0)
        val f0 = frequencies(i)
        frequencySum -= f0
        frequencies.update(i, f)
        frequencySum += f
        assert(frequencySum >= 0, "Integer overflow")
        if (f0 == 0 && f > 0) numberOfNonZeroFrequencies += 1
        else if (f0 > 0 && f == 0) numberOfNonZeroFrequencies -= 1
    }
    override def addFrequencyDelta(i: Int, delta: Int) {
        setFrequency(i, frequencies(i) + delta)
    }
    override def frequency(i: Int) = frequencies(i)
    override def volume = frequencySum
    override def numberOfAlternatives = numberOfNonZeroFrequencies
}