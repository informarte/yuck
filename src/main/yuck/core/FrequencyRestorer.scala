package yuck.core

import scala.collection.*

import yuck.util.arm.ManagedResource

/**
 * Managed resource to memorize changes to distributions to be undone upon closing.
 *
 * @author Michael Marte
 */
final class FrequencyRestorer(capacity: Int) extends ManagedResource {

    private final val distributions = new mutable.ArrayBuffer[Distribution](capacity)
    private final val indices = new mutable.ArrayBuffer[Int](capacity)
    private final val frequencies = new mutable.ArrayBuffer[Long](capacity)

    override def open() = {}

    override def close() = {
        var i = 0
        val n = indices.size
        while (i < n) {
            distributions(i).setFrequency(indices(i), frequencies(i))
            i += 1
        }
        distributions.clear()
        indices.clear()
        frequencies.clear()
    }

    inline def memorize(distribution: Distribution, i: Int): Unit = {
        distributions += distribution
        indices += i
        frequencies += distribution.frequency(i)
    }

}
