package yuck.core

/**
 * Helper class to memorize a change to a distribution and to undo the change at a later point.
 *
 * @author Michael Marte
 */
final class FrequencyRestorer {

    private var i = -1
    private var f = 0L

    inline def store(i: Int, f: Long): Unit = {
        this.i = i
        this.f = f
    }

    inline def restore(distribution: Distribution): Unit = {
        if (i > -1) {
            distribution.setFrequency(i, f)
            i = -1
        }
    }

}
