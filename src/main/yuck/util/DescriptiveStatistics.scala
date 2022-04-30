package yuck.util

import scala.collection.*

/**
 * @author Michael Marte
 *
 */
object DescriptiveStatistics {

    extension(xs0: IndexedSeq[Double]) {

        def median: Double = {
            val xs = xs0.sorted
            val n = xs.size
            val m = n / 2
            if (n % 2 == 0) (xs(m) + xs(m - 1)) / 2.0
            else xs(m)
        }

    }

}
