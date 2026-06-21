package yuck.util

import scala.collection.*
import scala.math.{exp, log}

object DescriptiveStatistics {

    extension(xs: IndexedSeq[Double]) {

        def median: Double = {
            val xs1 = xs.sorted
            val n = xs1.size
            val m = n / 2
            if n % 2 == 0 then (xs1(m) + xs1(m - 1)) / 2.0 else xs1(m)
        }

        def geometricMean: Double =
            exp(xs.view.map(log).sum / xs.size)

    }

}
