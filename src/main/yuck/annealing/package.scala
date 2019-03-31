package yuck

import yuck.core.DistributionFactory

/**
 * @author Michael Marte
 *
 */
package object annealing {

    val DefaultMoveSizeDistribution = DistributionFactory.createDistribution(1, List(90, 10))

}
