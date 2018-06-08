package yuck

import yuck.core.DistributionFactory

/**
 * @author Michael Marte
 *
 */
package object annealing {

    val DEFAULT_MOVE_SIZE_DISTRIBUTION = DistributionFactory.createDistribution(1, List(90, 10))

}
