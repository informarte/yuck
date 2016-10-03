package yuck

import yuck.core.DistributionFactory

package object annealing {

    val DEFAULT_PROBABILITY_OF_FAIR_CHOICE_IN_PERCENT = 3

    val DEFAULT_MOVE_SIZE_DISTRIBUTION = DistributionFactory.createDistribution(1, List(50, 35, 15))

}
