package yuck

import yuck.core.Distribution

/**
 * @author Michael Marte
 *
 */
package object annealing {

    val DefaultMoveSizeDistribution = Distribution(1, List(90, 10))

    val DefaultStartTemperature = 0.05
    val DefaultWarmStartTemperature = 0.001
    val DefaultFinalTemperature = 0.0001
    val DefaultMaximumUphillAcceptanceRatio = 0.15
    val DefaultMinimumUphillAcceptanceRatio = 0.0001
    val DefaultHeatingRate = 1.2
    val DefaultCoolingRate = 0.95

}
