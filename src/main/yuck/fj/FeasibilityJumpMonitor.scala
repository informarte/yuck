package yuck.fj

import yuck.core.IteratedLocalSearchMonitoring

/**
 * @author Michael Marte
 *
 */
class FeasibilityJumpMonitor extends IteratedLocalSearchMonitoring[FeasibilityJumpResult] {
    def onPerturbation(result: FeasibilityJumpResult): Unit = {}
}
