package yuck.fj

import yuck.core.IteratedLocalSearchMonitoring

class FeasibilityJumpMonitor extends IteratedLocalSearchMonitoring[FeasibilityJumpResult] {
    def onPerturbation(result: FeasibilityJumpResult): Unit = {}
}
