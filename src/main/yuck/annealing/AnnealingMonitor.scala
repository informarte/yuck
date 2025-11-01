package yuck.annealing

import yuck.core.IteratedLocalSearchMonitoring

/**
 * @author Michael Marte
 *
 */
class AnnealingMonitor extends IteratedLocalSearchMonitoring[AnnealingResult] {
    def onReheatingStarted(result: AnnealingResult): Unit = {}
    def onReheatingFinished(result: AnnealingResult): Unit = {}
    def onScheduleRestarted(result: AnnealingResult): Unit = {}
}
