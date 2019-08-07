package yuck.annealing

import yuck.core.SolverMonitor

/**
 * @author Michael Marte
 *
 */
class AnnealingMonitor extends SolverMonitor[AnnealingResult] {
    def onNextRound(result: AnnealingResult): Unit = {}
    def onReheatingStarted(result: AnnealingResult): Unit = {}
    def onReheatingFinished(result: AnnealingResult): Unit = {}
}
