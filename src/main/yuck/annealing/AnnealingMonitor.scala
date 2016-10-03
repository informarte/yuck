package yuck.annealing

import yuck.core.SolverMonitor

/**
 * @author Michael Marte
 *
 */
class AnnealingMonitor extends SolverMonitor[AnnealingResult] {
    def onNextRound(result: AnnealingResult) {}
    def onReheatingStarted(result: AnnealingResult) {}
    def onReheatingFinished(result: AnnealingResult) {}
}
