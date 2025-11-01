package yuck.core

/**
 * @author Michael Marte
 *
 */
trait IteratedLocalSearchMonitoring[Result <: LocalSearchResult] extends LocalSearchMonitoring[Result] {
    def onNextRound(result: Result): Unit = {}
}

/**
 * @author Michael Marte
 *
 */
class IteratedLocalSearchMonitor extends IteratedLocalSearchMonitoring[LocalSearchResult]
