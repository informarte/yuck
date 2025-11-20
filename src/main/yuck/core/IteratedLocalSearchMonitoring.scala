package yuck.core

trait IteratedLocalSearchMonitoring[Result <: LocalSearchResult] extends LocalSearchMonitoring[Result] {
    def onNextRound(result: Result): Unit = {}
}

class IteratedLocalSearchMonitor extends IteratedLocalSearchMonitoring[LocalSearchResult]
