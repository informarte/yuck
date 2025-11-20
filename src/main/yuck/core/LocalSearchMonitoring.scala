package yuck.core

trait LocalSearchMonitoring[Result <: LocalSearchResult] extends SolverMonitoring[Result] {
    def onObjectiveTightened(result: Result, x: AnyVariable): Unit = {}
}

class LocalSearchMonitor extends LocalSearchMonitoring[LocalSearchResult]
