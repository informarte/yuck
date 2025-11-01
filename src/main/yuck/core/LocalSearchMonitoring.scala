package yuck.core

/**
 * @author Michael Marte
 *
 */
trait LocalSearchMonitoring[Result <: LocalSearchResult] extends SolverMonitoring[Result] {
    def onObjectiveTightened(result: Result, x: AnyVariable): Unit = {}
}

/**
 * @author Michael Marte
 *
 */
class LocalSearchMonitor extends LocalSearchMonitoring[LocalSearchResult]
