package yuck.flatzinc.test.util

import yuck.flatzinc.FlatZincSolverConfiguration

/**
 * @author Michael Marte
 *
 */
object MiniZincChallengeRunner extends MiniZincTestSuite {

    def main(args: Array[String]) {
        val task =
            MiniZincTestTask(
                directoryLayout = StandardMiniZincBenchmarksLayout,
                suitePath = args(0),
                problemName = args(1),
                instanceName = args(2),
                solverConfiguration = FlatZincSolverConfiguration(stopOnFirstSolution = true))
        solve(task)
   }

}
