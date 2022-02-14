package yuck.flatzinc.test.util

import yuck.core.Result
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.test.util.{DefaultNumberOfThreads, DefaultRuntimeLimitInSeconds}

/**
 * @author Michael Marte
 *
 */
sealed abstract class MiniZincDirectoryLayout
// all model (mzn) files in one folder, models contain data
case object MiniZincExamplesLayout extends MiniZincDirectoryLayout {}
// several model (mzn) and data (dzn) files in one folder, data files may be organized into sub folders
case object StandardMiniZincBenchmarksLayout extends MiniZincDirectoryLayout {}
// several model (mzn) files in one folder, models contain data
case object NonStandardMiniZincBenchmarksLayout extends MiniZincDirectoryLayout {}

/**
 * @author Michael Marte
 *
 */
sealed abstract class VerificationTool
case object Gecode extends VerificationTool {}
case object Chuffed extends VerificationTool {}

/**
 * @author Michael Marte
 *
 */
case class MiniZincTestTask(
    directoryLayout: MiniZincDirectoryLayout,
    suitePath: String = "",
    suiteName: String = "",
    problemName: String = "",
    modelName: String = "",
    instanceName: String = "",
    dataAssignments: Map[String, String] = Map[String, String](),
    solverConfiguration: FlatZincSolverConfiguration =
        FlatZincSolverConfiguration(checkAssignmentsToNonChannelVariables = true),
    maybeRestartLimit: Option[Int] = None, // limits solverConfiguration.restartLimit
    maybeMaximumNumberOfThreads: Option[Int] = Some(DefaultNumberOfThreads), // limits solverConfiguration.numberOfThreads
    maybeRoundLimit: Option[Int] = None, // overrules solverConfiguration.maybeRoundLimitInSeconds
    maybeRuntimeLimitInSeconds: Option[Int] = Some(DefaultRuntimeLimitInSeconds), // overrules solverConfiguration.maybeRuntimeLimitInSeconds
    maybeOptimum: Option[Int] = None, // overrules solverConfiguration.maybeTargetObjectiveValue
    maybeHighScore: Option[Int] = None, // best ever recorded objective value
    maybeTargetObjectiveValue: Option[Int] = None, // overrules solverConfiguration.maybeTargetObjectiveValue
    logLevel: yuck.util.logging.LogLevel = yuck.util.logging.InfoLogLevel,
    assertWhenUnsolved: Boolean = false,
    reusePreviousTestResult: Boolean = true,
    verifySolution: Boolean = true,
    verificationModelName: String = "",
    verificationTool: VerificationTool = Gecode,
    keepFlatZincFile: Boolean = true,
    createDotFile: Boolean = false)
{
    def effectiveInstanceName: String = if (instanceName.isEmpty) problemName else instanceName
    override def toString = "%s/%s/%s".format(problemName, modelName, instanceName)
}
