package yuck.flatzinc.test.util

import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.test.util.{DefaultNumberOfThreads, DefaultRuntimeLimitInSeconds}
import yuck.util.logging.LogLevel

/**
 * @author Michael Marte
 *
 */
enum SourceFormat {
    case FlatZinc
    case MiniZinc
}

/**
 * @author Michael Marte
 *
 */
enum VerificationFrequency {
    case NoVerification
    case VerifyOnlyLastSolution
    case VerifyEverySolution
}

/**
 * @author Michael Marte
 *
 */
enum TestDataDirectoryLayout {
    // all model (mzn) files in one folder, models contain data
    case MiniZincExamplesLayout
    // several model (mzn) and data (dzn) files in one folder, data files may be organized into sub folders
    case StandardMiniZincBenchmarksLayout
    // several model (mzn) files in one folder, models contain data
    case NonStandardMiniZincBenchmarksLayout
}

/**
 * @author Michael Marte
 *
 */
enum VerificationTool {
    case Chuffed
    case Gecode
    case OrTools
}

/**
 * @author Michael Marte
 *
 */
final case class ZincTestTask(
    sourceFormat: SourceFormat = SourceFormat.MiniZinc,
    directoryLayout: TestDataDirectoryLayout,
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
    maybeOptimum: Option[Long] = None, // overrules solverConfiguration.maybeTargetObjectiveValue
    maybeHighScore: Option[Long] = None, // best ever recorded objective value
    maybeTargetObjectiveValue: Option[Long] = None, // overrules solverConfiguration.maybeTargetObjectiveValue
    logLevel: LogLevel = LogLevel.InfoLogLevel,
    throwWhenUnsolved: Boolean = false,
    reusePreviousTestResult: Boolean = true,
    verificationFrequency: VerificationFrequency = VerificationFrequency.VerifyOnlyLastSolution,
    verificationModelName: String = "",
    verificationTool: VerificationTool = VerificationTool.Gecode,
    miniZincCompilerRenamesVariables: Boolean = true,
    keepFlatZincFile: Boolean = true,
    createDotFile: Boolean = false)
{
    require(sourceFormat != SourceFormat.FlatZinc || directoryLayout == TestDataDirectoryLayout.MiniZincExamplesLayout)
    def effectiveInstanceName: String = if (instanceName.isEmpty) problemName else instanceName
    override def toString = "%s:%s:%s".format(problemName, modelName, instanceName)
}
