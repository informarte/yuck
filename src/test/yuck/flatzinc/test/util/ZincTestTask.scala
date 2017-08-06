package yuck.flatzinc.test.util

import yuck.core.Result
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.test.util.{DefaultNumberOfThreads, DefaultRuntimeLimitInSeconds}

/**
 * @author Michael Marte
 *
 */
sealed abstract class SourceFormat
case object FlatZinc extends SourceFormat
case object MiniZinc extends SourceFormat

/**
 * @author Michael Marte
 *
 */
sealed abstract class VerificationFrequency
case object NoVerification extends VerificationFrequency
case object VerifyOnlyLastSolution extends VerificationFrequency
case object VerifyEverySolution extends VerificationFrequency

/**
 * @author Michael Marte
 *
 */
sealed abstract class TestDataDirectoryLayout
// all model (mzn) files in one folder, models contain data
case object MiniZincExamplesLayout extends TestDataDirectoryLayout
// several model (mzn) and data (dzn) files in one folder, data files may be organized into sub folders
case object StandardMiniZincBenchmarksLayout extends TestDataDirectoryLayout
// several model (mzn) files in one folder, models contain data
case object NonStandardMiniZincBenchmarksLayout extends TestDataDirectoryLayout

/**
 * @author Michael Marte
 *
 */
sealed abstract class VerificationTool
case object Gecode extends VerificationTool
case object Chuffed extends VerificationTool

/**
 * @author Michael Marte
 *
 */
final case class ZincTestTask(
    sourceFormat: SourceFormat = MiniZinc,
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
    logLevel: yuck.util.logging.LogLevel = yuck.util.logging.InfoLogLevel,
    throwWhenUnsolved: Boolean = false,
    reusePreviousTestResult: Boolean = true,
    verificationFrequency: VerificationFrequency = VerifyOnlyLastSolution,
    verificationModelName: String = "",
    verificationTool: VerificationTool = Gecode,
    keepFlatZincFile: Boolean = true,
    createDotFile: Boolean = false)
{
    require(sourceFormat != FlatZinc || directoryLayout == MiniZincExamplesLayout)
    def effectiveInstanceName: String = if (instanceName.isEmpty) problemName else instanceName
    override def toString = "%s/%s/%s".format(problemName, modelName, instanceName)
}
