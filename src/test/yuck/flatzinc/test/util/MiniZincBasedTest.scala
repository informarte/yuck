package yuck.flatzinc.test.util

import scala.collection._

import org.junit._

import yuck.annealing._
import yuck.core._
import yuck.flatzinc.compiler.{FlatZincCompilerResult, InconsistentProblemException}
import yuck.flatzinc.parser._
import yuck.flatzinc.runner._
import yuck.util.arm.{ManagedShutdownHook, scoped, using}
import yuck.util.testing.{IntegrationTest, ProcessRunner}

/**
 * @author Michael Marte
 *
 */
class MiniZincBasedTest extends IntegrationTest {

    protected def solve(task: MiniZincTestTask): Result = {
        logger.setThresholdLogLevel(task.logLevel)
        try {
            trySolve(task)
        }
        catch {
            case error: Throwable => handleException(findUltimateCause(error))
        }
    }

    private def trySolve(task: MiniZincTestTask): Result = {
        val suitePath = task.suitePath
        val suiteName = if (task.suiteName.isEmpty) new java.io.File(suitePath).getName else task.suiteName
        val problemName = task.problemName
        val modelName = if (task.modelName.isEmpty) problemName else task.modelName
        val instanceName = task.instanceName
        val (mznFilePath, dznFilePath, outputDirectoryPath) = task.directoryLayout match {
            case MiniZincExamplesLayout =>
                ("%s/problems/%s.mzn".format(suitePath, problemName),
                 "",
                 "tmp/%s/%s".format(suiteName, problemName))
            case StandardMiniZincBenchmarksLayout =>
                ("%s/problems/%s/%s.mzn".format(suitePath, problemName, modelName),
                 "%s/problems/%s/%s.dzn".format(suitePath, problemName, instanceName),
                 "tmp/%s/%s/%s/%s".format(suiteName, problemName, modelName, instanceName))
            case NonStandardMiniZincBenchmarksLayout =>
                ("%s/problems/%s/%s.mzn".format(suitePath, problemName, instanceName),
                 "",
                 "tmp/%s/%s/%s".format(suiteName, problemName, instanceName))
        }
        new java.io.File(outputDirectoryPath).mkdirs
        val fznFilePath = "%s/problem.fzn".format(outputDirectoryPath)
        val logFilePath = "%s/yuck.log".format(outputDirectoryPath)
        val logFileHandler = new java.util.logging.FileHandler(logFilePath)
        logFileHandler.setFormatter(formatter)
        nativeLogger.addHandler(logFileHandler)
        logger.log("Processing %s".format(mznFilePath))
        logger.log("Logging into %s".format(logFilePath))
        new ProcessRunner(logger, List("mzn2fzn", "--version")).call
        val mzn2fznCommand = mutable.ArrayBuffer(
            "mzn2fzn",
            "-v",
            "-I", "resources/mzn/lib/yuck",
            "--no-output-ozn", "--output-fzn-to-file", fznFilePath)
        mzn2fznCommand += mznFilePath
        if (! dznFilePath.isEmpty) mzn2fznCommand += dznFilePath
        logger.withTimedLogScope("Flattening MiniZinc model") {
            val (_, errorLines) = new ProcessRunner(logger, mzn2fznCommand).call
            Predef.assert(errorLines.isEmpty, "Flattening failed")
        }
        val ast =
            logger.withTimedLogScope("Parsing FlatZinc file") {
                new FlatZincFileParser(fznFilePath, logger).call
            }
        val cfg =
            task.solverConfiguration.copy(
                restartLimit =
                    scala.math.min(
                        task.solverConfiguration.restartLimit,
                        task.maybeRestartLimit.getOrElse(Int.MaxValue)),
                numberOfVirtualCores =
                    scala.math.min(
                        task.solverConfiguration.numberOfVirtualCores,
                        task.maybeMaximumNumberOfVirtualCores.getOrElse(Int.MaxValue)),
                maybeRoundLimit = task.maybeRoundLimit,
                maybeRuntimeLimitInSeconds = task.maybeRuntimeLimitInSeconds,
                maybeTargetObjectiveValue = task.maybeOptimum,
                maybeQualityTolerance = task.maybeQualityTolerance)
        val sigint = new SettableSigint
        val result =
            scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); sigint.set})) {
                using(new StandardAnnealingMonitor(logger)) {
                    monitor => new FlatZincSolverGenerator(ast, cfg, logger, monitor, sigint).call.call
                }
            }
        logger.log("Quality of best proposal: %s".format(result.costsOfBestProposal))
        logger.log("Best proposal was produced by: %s".format(result.solverName))
        if (! result.isSolution) {
            logger.withRootLogLevel(yuck.util.logging.FinerLogLevel) {
                logger.withLogScope("Violated constraints") {
                    logViolatedConstraints(result)
                }
            }
        }
        logger.withLogScope("Best proposal") {
            new FlatZincResultFormatter(result).call.foreach(logger.log(_))
        }
        Assert.assertTrue(
            "No solution found, quality of best proposal was %s".format(result.costsOfBestProposal),
            result.isSolution)
        logger.withTimedLogScope("Verifying solution") {
            Assert.assertTrue(
                "Solution not verified",
                new MiniZincSolutionVerifier(task, result, logger).call)
        }
        result
    }

    private def logViolatedConstraints(result: Result) {
        val visited = new mutable.HashSet[AnyVariable]
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        result.space.definingConstraint(compilerResult.costVar).get match {
            case sum: yuck.constraints.Sum[IntegerValue @ unchecked] =>
                for (x <- sum.xs if result.space.searchState.value(x) > Zero) {
                    logViolatedConstraints(result, x, visited)
                }
        }
    }

    private def logViolatedConstraints(
        result: Result, x: AnyVariable, visited: mutable.Set[AnyVariable])
    {
        val a = result.bestProposal.anyValue(x)
        if (! visited.contains(x)) {
            visited += x
            val maybeConstraint = result.space.definingConstraint(x)
            if (maybeConstraint.isDefined) {
                val constraint = maybeConstraint.get
                logger.withLogScope("%s = %s computed by %s [%s]".format(x, a, constraint, constraint.goal)) {
                    for (x <- constraint.inVariables) {
                        logViolatedConstraints(result, x, visited)
                    }
                }
             } else if (! x.isParameter) {
                logger.logg("%s = %s".format(x, a, visited))
            }
        }
    }

    private def handleException(error: Throwable): Result = error match {
        case error: FlatZincParserException =>
            nativeLogger.info(error.getMessage)
            throw error
        case error: InconsistentProblemException =>
            nativeLogger.info(error.getMessage)
            nativeLogger.info(FLATZINC_INCONSISTENT_PROBLEM_INDICATOR)
            throw error
        case error: Throwable =>
            nativeLogger.log(java.util.logging.Level.SEVERE, "", error)
            throw error
    }

    private def findUltimateCause(error: Throwable): Throwable =
        if (error.getCause == null) error else findUltimateCause(error.getCause)

}
