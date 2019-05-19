package yuck.flatzinc.test.util

import scala.collection._

import org.junit._
import spray.json._

import yuck.annealing._
import yuck.core._
import yuck.flatzinc.ast._
import yuck.flatzinc.compiler.FlatZincCompilerResult
import yuck.flatzinc.parser._
import yuck.flatzinc.runner._
import yuck.util.arm._
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

    private val jsonNodes = new mutable.ArrayBuffer[JsField]

    private def trySolve(task: MiniZincTestTask): Result = {
        val suitePath = task.suitePath
        val suiteName = if (task.suiteName.isEmpty) new java.io.File(suitePath).getName else task.suiteName
        val problemName = task.problemName
        val modelName = if (task.modelName.isEmpty) problemName else task.modelName
        val instanceName = if (task.instanceName.isEmpty) modelName else task.instanceName
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
        val mzn2fznCommand = mutable.ArrayBuffer(
            "mzn2fzn",
            "-v",
            "-I", "resources/mzn/lib/yuck",
            "--no-output-ozn", "--output-fzn-to-file", fznFilePath)
        mzn2fznCommand += mznFilePath
        if (! dznFilePath.isEmpty) mzn2fznCommand += dznFilePath
        logger.withTimedLogScope("Flattening MiniZinc model") {
            new ProcessRunner(logger, mzn2fznCommand).call
        }
        val cfg =
            task.solverConfiguration.copy(
                restartLimit =
                    scala.math.min(
                        task.solverConfiguration.restartLimit,
                        task.maybeRestartLimit.getOrElse(Int.MaxValue)),
                numberOfThreads =
                    scala.math.min(
                        task.solverConfiguration.numberOfThreads,
                        task.maybeMaximumNumberOfThreads.getOrElse(Int.MaxValue)),
                maybeRoundLimit = task.maybeRoundLimit,
                maybeRuntimeLimitInSeconds = task.maybeRuntimeLimitInSeconds,
                maybeTargetObjectiveValue = task.maybeOptimum,
                maybeQualityTolerance = task.maybeQualityTolerance)
        val sigint = new SettableSigint
        val monitor = new StandardAnnealingMonitor(logger)
        val result =
            scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); sigint.set})) {
                maybeTimeboxed(cfg.maybeRuntimeLimitInSeconds, sigint, "solver", logger) {
                    val ast =
                        logger.withTimedLogScope("Parsing FlatZinc file") {
                            new FlatZincFileParser(fznFilePath, logger).call
                        }
                    logTask(task.copy(suiteName = suiteName, modelName = modelName, instanceName = instanceName), ast)
                    logFlatZincModelStatistics(ast)
                    scoped(monitor) {
                        new FlatZincSolverGenerator(ast, cfg, sigint, logger, monitor).call.call
                    }
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
        logYuckModelStatistics(result.space)
        logResult(result)
        logSolverStatistics(monitor)
        val jsonDoc = new JsObject(jsonNodes.toMap)
        val jsonFilePath = "%s/yuck.json".format(outputDirectoryPath)
        val jsonWriter = new java.io.FileWriter(jsonFilePath)
        jsonWriter.write(jsonDoc.prettyPrint)
        jsonWriter.close
        Assert.assertTrue(
            "No solution found, quality of best proposal was %s".format(result.costsOfBestProposal),
            result.isSolution || ! task.assertWhenUnsolved)
        if (result.isSolution) {
            logger.withTimedLogScope("Verifying solution") {
                Assert.assertTrue(
                    "Solution not verified",
                    new MiniZincSolutionVerifier(task, result, logger).call)
            }
        }
        result
    }

    private def logTask(task: MiniZincTestTask, ast: FlatZincAst) {
        val problemType =
            ast.solveGoal match {
                case Satisfy(_) => "SAT"
                case Minimize(_, _) => "MIN"
                case Maximize(_, _) => "MAX"
            }
        val taskNodes = new mutable.ArrayBuffer[JsField]
        taskNodes ++= List(
            "suite" -> JsString(task.suiteName),
            "problem" -> JsString(task.problemName),
            "model" -> JsString(task.modelName),
            "instance" -> JsString(task.instanceName),
            "problem-type" -> JsString(problemType)
        )
        if (task.maybeOptimum.isDefined) {
            taskNodes += "optimum" -> JsNumber(task.maybeOptimum.get)
        }
        if (task.maybeHighScore.isDefined) {
            taskNodes += "high-score" -> JsNumber(task.maybeHighScore.get)
        }
        jsonNodes += "task" -> JsObject(taskNodes.toMap)
    }

    private def logFlatZincModelStatistics(ast: FlatZincAst) {
        jsonNodes +=
            "flatzinc-model-statistics" -> JsObject(
                "number-of-predicate-declarations" -> JsNumber(ast.predDecls.size),
                "number-of-parameter-declarations" -> JsNumber(ast.paramDecls.size),
                "number-of-variable-declarations" -> JsNumber(ast.varDecls.size),
                "number-of-constraints" -> JsNumber(ast.constraints.size)
            )
    }

    private def logYuckModelStatistics(space: Space) {
        jsonNodes +=
            "yuck-model-statistics" -> JsObject(
                "number-of-search-variables" -> JsNumber(space.searchVariables.size),
                "number-of-channel-variables" -> JsNumber(space.channelVariables.size),
                // dangling variables are not readily available
                "number-of-constraints" -> JsNumber(space.numberOfConstraints),
                "number-of-implicit-constraints" -> JsNumber(space.numberOfImplicitConstraints)
            )
    }

    private def logResult(result: Result) {
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        val resultNodes = new mutable.ArrayBuffer[JsField]
        resultNodes += "solved" -> JsBoolean(result.isSolution)
        if (! result.isSolution) {
            resultNodes += "violation" -> JsNumber(result.bestProposal.value(compilerResult.costVar).violation)
        }
        if (compilerResult.maybeObjectiveVar.isDefined) {
            resultNodes += "quality" -> JsNumber(result.bestProposal.value(compilerResult.maybeObjectiveVar.get).value)
        }
        jsonNodes += "result" -> JsObject(resultNodes.toMap)
    }

    private def logSolverStatistics(monitor: StandardAnnealingMonitor) {
        if (monitor.wasSearchRequired) {
            jsonNodes +=
                "solver-statistics" -> JsObject(
                    "number-of-restarts" -> JsNumber(monitor.numberOfRestarts),
                    "runtime-in-seconds" -> JsNumber(monitor.runtimeInSeconds),
                    "moves-per-second" -> JsNumber(monitor.movesPerSecond),
                    "consultations-per-second" -> JsNumber(monitor.consultationsPerSecond),
                    "consultations-per-move" -> JsNumber(monitor.consultationsPerMove),
                    "commitments-per-second" -> JsNumber(monitor.commitmentsPerSecond),
                    "commitments-per-move" -> JsNumber(monitor.commitmentsPerMove)
                )
        }
    }

    private def logViolatedConstraints(result: Result) {
        val visited = new mutable.HashSet[AnyVariable]
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        result.space.definingConstraint(compilerResult.costVar).get match {
            case sum: yuck.constraints.Sum[BooleanValue @ unchecked] =>
                for (x <- sum.xs if result.space.searchState.value(x) > True) {
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
             } else if (! result.space.isProblemParameter(x)) {
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
            nativeLogger.info(FlatZincInconsistentProblemIndicator)
            throw error
        case error: SolverInterruptedException =>
            throw new AssertionError("No solution found")
        case error: Throwable =>
            nativeLogger.log(java.util.logging.Level.SEVERE, "", error)
            throw error
    }

    private def findUltimateCause(error: Throwable): Throwable =
        if (error.getCause == null) error else findUltimateCause(error.getCause)

}
