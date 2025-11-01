package yuck.flatzinc.test.util

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.collection.*
import scala.language.implicitConversions

import spray.json.*

import yuck.annealing.{AnnealingEventLogger, AnnealingMonitorCollection, AnnealingResult, AnnealingStatisticsCollector}
import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.*
import yuck.flatzinc.compiler.{FlatZincCompilerResult, UnsupportedFlatZincTypeException, VariableWithInfiniteDomainException}
import yuck.flatzinc.parser.*
import yuck.flatzinc.runner.*
import yuck.flatzinc.test.util.SourceFormat.*
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*
import yuck.flatzinc.test.util.VerificationFrequency.*
import yuck.flatzinc.util.{SharedBoundMaintainer, SummaryBuilder}
import yuck.test.util.{IntegrationTest, ProcessRunner}
import yuck.util.arm.*
import yuck.util.logging.ManagedLogHandler
import yuck.util.logging.LogLevel.*

/**
 * @author Michael Marte
 *
 */
class ZincBasedTest extends IntegrationTest {

    extension (result: Result) {

        protected def neighbourhood: Neighbourhood =
            compilerResult.maybeNeighbourhood.get

        protected def violation: BooleanValue =
            result.costsOfBestProposal match {
                case violation: BooleanValue => violation
                case costs: PolymorphicListValue => costs.value(0).asInstanceOf[BooleanValue]
            }

        protected def quality(i: Int): AnyValue =
            result.costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(i)

        protected def quality: AnyValue =
            result.quality(1)

        protected def assignment: SearchState =
            space.searchState

        protected def space: Space =
            compilerResult.space

        protected def warmStartWasPerformed: Boolean =
            compilerResult.performWarmStart

        protected def searchWasPerformed: Boolean =
            ! result.asInstanceOf[AnnealingResult].roundLogs.isEmpty

        protected def compilerResult: FlatZincCompilerResult =
            result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]

    }

    private val summaryBuilder = new SummaryBuilder
    import SummaryBuilder.*

    protected def solveWithResult(task: ZincTestTask): Result = {
        solve(task.copy(reusePreviousTestResult = false)).get
    }

    // Asserts when something went wrong.
    // Returns None when task.reusePreviousTestResult is true and the instance was already processed.
    protected def solve(task: ZincTestTask): Option[Result] = {
        logger.setThresholdLogLevel(task.logLevel)
        summaryBuilder.addOsEnv()
        summaryBuilder.addJavaEnv()
        summaryBuilder.addYuckVersion()
        val suitePath = task.suitePath
        val suiteName = if (task.suiteName.isEmpty) new java.io.File(suitePath).getName else task.suiteName
        val problemName = task.problemName
        val modelName = if (task.modelName.isEmpty) problemName else task.modelName
        val instanceName = if (task.instanceName.isEmpty) modelName else task.instanceName
        val outputDirectoryPath0 = task.directoryLayout match {
            case MiniZincExamplesLayout =>
                "tmp/%s/%s".format (suiteName, problemName)
            case StandardMiniZincBenchmarksLayout =>
                "tmp/%s/%s/%s/%s".format(suiteName, problemName, modelName, instanceName)
            case NonStandardMiniZincBenchmarksLayout =>
                "tmp/%s/%s/%s".format(suiteName, problemName, instanceName)
        }
        val outputDirectoryPath = task.solverConfiguration.maybeName
            .map(name => "%s/%s".format(outputDirectoryPath0, name))
            .getOrElse(outputDirectoryPath0)
        new java.io.File(outputDirectoryPath).mkdirs
        val logFilePath = "%s/yuck.log".format(outputDirectoryPath)
        val summaryFilePath = "%s/yuck.json".format(outputDirectoryPath)
        if (task.reusePreviousTestResult && new java.io.File(summaryFilePath).exists() && ! task.throwWhenUnsolved) {
            None
        } else scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); sigint.set()})) {
            val logFileHandler = new java.util.logging.FileHandler(logFilePath)
            logFileHandler.setFormatter(formatter)
            scoped(new ManagedLogHandler(nativeLogger, logFileHandler)) {
                logger.log("Logging into %s".format(logFilePath))
                solve(
                    task.copy(suiteName = suiteName, modelName = modelName, instanceName = instanceName),
                    outputDirectoryPath,
                    summaryFilePath)
            }
        }
    }

    private def solve(task: ZincTestTask, outputDirectoryPath: String, summaryFilePath: String): Option[Result] = {
        try {
            Some(trySolve(task, outputDirectoryPath))
        }
        catch {
            case error: Throwable =>
                handleException(task, findUltimateCause(error))
                None
        }
        finally {
            val jsonDoc = summaryBuilder.build()
            val jsonWriter = new java.io.FileWriter(summaryFilePath)
            jsonWriter.write(jsonDoc.prettyPrint)
            jsonWriter.close()
        }

    }

    private def trySolve(task: ZincTestTask, outputDirectoryPath: String): Result = {
        val fznFilePath = task.sourceFormat match {
            case FlatZinc => "%s/%s.fzn".format(task.suitePath, task.problemName)
            case MiniZinc => flatten(task, outputDirectoryPath)
        }
        logger.log("Processing %s".format(fznFilePath))
        val cfg = createSolverConfiguration(task)
        summaryBuilder.addSolverConfiguration(cfg)
        val statisticsCollector = new AnnealingStatisticsCollector(logger)
        val sharedBoundHolder = new AtomicReference[Costs]
        val monitors =
            Vector(new AnnealingEventLogger(logger), statisticsCollector)
                .appendedAll(
                    if task.sourceFormat == MiniZinc && task.verificationFrequency == VerifyEverySolution
                    then List(new MiniZincTestMonitor(task, spoilResult, logger))
                    else Nil)
                .appendedAll(
                    if cfg.shareBounds
                    then List(new SharedBoundMaintainer(sharedBoundHolder))
                    else Nil)
                .appendedAll(task.additionalMonitors)
        val monitor = new AnnealingMonitorCollection(monitors)
        val ((ast, result), _) = maybeTimeboxed(cfg.maybeRuntimeLimitInSeconds, sigint, "solver", logger) {
            val (ast, parserRuntime) =
                logger.withTimedLogScope("Parsing FlatZinc file") {
                    new FlatZincParser(fznFilePath, logger).call()
                }
            summaryBuilder.addTask(task, ast)
            summaryBuilder.addParserStatistics(parserRuntime)
            val md5Sum = SummaryBuilder.computeMd5Sum(fznFilePath)
            summaryBuilder.addFlatZincModelStatistics(ast, md5Sum)
            logger.withTimedLogScope("Solving problem") {
                scoped(monitor) {
                    val sharedBound = new SharedBound(sharedBoundHolder)
                    (ast, new FlatZincSolverGenerator(ast, cfg, sharedBound, monitor, logger, sigint).call().call())
                }
            }
        }
        logger.log("Quality of best proposal: %s".format(result.costsOfBestProposal))
        logger.log("Best proposal was produced by: %s".format(result.solverName))
        logger.withLogScope("Best proposal") {
            new FlatZincResultFormatter(ast)(new FlatZincResult(result)).foreach(logger.log(_))
        }
        summaryBuilder.addYuckModelStatistics(result.space)
        summaryBuilder.addResult(result)
        summaryBuilder.addSearchStatistics(statisticsCollector)
        if (cfg.maybeSpaceProfilingMode.isDefined) {
            summaryBuilder.addSpacePerformanceMetrics(result.space.performanceMetricsBuilder.build())
        }
        if (task.createDotFile) {
            logger.withTimedLogScope("Exporting constraint network to a DOT file") {
                val dotFilePath = "%s/yuck.dot".format(outputDirectoryPath)
                val dotWriter = new java.io.FileWriter(dotFilePath)
                new DotExporter(result.space, dotWriter).run()
            }
        }
        if (result.isSolution) {
            if (task.sourceFormat == MiniZinc && task.verificationFrequency == VerifyOnlyLastSolution) {
                verifySolution(task, result)
            }
        } else {
            logger.withRootLogLevel(FinerLogLevel) {
                logger.withLogScope("Violated constraints") {
                    logViolatedConstraints(result)
                }
            }
            assert(
                "No solution found, quality of best proposal was %s".format(result.costsOfBestProposal),
                ! task.throwWhenUnsolved)
        }
        if (task.sourceFormat == MiniZinc && ! task.keepFlatZincFile) {
            new java.io.File(fznFilePath).delete()
        }
        result
    }

    private def flatten(task: ZincTestTask, outputDirectoryPath: String): String = {
        require(task.sourceFormat == MiniZinc)
        val fznFilePath = "%s/problem.fzn".format(outputDirectoryPath)
        val oznFilePath = "%s/problem.ozn".format(outputDirectoryPath)
        val (mznFilePath, dataFilePath) = task.directoryLayout match {
            case MiniZincExamplesLayout =>
                ("%s/%s.mzn".format(task.suitePath, task.problemName), "")
            case StandardMiniZincBenchmarksLayout =>
                ("%s/%s/%s.mzn".format(task.suitePath, task.problemName, task.modelName), {
                    val dznFilePath = "%s/%s/%s.dzn".format(task.suitePath, task.problemName, task.instanceName)
                    val jsonFilePath = "%s/%s/%s.json".format(task.suitePath, task.problemName, task.instanceName)
                    if (new java.io.File(dznFilePath).exists()) dznFilePath else jsonFilePath
                })
            case NonStandardMiniZincBenchmarksLayout =>
                ("%s/%s/%s.mzn".format(task.suitePath, task.problemName, task.instanceName), "")
        }
        val miniZincCommand = mutable.ArrayBuffer(
            "minizinc",
            "-v",
            "-c",
            "--solver", "org.minizinc.mzn-fzn",
            "-I", "resources/mzn/lib/yuck",
            "--output-fzn-to-file", fznFilePath)
        if (task.miniZincCompilerRenamesVariables) {
            miniZincCommand ++= List(
                "--output-mode", "dzn",
                "--output-ozn-to-file", oznFilePath)
        } else {
            miniZincCommand += "--no-output-ozn"
        }
        for ((key, value) <- task.dataAssignments) {
            miniZincCommand ++= List("-D", "%s=%s".format(key, value))
        }
        miniZincCommand += mznFilePath
        if (! dataFilePath.isEmpty) {
            miniZincCommand += dataFilePath
        }
        val (outputLines, _) =
            logger.withTimedLogScope("Flattening MiniZinc model") {
                logger.withRootLogLevel(FineLogLevel) {
                    new ProcessRunner(logger, miniZincCommand).call()
                }
            }
        summaryBuilder.addMiniZincVersion(outputLines.head)
        fznFilePath
    }

    private def createSolverConfiguration(task: ZincTestTask): FlatZincSolverConfiguration = {
        task.solverConfiguration.copy(
            numberOfSolvers =
                task.maybeNumberOfSolvers
                    .getOrElse(task.solverConfiguration.numberOfSolvers),
            maybeRoundLimit =
                task.maybeRoundLimit
                    .orElse(task.solverConfiguration.maybeRoundLimit),
            maybeRuntimeLimitInSeconds =
                task.maybeRuntimeLimitInSeconds
                    .orElse(task.solverConfiguration.maybeRuntimeLimitInSeconds),
            maybeTargetObjectiveValue =
                task.maybeTargetObjectiveValue
                    .orElse(task.maybeOptimum)
                    .orElse(task.maybeHighScore)
                    .orElse(task.solverConfiguration.maybeTargetObjectiveValue)
        )
    }

    protected def spoilResult(result: Result): Result = result

    private def verifySolution(task: ZincTestTask, result: Result): Unit = {
        logger.withTimedLogScope("Verifying solution") {
            logger.withRootLogLevel(FineLogLevel) {
                val verifier = new MiniZincSolutionVerifier(task, spoilResult(result), logger)
                if (! verifier.call()) {
                    throw new SolutionNotVerifiedException
                }
            }
        }
    }

    extension (summaryBuilder: SummaryBuilder) {

        private def addTask(task: ZincTestTask, ast: FlatZincAst): SummaryBuilder = {
            val problemType =
                ast.solveGoal match {
                    case Satisfy(_) => "SAT"
                    case Minimize(_, _) => "MIN"
                    case Maximize(_, _) => "MAX"
                }
            val taskNode = JsObjectBuilder(
                "suite" -> JsString(task.suiteName),
                "problem" -> JsString(task.problemName),
                "model" -> JsString(task.modelName),
                "instance" -> JsString(task.instanceName),
                "problem-type" -> JsString(problemType)
            )
            if (task.maybeOptimum.isDefined) {
                taskNode += "optimum" -> JsNumber(task.maybeOptimum.get)
            }
            if (task.maybeHighScore.isDefined) {
                taskNode += "high-score" -> JsNumber(task.maybeHighScore.get)
            }
            summaryBuilder.extendRoot("task", taskNode)
        }

        private def addMiniZincVersion(versionInfo: String): SummaryBuilder = {
            // MiniZinc to FlatZinc converter, version 2.3.1, build 70205949
            val pattern = java.util.regex.Pattern.compile(".*, version ([\\.\\d]+), build (\\d+)")
            val matcher = pattern.matcher(versionInfo)
            if (matcher.matches) {
                summaryBuilder.extendEnv(
                    "minizinc",
                    JsObjectBuilder(
                        "version" -> JsString(matcher.group(1)),
                        "build" -> JsString(matcher.group(2))
                    ))
            }
            summaryBuilder
        }

    }

    private def logViolatedConstraints(result: Result): Unit = {
        val visited = new mutable.HashSet[AnyVariable]
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        val costVar = compilerResult.objective.objectiveVariables(0).asInstanceOf[BooleanVariable]
        result.space.definingConstraint(costVar) match {
            case sum: yuck.constraints.Conjunction =>
                for (x <- sum.xs if result.space.searchState.value(x) > True) {
                    logViolatedConstraints(result, x, visited)
                }
            case _ =>
        }
    }

    private def logViolatedConstraints(
        result: Result, x: AnyVariable, visited: mutable.Set[AnyVariable]): Unit =
    {
        val a = result.bestProposal.value(x)
        if (! visited.contains(x)) {
            visited += x
            val maybeConstraint = result.space.maybeDefiningConstraint(x)
            if (maybeConstraint.isDefined) {
                val constraint = maybeConstraint.get
                logger.withLogScope("%s = %s computed by %s [%s]".format(x, a, constraint, constraint.maybeGoal)) {
                    for (x <- constraint.inVariables) {
                        logViolatedConstraints(result, x, visited)
                    }
                }
             } else if (! result.space.isProblemParameter(x)) {
                logger.logg("%s = %s".format(x, a))
            }
        }
    }

    private def handleException(task: ZincTestTask, throwable: Throwable): Unit = {
        throwable match {
            case _: FlatZincParserException | _: SolutionNotVerifiedException =>
                summaryBuilder.addError(throwable)
                logger.log(throwable.getMessage)
                throw throwable
            case _: UnsupportedFlatZincTypeException | _: VariableWithInfiniteDomainException =>
                summaryBuilder.addWarning(throwable)
                logger.log(throwable.getMessage)
                throw throwable
            case _: InconsistentProblemException =>
                summaryBuilder.extendResult("satisfiable", JsBoolean(false))
                logger.log(throwable.getMessage)
                logger.log(FlatZincInconsistentProblemIndicator)
                throw throwable
            case _: InterruptedException =>
                summaryBuilder.addWarning(throwable)
                logger.log(throwable.getMessage)
                logger.log(FlatZincNoSolutionFoundIndicator)
                assert(throwable.getMessage, ! task.throwWhenUnsolved)
            case _: Throwable =>
                summaryBuilder.addError(throwable)
                logger.withLogScope(throwable.getMessage) {
                    throwable.getStackTrace.foreach(frame => logger.log(frame.toString))
                }
                throw throwable
        }
    }

    @tailrec
    private def findUltimateCause(throwable: Throwable): Throwable =
        if (throwable.getCause.eq(null)) throwable else findUltimateCause(throwable.getCause)

}
