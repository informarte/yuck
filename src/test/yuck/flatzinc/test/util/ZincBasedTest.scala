package yuck.flatzinc.test.util

import java.time.Duration

import scala.annotation.tailrec
import scala.collection.*
import scala.language.implicitConversions

import spray.json.*
import yuck.BuildInfo
import yuck.core.{*, given}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.*
import yuck.flatzinc.compiler.{FlatZincCompilerResult, UnsupportedFlatZincTypeException, VariableWithInfiniteDomainException}
import yuck.flatzinc.parser.*
import yuck.flatzinc.runner.*
import yuck.test.util.{IntegrationTest, ProcessRunner}
import yuck.util.arm.*
import yuck.util.logging.{FineLogLevel, ManagedLogHandler}

/**
 * @author Michael Marte
 *
 */
class ZincBasedTest extends IntegrationTest {

    private abstract class JsNode {
        def value: JsValue
    }
    private class JsEntry(override val value: JsValue) extends JsNode
    private object JsEntry {
        def apply(value: JsValue): JsEntry = new JsEntry(value)
    }
    private implicit def createJsNode(value: JsValue): JsNode = new JsEntry(value)
    private class JsSection extends JsNode {
        private val fields = new mutable.HashMap[String, JsNode]
        override def value =
            JsObject(fields.iterator.map{case (name, node) => (name, node.value)}.to(immutable.TreeMap))
        def +=(field: (String, JsNode)): JsSection = {
            fields += field
            this
        }
        def ++=(fields: IterableOnce[(String, JsNode)]): JsSection = {
            this.fields ++= fields
            this
        }
    }
    private object JsSection {
        def apply(fields: (String, JsNode)*): JsSection =
            new JsSection ++= fields
    }

    private val jsonRoot = new JsSection
    private val envNode = new JsSection
    private val resultNode = new JsSection

    protected def solveWithResult(task: ZincTestTask): Result = {
        solve(task.copy(reusePreviousTestResult = false)).get
    }

    // Asserts when something went wrong.
    // Returns None when task.reusePreviousTestResult is true and the instance was already processed.
    protected def solve(task: ZincTestTask): Option[Result] = {
        logger.setThresholdLogLevel(task.logLevel)
        jsonRoot += "env" -> envNode
        logOsEnv()
        logJavaEnv()
        logYuckVersion()
        val suitePath = task.suitePath
        val suiteName = if (task.suiteName.isEmpty) new java.io.File(suitePath).getName else task.suiteName
        val problemName = task.problemName
        val modelName = if (task.modelName.isEmpty) problemName else task.modelName
        val instanceName = if (task.instanceName.isEmpty) modelName else task.instanceName
        val outputDirectoryPath = task.directoryLayout match {
            case MiniZincExamplesLayout =>
                "tmp/%s/%s".format (suiteName, problemName)
            case StandardMiniZincBenchmarksLayout =>
                "tmp/%s/%s/%s/%s".format(suiteName, problemName, modelName, instanceName)
            case NonStandardMiniZincBenchmarksLayout =>
                "tmp/%s/%s/%s".format(suiteName, problemName, instanceName)
        }
        new java.io.File(outputDirectoryPath).mkdirs
        val logFilePath = "%s/yuck.log".format(outputDirectoryPath)
        val summaryFilePath = "%s/yuck.json".format(outputDirectoryPath)
        if (task.reusePreviousTestResult && new java.io.File(summaryFilePath).exists() && ! task.throwWhenUnsolved) {
            None
        } else {
            jsonRoot += "result" -> resultNode
            val logFileHandler = new java.util.logging.FileHandler(logFilePath)
            logFileHandler.setFormatter(formatter)
            scoped(new ManagedLogHandler(nativeLogger, logFileHandler)) {
                logger.log("Logging into %s".format(logFilePath))
                try {
                    val result = solve(
                        task.copy(suiteName = suiteName, modelName = modelName, instanceName = instanceName),
                        outputDirectoryPath)
                    Some(result)
                }
                catch {
                    case error: Throwable =>
                        handleException(task, findUltimateCause(error))
                        None
                }
                finally {
                    val jsonDoc = jsonRoot.value
                    val jsonWriter = new java.io.FileWriter(summaryFilePath)
                    jsonWriter.write(jsonDoc.prettyPrint)
                    jsonWriter.close()
                }
            }
        }
    }

    private def solve(task: ZincTestTask, outputDirectoryPath: String): Result = {
        val fznFilePath = task.sourceFormat match {
            case FlatZinc => "%s/%s.fzn".format(task.suitePath, task.problemName)
            case MiniZinc => flatten(task, outputDirectoryPath)
        }
        logger.log("Processing %s".format(fznFilePath))
        val cfg = createSolverConfiguration(task)
        logSolverConfiguration(cfg)
        val monitor = createTestMonitor(task)
        val (result, _) =
            scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); sigint.set()})) {
                maybeTimeboxed(cfg.maybeRuntimeLimitInSeconds, sigint, "solver", logger) {
                    val (ast, parserRuntime) =
                        logger.withTimedLogScope("Parsing FlatZinc file") {
                            new FlatZincFileParser(fznFilePath, logger).call()
                        }
                    logTask(task, ast)
                    logParserStatistics(parserRuntime)
                    val md5Sum = computeMd5Sum(fznFilePath)
                    logFlatZincModelStatistics(ast, md5Sum)
                    logger.withTimedLogScope("Solving problem") {
                        scoped(monitor) {
                            new FlatZincSolverGenerator(ast, cfg, sigint, logger, monitor).call().call()
                        }
                    }
                }
            }
        logger.log("Quality of best proposal: %s".format(result.costsOfBestProposal))
        logger.log("Best proposal was produced by: %s".format(result.solverName))
        logger.withLogScope("Best proposal") {
            new FlatZincResultFormatter(result).call().foreach(logger.log(_))
        }
        logYuckModelStatistics(result.space)
        logResult(result)
        logQualityStepFunction(monitor)
        logSearchStatistics(monitor)
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
            logger.withRootLogLevel(yuck.util.logging.FinerLogLevel) {
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
        logMiniZincVersion(outputLines.head)
        fznFilePath
    }

    private def createSolverConfiguration(task: ZincTestTask): FlatZincSolverConfiguration = {
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
            maybeTargetObjectiveValue =
                if (task.maybeTargetObjectiveValue.isDefined) task.maybeTargetObjectiveValue
                else task.maybeOptimum)
    }

    protected def createTestMonitor(task: ZincTestTask): ZincTestMonitor = {
        new ZincTestMonitor(task, logger)
    }

    private def verifySolution(task: ZincTestTask, result: Result): Unit = {
        logger.withTimedLogScope("Verifying solution") {
            logger.withRootLogLevel(FineLogLevel) {
                val verifier = new MiniZincSolutionVerifier(task, result, logger)
                if (!verifier.call()) {
                    throw new SolutionNotVerifiedException
                }
            }
        }
    }

    private def computeMd5Sum(filePath: String): String = {
        import java.math.BigInteger
        import java.nio.file.{Files, Paths}
        import java.security.MessageDigest
        val md = MessageDigest.getInstance("MD5")
        md.update(Files.readAllBytes(Paths.get(filePath)))
        new BigInteger(1, md.digest).toString(16)
    }

    private def logTask(task: ZincTestTask, ast: FlatZincAst): Unit = {
        val problemType =
            ast.solveGoal match {
                case Satisfy(_) => "SAT"
                case Minimize(_, _) => "MIN"
                case Maximize(_, _) => "MAX"
            }
        val taskNode = JsSection(
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
        jsonRoot += "task" -> taskNode
    }

    private def logOsEnv(): Unit = {
        envNode +=
            "os" -> JsSection(
                "arch" -> JsString(System.getProperty("os.arch", "")),
                "name" -> JsString(System.getProperty("os.name", "")),
                "version" -> JsString(System.getProperty("os.version", ""))
            )
    }

    private def logJavaEnv(): Unit = {
        val runtime = java.lang.Runtime.getRuntime
        envNode +=
            "java" -> JsSection(
                "runtime" -> JsSection(
                    "number-of-virtual-cores" -> JsNumber(runtime.availableProcessors),
                    "max-memory" -> JsNumber(runtime.maxMemory)
                ),
                "vm" -> JsSection(
                    "name" -> JsString(System.getProperty("java.vm.name", "")),
                    "version" -> JsString(System.getProperty("java.vm.version", ""))
                ),
                "vendor" -> JsSection(
                    "name" -> JsString(System.getProperty("java.vendor", "")),
                    "version" -> JsString(System.getProperty("java.vendor.version", ""))
                ),
                "version" -> JsString(System.getProperty("java.version", ""))
            )
    }

    private def logYuckVersion(): Unit = {
        jsonRoot +=
            "solver" -> JsSection(
                "name" -> JsString("yuck"),
                "branch" -> JsString(BuildInfo.gitBranch),
                "commit-date" -> JsString(BuildInfo.gitCommitDate),
                "commit-hash" -> JsString(BuildInfo.gitCommitHash),
                "version" -> JsString(BuildInfo.version))
    }

    private def logMiniZincVersion(versionInfo: String): Unit = {
        // MiniZinc to FlatZinc converter, version 2.3.1, build 70205949
        val pattern = java.util.regex.Pattern.compile(".*, version ([\\.\\d]+), build (\\d+)")
        val matcher = pattern.matcher(versionInfo)
        if (matcher.matches) {
            envNode +=
                "minizinc" -> JsSection(
                    "version" -> JsString(matcher.group(1)),
                    "build" -> JsString(matcher.group(2))
                )
        }
    }

    private def logSolverConfiguration(cfg: FlatZincSolverConfiguration): Unit = {
        val cfgNode =
            JsSection(
                "seed" -> JsNumber(cfg.seed),
                "restart-limit" -> JsNumber(cfg.restartLimit),
                "number-of-threads" -> JsNumber(cfg.numberOfThreads),
                "focus-on-top-objective" -> JsBoolean(cfg.focusOnTopObjective),
                "stop-on-first-solution" -> JsBoolean(cfg.stopOnFirstSolution),
                "prune-constraint-network" -> JsBoolean(cfg.pruneConstraintNetwork),
                "run-presolver" -> JsBoolean(cfg.runPresolver),
                "use-implicit-solving" -> JsBoolean(cfg.useImplicitSolving),
                "use-progressive-tightening" -> JsBoolean(cfg.useProgressiveTightening)
            )
        if (cfg.maybeRoundLimit.isDefined) {
            cfgNode += "round-limit" -> JsNumber(cfg.maybeRoundLimit.get)
        }
        if (cfg.maybeRuntimeLimitInSeconds.isDefined) {
            cfgNode += "runtime-limit-in-seconds" -> JsNumber(cfg.maybeRuntimeLimitInSeconds.get)
        }
        if (cfg.maybeTargetObjectiveValue.isDefined) {
            cfgNode += "target-objective-value" -> JsNumber(cfg.maybeTargetObjectiveValue.get)
        }
        jsonRoot +="solver-configuration" -> cfgNode
    }

    private def logFlatZincModelStatistics(ast: FlatZincAst, md5Sum: String): Unit = {
        jsonRoot +=
            "flatzinc-model-statistics" -> JsSection(
                "md5sum" -> JsString(md5Sum),
                "number-of-predicate-declarations" -> JsNumber(ast.predDecls.size),
                "number-of-parameter-declarations" -> JsNumber(ast.paramDecls.size),
                "number-of-variable-declarations" -> JsNumber(ast.varDecls.size),
                "number-of-constraints" -> JsNumber(ast.constraints.size)
            )
    }

    private def logYuckModelStatistics(space: Space): Unit = {
        jsonRoot +=
            "yuck-model-statistics" -> JsSection(
                "number-of-search-variables" -> JsNumber(space.searchVariables.size),
                "number-of-implicitly-constrained-search-variables" ->
                    JsNumber(space.implicitlyConstrainedSearchVariables.size),
                "number-of-channel-variables" -> JsNumber(space.channelVariables.size),
                // dangling variables are not readily available
                "number-of-constraints" -> JsNumber(space.numberOfConstraints),
                "number-of-implicit-constraints" -> JsNumber(space.numberOfImplicitConstraints)
            )
    }

    private def logResult(result: Result): Unit = {
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        logCompilerStatistics(compilerResult)
        val objectiveVariables = compilerResult.objective.objectiveVariables
        resultNode += "solved" -> JsBoolean(result.isSolution)
        if (! result.isSolution) {
            val costVar = objectiveVariables(0).asInstanceOf[BooleanVariable]
            resultNode += "violation" -> JsNumber(result.bestProposal.value(costVar).violation)
        }
        if (objectiveVariables.size > 1) {
            objectiveVariables(1) match {
                case objectiveVar: IntegerVariable =>
                    resultNode += "quality" -> JsNumber(result.bestProposal.value(objectiveVar).value)
                    if (result.isOptimal) {
                        resultNode += "optimal" -> JsBoolean(true)
                    }
                case _ =>
            }
        }
    }

    private def logQualityStepFunction(monitor: ZincTestMonitor): Unit = {
        if (monitor.maybeQualityStepFunction.isDefined) {
            val array =
                monitor.maybeQualityStepFunction.get.flatMap(
                    step => Vector(JsNumber(step.runtimeInMillis),
                                   JsNumber(step.quality.asInstanceOf[IntegerValue].value)))
            resultNode += "quality-step-function" -> JsArray(array.toVector)
        }
    }

    private def logParserStatistics(runtime: Duration): Unit = {
        val statsNode =
            JsSection(
                "runtime-in-seconds" -> JsNumber(runtime.toMillis / 1000.0)
            )
        jsonRoot += "parser-statistics" -> statsNode
    }

    private def logCompilerStatistics(compilerResult: FlatZincCompilerResult): Unit = {
        val statsNode =
            JsSection(
                "runtime-in-seconds" -> JsNumber(compilerResult.runtime.toMillis / 1000.0)
            )
        jsonRoot += "compiler-statistics" -> statsNode
    }

    private def logSearchStatistics(monitor: ZincTestMonitor): Unit = {
        val statsNode =
            if (monitor.wasSearchRequired) {
                JsSection(
                    "number-of-restarts" -> JsNumber(monitor.numberOfRestarts),
                    "moves-per-second" -> JsNumber(monitor.movesPerSecond),
                    "consultations-per-second" -> JsNumber(monitor.consultationsPerSecond),
                    "consultations-per-move" -> JsNumber(monitor.consultationsPerMove),
                    "commitments-per-second" -> JsNumber(monitor.commitmentsPerSecond),
                    "commitments-per-move" -> JsNumber(monitor.commitmentsPerMove)
                )
            } else {
                JsSection()
            }
        if (monitor.maybeRuntimeToFirstSolutionInSeconds.isDefined) {
            statsNode += "runtime-to-first-solution-in-seconds" -> JsNumber(monitor.maybeRuntimeToFirstSolutionInSeconds.get)
        }
        if (monitor.maybeRuntimeToBestSolutionInSeconds.isDefined) {
            statsNode += "runtime-to-best-solution-in-seconds" -> JsNumber(monitor.maybeRuntimeToBestSolutionInSeconds.get)
        }
        statsNode += "runtime-in-seconds" -> JsNumber(monitor.runtimeInSeconds)
        if (monitor.maybeArea.isDefined) {
            statsNode += "area" -> JsNumber(monitor.maybeArea.get)
        }
        jsonRoot += "search-statistics" -> statsNode
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

    private def handleException(task: ZincTestTask, error: Throwable) = {
        def createErrorNode = {
            val node = new JsSection
            node += "type" -> JsString(error.getClass.getName)
            if (error.getMessage.ne(null) && ! error.getMessage.isEmpty) {
                node += "message" -> JsString(error.getMessage)
            }
            node
        }
        def addErrorNode() = {
            resultNode += "error" -> createErrorNode
        }
        def addWarningNode() = {
            resultNode += "warning" -> createErrorNode
        }
        error match {
            case _: FlatZincParserException | _: SolutionNotVerifiedException =>
                addErrorNode()
                nativeLogger.severe(error.getMessage)
                throw error
            case _: UnsupportedFlatZincTypeException | _: VariableWithInfiniteDomainException =>
                addWarningNode()
                nativeLogger.warning(error.getMessage)
                throw error
            case _: InconsistentProblemException =>
                addWarningNode()
                resultNode += "satisfiable" -> JsBoolean(false)
                nativeLogger.warning(error.getMessage)
                nativeLogger.info(FlatZincInconsistentProblemIndicator)
                throw error
            case _: InterruptedException =>
                addWarningNode()
                nativeLogger.warning(error.getMessage)
                nativeLogger.info(FlatZincNoSolutionFoundIndicator)
                assert(error.getMessage, ! task.throwWhenUnsolved)
            case _: Throwable =>
                addErrorNode()
                nativeLogger.log(java.util.logging.Level.SEVERE, "", error)
                throw error
        }
    }

    @tailrec
    private def findUltimateCause(error: Throwable): Throwable =
        if (error.getCause.eq(null)) error else findUltimateCause(error.getCause)

}
