package yuck.flatzinc.runner

import java.io.IOException
import java.util.concurrent.CancellationException
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math.max

import scopt.*
import spray.json.JsBoolean

import yuck.annealing.*
import yuck.core.profiling.SpaceProfilingMode
import yuck.core.{Costs, CyclicConstraintNetworkException, InconsistentProblemException, SharedBound, SolverMonitoring}
import yuck.fj.FeasibilityJumpEventLogger
import yuck.flatzinc.compiler.{FlatZincCompilerResult, UnsupportedFlatZincTypeException, VariableWithInfiniteDomainException}
import yuck.flatzinc.parser.*
import yuck.flatzinc.util.*
import yuck.flatzinc.{AnnealingConfiguration, FlatZincSolverConfiguration}
import yuck.util.arm.*
import yuck.util.logging.YuckLogging
import yuck.{BuildInfo, SolvingMethod}

object FlatZincRunner extends YuckLogging {

    private case class CommandLine(
        logLevel: yuck.util.logging.LogLevel = yuck.util.logging.LogLevel.NoLogging,
        logFilePath: String = "",
        summaryFilePath: String = "",
        printIntermediateSolutions: Boolean = false,
        outputThrottlingIntervalInMillis: Int = 1000,
        fznFilePath: String = "",
        cfg: FlatZincSolverConfiguration =
            FlatZincSolverConfiguration(
                // The parser expects the following values to be undefined!
                maybePreferredSolvingMethod = None,
                annealingConfiguration = AnnealingConfiguration(maybeRoundLimit = None),
                maybeRuntimeLimitInSeconds = None,
                maybeTargetObjectiveValue = None))
    {}

    given Read[SolvingMethod] = Read.reads { str =>
        try SolvingMethod.fromAbbreviation(str)
        catch case _: IllegalArgumentException =>
            throw new IllegalArgumentException(
                "Invalid solving method %s, use one of {%s}".format(str, SolvingMethod.values.mkString(", ")))
    }

    private class CommandLineParser extends OptionParser[CommandLine]("yuck") {
        val defaultCl = CommandLine()
        val defaultCfg = defaultCl.cfg
        val logLevels = yuck.util.logging.logLevels
        val logLevelMap = logLevels.map(level => (level.toString -> level)).toMap
        head("Yuck FlatZinc front end %s".format(BuildInfo.version))
        help("help").abbr("h").text("Show this help message")
        version("version")
        // -a and -f are used by MiniZinc challenge scripts!
        opt[Unit]('a', "all-solutions")
            .text("Equivalent to -i")
            .action((_, cl) => cl.copy(printIntermediateSolutions = true))
        opt[Unit]('f', "free-search")
            .text("Ignored")
        opt[Unit]('i', "intermediate-solutions")
            .action((_, cl) => cl.copy(printIntermediateSolutions = true))
        opt[Int]("output-throttling-interval")
            .text("Output-throttling interval in milliseconds, default value is %s, 0 implies no throttling"
                .format(defaultCl.outputThrottlingIntervalInMillis))
            .action((x, cl) => cl.copy(outputThrottlingIntervalInMillis = x))
        opt[Int]('p', "number-of-solvers")
            .text("Default value is %s".format(defaultCfg.numberOfSolvers))
            .action((x, cl) => cl.copy(
                cfg = cl.cfg.copy(numberOfThreads = max(1, x), numberOfSolvers = max(1, x))))
        opt[Long]('r', "seed")
            .text("Default value is %s".format(defaultCfg.seed))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(seed = x)))
        opt[Int]("target-objective-value")
            .text("Optional stopping criterion in terms of an objective value")
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(maybeTargetObjectiveValue = Some(x))))
        opt[Boolean]("optimize-array-access")
            .text("Default value is %s".format(defaultCfg.optimizeArrayAccess))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(optimizeArrayAccess = x)))
        opt[Boolean]("prune-constraint-network")
            .text("Default value is %s".format(defaultCfg.pruneConstraintNetwork))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(pruneConstraintNetwork = x)))
        opt[Boolean]("run-presolver")
            .text("Default value is %s".format(defaultCfg.runPresolver))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(runPresolver = x)))
        opt[Boolean]("use-implicit-solving")
            .text("Default value is %s".format(defaultCfg.annealingConfiguration.useImplicitSolving))
            .action((x, cl) => cl.copy(
                cfg = cl.cfg.copy(
                    annealingConfiguration =
                        cl.cfg.annealingConfiguration.copy(useImplicitSolving = x))))
        opt[Boolean]("use-progressive-tightening")
            .text("Default value is %s".format(defaultCfg.useProgressiveTightening))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(useProgressiveTightening = x)))
        opt[Boolean]("share-bounds")
            .text("Default value is %s".format(defaultCfg.shareBounds))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(shareBounds = x)))
        opt[SolvingMethod]("solving-method")
            .text("Optional solving method (%s)".format(SolvingMethod.values.mkString("|")))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(maybePreferredSolvingMethod = Some(x))))
        opt[Double]("start-temperature")
            .text("Default value is %s".format(defaultCfg.annealingConfiguration.startTemperature))
            .action((x, cl) => cl.copy(
                cfg = cl.cfg.copy(
                    annealingConfiguration =
                        cl.cfg.annealingConfiguration.copy(startTemperature = x))))
        opt[Double]("warm-start-temperature")
            .text("Default value is %s".format(defaultCfg.annealingConfiguration.warmStartTemperature))
            .action((x, cl) => cl.copy(
                cfg = cl.cfg.copy(
                    annealingConfiguration =
                        cl.cfg.annealingConfiguration.copy(warmStartTemperature = x))))
        opt[Int]("round-limit")
            .text("Optional round limit for simulated annealing")
            .action((x, cl) => cl.copy(
                cfg = cl.cfg.copy(
                    annealingConfiguration =
                        cl.cfg.annealingConfiguration.copy(maybeRoundLimit = Some(max(0, x))))))
        opt[Int]("runtime-limit")
            .text("Optional runtime limit in seconds")
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(maybeRuntimeLimitInSeconds = Some(max(0, x)))))
        opt[Unit]('v', "verbose")
            .text("Enable verbose solving (equivalent to --log-level INFO)")
            .action((_, cl) => cl.copy(logLevel = List(cl.logLevel, yuck.util.logging.LogLevel.InfoLogLevel).minBy(_.intValue)))
        opt[String]("log-level")
            .text("%s, default value is %s".format(logLevels.mkString("|"), defaultCl.logLevel.toString))
            .action((x, cl) => cl.copy(logLevel = List(cl.logLevel, logLevelMap.getOrElse(x, cl.logLevel)).minBy(_.intValue)))
            .validate(x => if logLevelMap.contains(x) then success else failure("Unknown log level %s".format(x)))
        opt[String]("log-file-path")
            .text("Optional log file path")
            .action((x, cl) => cl.copy(logFilePath = x))
        opt[String]("summary-file-path")
            .text("Optional summary file path")
            .action((x, cl) => cl.copy(summaryFilePath = x))
        opt[Boolean]("constraint-profiling")
            .text("Profile constraints and write results into summary file")
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(maybeSpaceProfilingMode = Some(SpaceProfilingMode.ByConstraint))))
        opt[Boolean]("goal-profiling")
            .text("Profile goals and write results into summary file")
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(maybeSpaceProfilingMode = Some(SpaceProfilingMode.ByGoal))))
        arg[String]("FlatZinc file")
            .required()
            .hidden()
            .action((x, cl) => cl.copy(fznFilePath = x))
    }

    private val sigint = new SettableSigint

    private val summaryBuilder = new SummaryBuilder

    def main(args: Array[String]): Unit = {
        Thread.currentThread.setName("Yuck")
        val parser = new CommandLineParser
        val maybeCl = parser.parse(args, new CommandLine)
        if maybeCl.isEmpty then {
            System.exit(1)
        }
        val cl = maybeCl.get
        setupLogging(cl)
        summaryBuilder.addOsEnv()
        summaryBuilder.addJavaEnv()
        summaryBuilder.addYuckVersion()
        summaryBuilder.addSolverConfiguration(cl.cfg)
        val exitCode = scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); sigint.set()})) {
            val maybeRuntimeLimitInMillis =
                cl.cfg.maybeRuntimeLimitInSeconds.map(seconds => new AtomicLong(seconds * 1000))
            val exitCode = maybeTimeboxed(maybeRuntimeLimitInMillis, sigint, logger) {
                solve(cl)
            }
            logger.log("Shutdown complete, exiting")
            exitCode
        }
        System.exit(exitCode)
    }

    private def setupLogging(cl: CommandLine): Unit = {
        try {
            trySetupLogging(cl)
        }
        catch {
            case error: Throwable => throw findUltimateCause(error)
        }
    }

    private def trySetupLogging(cl: CommandLine): Unit = {
        nativeLogger.setUseParentHandlers(false); // otherwise our console handler would remain unused
        val formatter = new yuck.util.logging.Formatter
        if cl.logFilePath.isEmpty then {
            val consoleHandler = new java.util.logging.ConsoleHandler
            consoleHandler.setFormatter(formatter)
            nativeLogger.addHandler(consoleHandler)
        } else {
            val logFileHandler = new java.util.logging.FileHandler(cl.logFilePath)
            logFileHandler.setFormatter(formatter)
            nativeLogger.addHandler(logFileHandler)
        }
        logger.setThresholdLogLevel(cl.logLevel)
    }

    private def solve(cl: CommandLine): Int = {
        var exitCode = 0
        try {
            trySolve(cl)
        }
        catch {
            case _: CancellationException =>
            case _: InterruptedException =>
            case _: ShutdownInProgressException =>
            case throwable: Throwable => exitCode = handleException(findUltimateCause(throwable))
        }
        finally {
            if ! cl.summaryFilePath.isEmpty then {
                logger.withLogScope("Writing %s".format(cl.summaryFilePath)) {
                    val jsonDoc = summaryBuilder.build()
                    val jsonWriter = new java.io.FileWriter(cl.summaryFilePath)
                    jsonWriter.write(jsonDoc.prettyPrint)
                    jsonWriter.close()
                }
            }
        }
        exitCode
    }

    private def trySolve(cl: CommandLine): Unit = {
        logger.log("Processing %s".format(cl.fznFilePath))
        val (ast, parserRuntime) =
            logger.withTimedLogScope("Parsing FlatZinc file") {
                new FlatZincParser(cl.fznFilePath, logger).call()
            }
        summaryBuilder.addParserMetrics(parserRuntime)
        val md5Sum = SummaryBuilder.computeMd5Sum(cl.fznFilePath)
        summaryBuilder.addFlatZincModelMetrics(ast, md5Sum)
        val monitors = new ArrayBuffer[SolverMonitoring[?]]
        if cl.logLevel != yuck.util.logging.LogLevel.NoLogging then {
            monitors += new AnnealingEventLogger(logger)
            monitors += new FeasibilityJumpEventLogger(logger)
            monitors += new BestProposalLogger(logger)
        }
        val metricsCollector = new LocalSearchMetricsCollector(logger)
        monitors += metricsCollector
        val resultPrinter = new FlatZincResultPrinter(ast, cl.outputThrottlingIntervalInMillis)
        if cl.printIntermediateSolutions then {
            monitors += resultPrinter
        }
        val sharedBoundHolder = new AtomicReference[Costs]
        if cl.cfg.shareBounds then {
            monitors += new SharedBoundMaintainer(sharedBoundHolder)
        }
        val monitor = new PortfolioSolverMonitor(monitors.toVector)
        val (result, _) = logger.withTimedLogScope("Solving problem") {
            val resultPrinterThread = new Thread(resultPrinter, resultPrinter.getClass.getSimpleName)
            scoped(new ManagedThread(resultPrinterThread, logger)) {
                scoped(monitor) {
                    val sharedBound = new SharedBound(sharedBoundHolder)
                    new FlatZincSolverGenerator(ast, cl.cfg, sharedBound, monitor, logger, sigint).call().call()
                }
            }
        }
        if result.isSolution then {
            val outputLines = new FlatZincResultFormatter(ast)(new FlatZincResult(result))
            if cl.printIntermediateSolutions then {
                resultPrinter.flush()
            } else {
                outputLines.foreach(println)
            }
            logger.withLogScope("Solution") {
                outputLines.foreach(logger.log(_))
            }
        } else {
            println(FlatZincNoSolutionFoundIndicator)
        }
        val space = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult].space
        summaryBuilder.addYuckModelMetrics(space)
        summaryBuilder.addResult(result)
        summaryBuilder.addSearchMetrics(metricsCollector, None)
        if cl.cfg.maybeSpaceProfilingMode.isDefined then {
            summaryBuilder.addSpacePerformanceMetrics(space.performanceMetricsBuilder.build())
        }
    }

    private def handleException(throwable: Throwable): Int = throwable match {
        case error: java.nio.file.NoSuchFileException =>
            Console.err.println("%s: Directory or file not found".format(error.getFile))
            1
        case error: java.nio.file.AccessDeniedException =>
            Console.err.println("%s: Access denied".format(error.getFile))
            1
        case error: java.nio.file.FileSystemException if error.getReason.ne(null) =>
            Console.err.println("%s: %s".format(error.getFile, error.getReason))
            1
        case error: java.nio.file.FileSystemException =>
            Console.err.println("%s: I/O error".format(error.getFile))
            1
        case _: IOException =>
            Console.err.println("I/O error: %s".format(throwable.getMessage))
            1
        case _: FlatZincParserException =>
            summaryBuilder.addError(throwable)
            logger.log(throwable.getMessage)
            System.err.println(throwable.getMessage)
            1
        case _: UnsupportedFlatZincTypeException | _: VariableWithInfiniteDomainException =>
            summaryBuilder.addWarning(throwable)
            logger.log(throwable.getMessage)
            System.err.println(throwable.getMessage)
            1
        case _: InconsistentProblemException =>
            summaryBuilder.extendResult("satisfiable", JsBoolean(false))
            logger.log(throwable.getMessage)
            logger.log(FlatZincInconsistentProblemIndicator)
            System.err.println(throwable.getMessage)
            println(FlatZincInconsistentProblemIndicator)
            0
        case _: CyclicConstraintNetworkException =>
            summaryBuilder.addError(throwable)
            logger.log(throwable.getMessage)
            System.err.println(throwable.getMessage)
            1
        case _: Throwable =>
            summaryBuilder.addError(throwable)
            logger.withLogScope(throwable.getMessage) {
                throwable.getStackTrace.foreach(frame => logger.log(frame.toString))
            }
            1
    }

    @tailrec
    private def findUltimateCause(throwable: Throwable): Throwable =
        if throwable.getCause.eq(null) then throwable else findUltimateCause(throwable.getCause)

}
