package yuck.flatzinc.runner

import java.io.IOException
import java.util.concurrent.CancellationException

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math.max

import scopt.*
import spray.json.JsBoolean

import yuck.BuildInfo
import yuck.annealing.*
import yuck.core.profiling.SpaceProfilingMode
import yuck.core.{CyclicConstraintNetworkException, InconsistentProblemException}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.{UnsupportedFlatZincTypeException, VariableWithInfiniteDomainException}
import yuck.flatzinc.parser.*
import yuck.flatzinc.util.SummaryBuilder
import yuck.util.arm.*
import yuck.util.logging.YuckLogging
import yuck.util.logging.LogLevel.InfoLogLevel

/**
 * @author Michael Marte
 *
 */
object FlatZincRunner extends YuckLogging {

    private case class CommandLine(
        logLevel: yuck.util.logging.LogLevel = yuck.util.logging.LogLevel.NoLogging,
        logFilePath: String = "",
        summaryFilePath: String = "",
        printIntermediateSolutions: Boolean = false,
        fznFilePath: String = "",
        cfg: FlatZincSolverConfiguration =
            FlatZincSolverConfiguration(
                numberOfThreads = 1,
                // The parser expects the following values to be undefined!
                maybeRoundLimit = None,
                maybeRuntimeLimitInSeconds = None,
                maybeTargetObjectiveValue = None))
    {}

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
        opt[Int]('p', "number-of-threads")
            .text("Default value is %s".format(defaultCfg.numberOfThreads))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(numberOfThreads = max(1, x))))
        opt[Long]('r', "seed")
            .text("Default value is %s".format(defaultCfg.seed))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(seed = x)))
        opt[Int]("restart-limit")
            .text("Default value is %s".format(defaultCfg.restartLimit))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(restartLimit = max(0, x))))
        opt[Int]("target-objective-value")
            .text("Optional stopping criterion in terms of an objective value")
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(maybeTargetObjectiveValue = Some(x))))
        opt[Int]("round-limit")
            .text("Optional round limit for simulated annealing")
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(maybeRoundLimit = Some(max(0, x)))))
        opt[Int]("runtime-limit")
            .text("Optional runtime limit in seconds")
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(maybeRuntimeLimitInSeconds = Some(max(0, x)))))
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
            .text("Default value is %s".format(defaultCfg.useImplicitSolving))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(useImplicitSolving = x)))
        opt[Boolean]("use-progressive-tightening")
            .text("Default value is %s".format(defaultCfg.useProgressiveTightening))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(useProgressiveTightening = x)))
        opt[Boolean]("delay-cycle-checking-until-initialization")
            .text("Default value is %s".format(defaultCfg.delayCycleCheckingUntilInitialization))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(delayCycleCheckingUntilInitialization = x)))
        opt[Unit]('v', "verbose")
            .text("Enable verbose solving (equivalent to --log-level INFO)")
            .action((_, cl) => cl.copy(logLevel = List(cl.logLevel, yuck.util.logging.LogLevel.InfoLogLevel).minBy(_.intValue)))
        opt[String]("log-level")
            .text("%s, default value is %s".format(logLevels.mkString("|"), defaultCl.logLevel.toString))
            .action((x, cl) => cl.copy(logLevel = List(cl.logLevel, logLevelMap.getOrElse(x, cl.logLevel)).minBy(_.intValue)))
            .validate(x => if (logLevelMap.contains(x)) success else failure("Unknown log level %s".format(x)))
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
        val parser = new CommandLineParser
        val maybeCl = parser.parse(args, new CommandLine)
        if (maybeCl.isEmpty) {
            System.exit(1)
        }
        val cl = maybeCl.get
        setupLogging(cl)
        summaryBuilder.addOsEnv()
        summaryBuilder.addJavaEnv()
        summaryBuilder.addYuckVersion()
        summaryBuilder.addSolverConfiguration(cl.cfg)
        val exitCode = scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); sigint.set()})) {
            val exitCode = maybeTimeboxed(cl.cfg.maybeRuntimeLimitInSeconds, sigint, "solver", logger) {
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
        if (cl.logFilePath.isEmpty) {
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
            if (! cl.summaryFilePath.isEmpty) {
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
        summaryBuilder.addParserStatistics(parserRuntime)
        val md5Sum = SummaryBuilder.computeMd5Sum(cl.fznFilePath)
        summaryBuilder.addFlatZincModelStatistics(ast, md5Sum)
        val statisticsCollector = new AnnealingStatisticsCollector(logger)
        val monitors = new ArrayBuffer[AnnealingMonitor]
        monitors += new AnnealingEventLogger(logger)
        monitors += statisticsCollector
        if (cl.printIntermediateSolutions) {
            monitors += new FlatZincResultPrinter(ast, logger)
        }
        val monitor = new AnnealingMonitorCollection(monitors.toVector)
        val (result, _) = logger.withTimedLogScope("Solving problem") {
            scoped(monitor) {
                new FlatZincSolverGenerator(ast, cl.cfg, sigint, logger, monitor).call().call()
            }
        }
        if (result.isSolution) {
            val outputLines = new FlatZincResultFormatter(ast)(result)
            if (! cl.printIntermediateSolutions) {
                outputLines.foreach(println)
            }
            logger.withLogScope("Solution") {
                outputLines.foreach(logger.log(_))
            }
        } else {
            println(FlatZincNoSolutionFoundIndicator)
        }
        summaryBuilder.addYuckModelStatistics(result.space)
        summaryBuilder.addResult(result)
        summaryBuilder.addSearchStatistics(statisticsCollector)
        if (cl.cfg.maybeSpaceProfilingMode.isDefined) {
            summaryBuilder.addSpacePerformanceMetrics(result.space.performanceMetricsBuilder.build())
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
        if (throwable.getCause.eq(null)) throwable else findUltimateCause(throwable.getCause)

}
