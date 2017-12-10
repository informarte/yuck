package yuck.flatzinc.runner

import java.io.IOException
import java.util.concurrent.CancellationException

import scala.math.max

import scopt._

import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.{InconsistentProblemException, UnsupportedFlatZincTypeException, VariableWithInfiniteDomainException}
import yuck.flatzinc.parser._
import yuck.util.arm._


/**
 * @author Michael Marte
 *
 */
object FlatZincRunner {

    System.setProperty("java.util.logging.manager", classOf[yuck.util.logging.ManagedLogManager].getName)
    val logManager = java.util.logging.LogManager.getLogManager.asInstanceOf[yuck.util.logging.ManagedLogManager]
    val nativeLogger = java.util.logging.Logger.getLogger(this.getClass.getName)
    val logger = new yuck.util.logging.LazyLogger(nativeLogger)

    case class CommandLine(
        val logLevel: yuck.util.logging.LogLevel = yuck.util.logging.NoLogging,
        val logFilePath: String = "",
        val fznFilePath: String = "",
        val cfg: FlatZincSolverConfiguration =
            new FlatZincSolverConfiguration(
                // The parser expects the following values to be undefined!
                maybeRoundLimit = None,
                maybeRuntimeLimitInSeconds = None,
                maybeTargetObjectiveValue = None))
    {}

    class CommandLineParser extends OptionParser[CommandLine]("yuck") {
        val defaultCl = new CommandLine
        val defaultCfg = defaultCl.cfg
        val logLevels = yuck.util.logging.logLevels
        val logLevelMap = logLevels.map(level => (level.toString -> level)).toMap
        head("Yuck FlatZinc frontend")
        help("help").abbr("h").text("Show this help message")
        opt[Unit]('a', "print-all-solutions").text("Ignored")
        opt[Unit]('f', "ignore-search-strategy").text("Ignored")
        opt[Int]('p', "number-of-virtual-cores")
            .text("Default value is %s".format(defaultCfg.numberOfVirtualCores))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(numberOfVirtualCores = max(1, x))))
        opt[Int]('r', "seed")
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
        opt[Boolean]("use-progressive-tightening")
            .text("Default value is %s".format(defaultCfg.useProgressiveTightening))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(useProgressiveTightening = x)))
        opt[Boolean]("prefer-implicit-solving-over-domain-pruning")
            .text("Default value is %s".format(defaultCfg.preferImplicitSolvingOverDomainPruning))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(preferImplicitSolvingOverDomainPruning = x)))
        opt[String]("log-level")
            .text("%s, default value is %s".format(logLevels.mkString("|"), defaultCl.logLevel.toString))
            .action((x, cl) => cl.copy(logLevel = logLevelMap.getOrElse(x, cl.logLevel)))
            .validate(x => if (logLevelMap.contains(x)) success else failure("Unknown log level %s".format(x)))
        opt[String]("log-file-path")
            .text("Optional log file path")
            .action((x, cl) => cl.copy(logFilePath = x))
        arg[String]("FlatZinc file")
            .required()
            .hidden()
            .action((x, cl) => cl.copy(fznFilePath = x))
        override def usageExample =
            "%s <JVM option>* -- <Yuck option>* <FlatZinc file>".format(programName)
    }

    def main(args: Array[String]) {
        val parser = new CommandLineParser
        val maybeCl = parser.parse(args, new CommandLine)
        if (maybeCl.isEmpty) {
            System.exit(1)
        }
        val cl = maybeCl.get
        try {
            // We use an empty, managed shutdown hook to enforce the completion of a shutdown
            // initiated upon interrupt.
            // (Without it, the JVM would already exit when the inner, managed shutdown hook
            //  goes out of scope.)
            scoped(new ManagedShutdownHook({})) {
                scoped(logManager) {
                    setupLogging(cl)
                    val sigint = new SettableSigint
                    scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); sigint.set})) {
                        maybeTimeboxed(cl.cfg.maybeRuntimeLimitInSeconds, sigint, "solver", logger) {
                            solve(cl, sigint)
                        }
                    }
                    logger.log("Shutdown complete, exiting")
                }
            }
        }
        catch {
            case error: ShutdownInProgressException =>
        }
    }

    private def setupLogging(cl: CommandLine) {
        try {
            trySetupLogging(cl)
        }
        catch {
            case error: Throwable =>
                reportLoggingProblem(error)
                System.exit(1)
        }
    }

    private def trySetupLogging(cl: CommandLine) {
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

    private def reportLoggingProblem(error: Throwable) = error match {
        case error: java.nio.file.NoSuchFileException =>
            Console.err.println("%s: Directory or file not found".format(error.getFile))
        case error: java.nio.file.AccessDeniedException =>
            Console.err.println("%s: Access denied".format(error.getFile))
        case error: java.nio.file.FileSystemException if error.getReason != null =>
            Console.err.println("%s: %s".format(error.getFile, error.getReason))
        case error: java.nio.file.FileSystemException =>
            Console.err.println("%s: I/O error".format(error.getFile))
        case error: IOException =>
            Console.err.println("I/O error: %s".format(error.getMessage))
        case error: Throwable =>
            // JVM will print error
            throw error
    }

    private def solve(cl: CommandLine, sigint: SettableSigint): Unit = {
        try {
            trySolve(cl, sigint)
        }
        catch {
            case error: CancellationException =>
            case error: InterruptedException =>
            case error: ShutdownInProgressException =>
            case error: Throwable => reportSolverError(findUltimateCause(error))
        }
    }

    private def trySolve(cl: CommandLine, sigint: SettableSigint) {
        logger.log("Processing %s".format(cl.fznFilePath))
        val ast =
            logger.withTimedLogScope("Parsing FlatZinc file") {
                new FlatZincFileParser(cl.fznFilePath, logger).call
            }
        val monitor = new FlatZincSolverMonitor(logger)
        val solverGenerator = new FlatZincSolverGenerator(ast, cl.cfg, sigint, logger, monitor)
        val solver = solverGenerator.call
        val result = solver.call
        if (! result.isSolution) {
            println(FLATZINC_NO_SOLUTION_FOUND_INDICATOR)
        } else {
            logger.criticalSection {
                logger.withLogScope("Solution") {
                    new FlatZincResultFormatter(result).call.foreach(logger.log(_))
                }
            }
        }
    }

    private def reportSolverError(error: Throwable) = error match {
        case error: IOException =>
            nativeLogger.severe(error.getMessage)
        case error: FlatZincParserException =>
            nativeLogger.severe(error.getMessage)
        case error: UnsupportedFlatZincTypeException =>
            nativeLogger.severe(error.getMessage)
        case error: VariableWithInfiniteDomainException =>
            nativeLogger.severe(error.getMessage)
        case error: InconsistentProblemException =>
            nativeLogger.fine(error.getMessage)
            println(FLATZINC_INCONSISTENT_PROBLEM_INDICATOR)
        case error: Throwable =>
            nativeLogger.log(java.util.logging.Level.SEVERE, "", error)
            // JVM will print error
            throw error
    }

    private def findUltimateCause(error: Throwable): Throwable =
        if (error.getCause == null) error else findUltimateCause(error.getCause)

}
