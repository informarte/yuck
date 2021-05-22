package yuck.flatzinc.runner

import java.io.IOException
import java.util.concurrent.CancellationException

import scala.math.max

import scopt._

import yuck.BuildInfo
import yuck.core.InconsistentProblemException
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.{UnsupportedFlatZincTypeException, VariableWithInfiniteDomainException}
import yuck.flatzinc.parser._
import yuck.util.arm._
import yuck.util.logging.YuckLogging

/**
 * @author Michael Marte
 *
 */
object FlatZincRunner extends YuckLogging {

    private case class CommandLine(
        val logLevel: yuck.util.logging.LogLevel = yuck.util.logging.NoLogging,
        val logFilePath: String = "",
        val fznFilePath: String = "",
        val cfg: FlatZincSolverConfiguration =
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
        head("Yuck FlatZinc front end %s, %s build".format(BuildInfo.version, BuildInfo.buildType))
        help("help").abbr("h").text("Show this help message")
        version("version")
        // -a and -f are used by MiniZinc challenge scripts!
        opt[Unit]('a', "print-all-solutions").text("Ignored")
        opt[Unit]('f', "free-search").text("Ignored")
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
        opt[Unit]('v', "verbose")
            .text("Enable verbose solving (equivalent to --log-level INFO)")
            .action((_, cl) => cl.copy(logLevel = List(cl.logLevel, yuck.util.logging.InfoLogLevel).minBy(_.intValue)))
        opt[String]("log-level")
            .text("%s, default value is %s".format(logLevels.mkString("|"), defaultCl.logLevel.toString))
            .action((x, cl) => cl.copy(logLevel = List(cl.logLevel, logLevelMap.getOrElse(x, cl.logLevel)).minBy(_.intValue)))
            .validate(x => if (logLevelMap.contains(x)) success else failure("Unknown log level %s".format(x)))
        opt[String]("log-file-path")
            .text("Optional log file path")
            .action((x, cl) => cl.copy(logFilePath = x))
        arg[String]("FlatZinc file")
            .required()
            .hidden()
            .action((x, cl) => cl.copy(fznFilePath = x))
    }

    def main(args: Array[String]): Unit = {
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
                    logVersion
                    val sigint = new SettableSigint
                    scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); sigint.set()})) {
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
            case error: Throwable => handleError(error)
        }
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

    private def logVersion: Unit = {
        logger.withLogScope("Yuck version") {
            logger.log("Git branch: %s".format(BuildInfo.gitBranch))
            logger.log("Git commit hash: %s".format(BuildInfo.gitCommitHash))
            logger.log("Build type: %s".format(BuildInfo.buildType))
        }
    }

    private def solve(cl: CommandLine, sigint: SettableSigint): Unit = {
        try {
            trySolve(cl, sigint)
        }
        catch {
            case error: CancellationException =>
            case error: InterruptedException =>
            case error: ShutdownInProgressException =>
            case error: Throwable => throw findUltimateCause(error)
        }
    }

    private def trySolve(cl: CommandLine, sigint: SettableSigint): Unit = {
        logger.log("Processing %s".format(cl.fznFilePath))
        val ast =
            logger.withTimedLogScope("Parsing FlatZinc file") {
                new FlatZincFileParser(cl.fznFilePath, logger).call()
            }
        val monitor = new FlatZincSolverMonitor(logger)
        val solverGenerator = new FlatZincSolverGenerator(ast, cl.cfg, sigint, logger, monitor)
        val solver = solverGenerator.call()
        val result = solver.call()
        if (! result.isSolution) {
            println(FlatZincNoSolutionFoundIndicator)
        } else {
            logger.criticalSection {
                logger.withLogScope("Solution") {
                    new FlatZincResultFormatter(result).call().foreach(logger.log(_))
                }
            }
        }
    }

    private def handleError(error: Throwable) = error match {
        case error: java.nio.file.NoSuchFileException =>
            Console.err.println("%s: Directory or file not found".format(error.getFile))
            System.exit(1)
        case error: java.nio.file.AccessDeniedException =>
            Console.err.println("%s: Access denied".format(error.getFile))
            System.exit(1)
        case error: java.nio.file.FileSystemException if error.getReason.ne(null) =>
            Console.err.println("%s: %s".format(error.getFile, error.getReason))
            System.exit(1)
        case error: java.nio.file.FileSystemException =>
            Console.err.println("%s: I/O error".format(error.getFile))
            System.exit(1)
        case error: IOException =>
            Console.err.println("I/O error: %s".format(error.getMessage))
            System.exit(1)
        case error: FlatZincParserException =>
            System.err.println(error.getMessage)
            System.exit(1)
        case error: UnsupportedFlatZincTypeException =>
            System.err.println(error.getMessage)
            System.exit(1)
        case error: VariableWithInfiniteDomainException =>
            System.err.println(error.getMessage)
            System.exit(1)
        case error: InconsistentProblemException =>
            System.err.println(error.getMessage)
            println(FlatZincInconsistentProblemIndicator)
        case error: Throwable =>
            // JVM will print error
            throw error
    }

    private def findUltimateCause(error: Throwable): Throwable =
        if (error.getCause.eq(null)) error else findUltimateCause(error.getCause)

}
