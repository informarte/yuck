package yuck.flatzinc.runner

import java.io.IOException

import scala.math.max
import scopt._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.{InconsistentProblemException, UnsupportedFlatZincTypeException, VariableWithInfiniteDomainException}
import yuck.flatzinc.parser._

object FlatZincRunner {

    val nativeLogger = java.util.logging.Logger.getLogger(this.getClass.getName)
    val logger = new yuck.util.logging.LazyLogger(nativeLogger)

    case class CommandLine(
        val logLevel: yuck.util.logging.LogLevel = yuck.util.logging.NoLogging,
        val logFilePath: String = "",
        val problemPath: String = "",
        val cfg: FlatZincSolverConfiguration = new FlatZincSolverConfiguration)
    {}

    class CommandLineParser extends OptionParser[CommandLine]("yuck") {
        val defaultCl = new CommandLine
        val defaultCfg = defaultCl.cfg
        val logLevels = yuck.util.logging.logLevels
        val logLevelMap = logLevels.map(level => (level.toString -> level)).toMap
        head("Yuck FlatZinc frontend")
        help("help").abbr("h").text("Show this help message")
        opt[Int]('p', "number-of-virtual-cores")
            .text("Default value is %s".format(defaultCfg.numberOfVirtualCores))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(numberOfVirtualCores = max(1, x))))
        opt[Int]('r', "seed")
            .text("Default value is %s".format(defaultCfg.seed))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(seed = x)))
        opt[Int]("restart-limit")
            .text("Default value is %s".format(defaultCfg.restartLimit))
            .action((x, cl) => cl.copy(cfg = cl.cfg.copy(restartLimit = max(1, x))))
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
            .action((x, cl) => cl.copy(problemPath = x))
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
        setupLogging(cl)
        solve(cl)
    }

    private def setupLogging(cl: CommandLine): Unit = {
        try {
            trySetup(cl)
        }
        catch {
            case error: Throwable =>
                reportLoggingProblem(error)
                System.exit(1)
        }
    }

    private def trySetup(cl: CommandLine) {
        java.util.logging.LogManager.getLogManager.reset // remove handlers
        val formatter = new yuck.util.logging.Formatter
        if (cl.logFilePath.isEmpty) {
            val consoleHandler = new java.util.logging.ConsoleHandler {
                override def publish(record: java.util.logging.LogRecord) {
                    synchronized {
                        super.publish(record)
                        flush
                    }
                }
            }
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

    private def solve(cl: CommandLine): Unit = {
        try {
            trySolve(cl)
        }
        catch {
            case error: Throwable => reportSolverError(findUltimateCause(error))
        }
    }

    private def trySolve(cl: CommandLine) {
        logger.log("Processing %s".format(cl.problemPath))
        val file = new java.io.File(cl.problemPath)
        val reader = new java.io.InputStreamReader(new java.io.FileInputStream(file))
        val ast = FlatZincParser.parse(reader)
        val monitor = new FlatZincSolverMonitor(logger)
        val solver = new FlatZincSolverGenerator(ast, cl.cfg, logger, monitor).call
        val maybeResult = solver.call
        if (maybeResult.isEmpty || ! maybeResult.get.isSolution) {
            println(FLATZINC_NO_SOLUTION_FOUND_INDICATOR)
        } else {
            logger.criticalSection {
                logger.withLogScope("Solution") {
                    new FlatZincResultFormatter(maybeResult.get).call.foreach(logger.log(_))
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
