package yuck.flatzinc.runner

import java.io.IOException

import scala.math.max

import scopt._

import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.InconsistentProblemException
import yuck.flatzinc.parser._
import yuck.flatzinc.compiler.VariableWithInfiniteDomainException

object FlatZincRunner {

    val nativeLogger = java.util.logging.Logger.getLogger(this.getClass.getName)
    val logger = new yuck.util.logging.LazyLogger(nativeLogger)
    val defaultCfg = new FlatZincSolverConfiguration(maybeRuntimeLimitInSeconds = None)

    class CommandLineParser extends OptionParser[FlatZincSolverConfiguration]("yuck") {
        var flatZincFilePath = ""
        head("Yuck FlatZinc frontend")
        help("help").abbr("h").text("Show this help message")
        opt[Int]('p', "number-of-virtual-cores")
            .text("Default value is %s".format(defaultCfg.numberOfVirtualCores))
            .action((x, cfg) => cfg.copy(numberOfVirtualCores = max(1, x)))
        opt[Int]('r', "seed")
            .text("Default value is %s".format(defaultCfg.seed))
            .action((x, cfg) => cfg.copy(seed = x))
        opt[Int]("restart-limit")
            .text("Default value is %s".format(defaultCfg.restartLimit))
            .action((x, cfg) => cfg.copy(restartLimit = max(1, x)))
        opt[Int]("target-objective-value")
            .text("Optional stopping criterion in terms of an objective value")
            .action((x, cfg) => cfg.copy(maybeTargetObjectiveValue = Some(x)))
        opt[Int]("round-limit")
            .text("Optional round limit for simulated annealing")
            .action((x, cfg) => cfg.copy(maybeRoundLimit = Some(max(0, x))))
        opt[Int]("runtime-limit")
            .text("Optional runtime limit in seconds")
            .action((x, cfg) => cfg.copy(maybeRuntimeLimitInSeconds = Some(max(0, x))))
        opt[Boolean]("use-progressive-tightening")
            .text("Default value is %s".format(defaultCfg.useProgressiveTightening))
            .action((x, cfg) => cfg.copy(useProgressiveTightening = x))
        arg[String]("FlatZinc file")
            .required()
            .hidden()
            .action((x, cfg) => {flatZincFilePath = x; cfg})
        override def usageExample =
            "%s <JVM option>* -- <Yuck option>* <FlatZinc file>".format(programName)
    }

    def main(args: Array[String]) {
        val parser = new CommandLineParser
        val maybeCfg = parser.parse(args, new FlatZincSolverConfiguration)
        if (maybeCfg.isEmpty) {
            System.exit(1)
        }
        val cfg = maybeCfg.get
        val problemPath = parser.flatZincFilePath
        java.util.logging.LogManager.getLogManager.reset // remove handlers
        val consoleHandler = new java.util.logging.ConsoleHandler() {
            override def publish(record: java.util.logging.LogRecord) {
                synchronized {
                    super.publish(record)
                    flush
                }
            }
        }
        consoleHandler.setFormatter(new java.util.logging.Formatter {
            override def format(logRecord: java.util.logging.LogRecord) =
                "%s\n".format(logRecord.getMessage)
        })
        nativeLogger.addHandler(consoleHandler)
        logger.setThresholdLogLevel(yuck.util.logging.NoLogging)
        try {
            trySolve(problemPath, cfg)
        }
        catch {
            case error: Throwable => handleException(findUltimateCause(error))
        }
    }

    def trySolve(problemPath: String, cfg: FlatZincSolverConfiguration) {
        val file = new java.io.File(problemPath)
        val reader = new java.io.InputStreamReader(new java.io.FileInputStream(file))
        val ast = FlatZincParser.parse(reader)
        val monitor = new FlatZincSolverMonitor
        val solver = new FlatZincSolverGenerator(ast, cfg, logger, monitor).call
        val maybeResult = solver.call
        if (maybeResult.isEmpty || ! maybeResult.get.isSolution) {
            println(FLATZINC_NO_SOLUTION_FOUND_INDICATOR)
        }
    }

    private def handleException(error: Throwable) = error match {
        case error: IOException =>
            nativeLogger.severe(error.getMessage)
        case error: FlatZincParserException =>
            nativeLogger.severe(error.getMessage)
        case error: VariableWithInfiniteDomainException =>
            nativeLogger.severe(error.getMessage)
        case error: InconsistentProblemException =>
            nativeLogger.fine(error.getMessage)
            println(FLATZINC_INCONSISTENT_PROBLEM_INDICATOR)
        case error: Throwable =>
            // JVM will print error
            throw error
    }

    private def findUltimateCause(error: Throwable): Throwable =
        if (error.getCause == null) error else findUltimateCause(error.getCause)

}
