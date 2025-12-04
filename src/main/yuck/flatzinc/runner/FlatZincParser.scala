package yuck.flatzinc.runner

import java.util.concurrent.{Callable, Executors}

import yuck.flatzinc.ast.FlatZincAst
import yuck.util.arm.{ManagedExecutorService, ManagedShutdownHook, scoped}
import yuck.util.logging.LazyLogger

/**
 * Provides FlatZinc parsing with interrupt handling.
 */
final class FlatZincParser(fznFilePath: String, logger: LazyLogger) extends Callable[FlatZincAst] {

    // https://com-lihaoyi.github.io/fastparse/#StreamingParsingLimitations mentions that parsing
    // from a string is faster than from a stream, so we read the whole file into memory.
    // (FlatZinc uses ASCII, so converting bytes to characters is not an issue.)
    override def call() = {
        val file = new java.io.File(fznFilePath)
        val input = new String(java.nio.file.Files.readAllBytes(file.toPath))
        class FlatZincParserRunner extends Callable[FlatZincAst] {
            override def call() = yuck.flatzinc.parser.FlatZincParser.parse(input)
        }
        val threadPool = Executors.newFixedThreadPool(1)
        val futureAst = threadPool.submit(new FlatZincParserRunner)
        val ast =
            scoped(new ManagedExecutorService(threadPool, logger)) {
                scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); futureAst.cancel(false)})) {
                    futureAst.get
                }
            }
        logger.withLogScope("FlatZinc model metrics") {
            logFlatZincModelMetrics(ast)
        }
        ast
    }

    private def logFlatZincModelMetrics(ast: FlatZincAst) = {
        logger.log("%d predicate declarations".format(ast.predDecls.size))
        logger.log("%d parameter declarations".format(ast.paramDecls.size))
        logger.log("%d variable declarations".format(ast.varDecls.size))
        logger.log("%d constraints".format(ast.constraints.size))
    }

}
