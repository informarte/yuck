package yuck.flatzinc.runner

import java.util.concurrent.{Callable, Executors}

import yuck.flatzinc.ast.FlatZincAst
import yuck.flatzinc.parser.{ByteArrayAsCharSequence, FlatZincParser}
import yuck.util.arm.{ManagedExecutorService, ManagedShutdownHook, scoped}
import yuck.util.logging.LazyLogger

/**
 * Provides FlatZinc parsing with interrupt handling.
 *
 * @author Michael Marte
 */
final class FlatZincFileParser(fznFilePath: String, logger: LazyLogger) extends Callable[FlatZincAst] {

    // Combinator parsing from an InputStreamReader (currently) does not scale
    // (see https://github.com/scala/scala-parser-combinators/issues/64),
    // so we read the whole file into memory and parse from there via a CharSequence.
    // (FlatZinc uses ASCII, so converting bytes to characters is not an issue.)
    override def call() = {
        val file = new java.io.File(fznFilePath)
        val bytes = java.nio.file.Files.readAllBytes(file.toPath)
        val sequence = new ByteArrayAsCharSequence(bytes, 0, bytes.length)
        class FlatZincParserRunner extends Callable[FlatZincAst] {
            override def call() = FlatZincParser.parse(sequence)
        }
        val threadPool = Executors.newFixedThreadPool(1)
        val futureAst = threadPool.submit(new FlatZincParserRunner)
        val ast =
            scoped(new ManagedExecutorService(threadPool, logger)) {
                scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); futureAst.cancel(false)})) {
                    futureAst.get
                }
            }
        logger.withLogScope("FlatZinc model statistics") {
            logFlatZincModelStatistics(ast)
        }
        ast
    }

    private def logFlatZincModelStatistics(ast: FlatZincAst) = {
        logger.log("%d predicate declarations".format(ast.predDecls.size))
        logger.log("%d parameter declarations".format(ast.paramDecls.size))
        logger.log("%d variable declarations".format(ast.varDecls.size))
        logger.log("%d constraints".format(ast.constraints.size))
    }

}
