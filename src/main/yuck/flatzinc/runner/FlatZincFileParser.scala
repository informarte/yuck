package yuck.flatzinc.runner

import java.util.concurrent.{Callable, Executors}

import yuck.flatzinc.ast.FlatZincAst
import yuck.flatzinc.parser.FlatZincParser
import yuck.util.arm.{ManagedExecutorService, ManagedShutdownHook, scoped}
import yuck.util.logging.LazyLogger

/**
 * Provides FlatZinc parsing with interrupt handling.
 *
 * @author Michael Marte
 */
final class FlatZincFileParser(fznFilePath: String, logger: LazyLogger) extends Callable[FlatZincAst] {

    override def call = {
        val file = new java.io.File(fznFilePath)
        val reader = new java.io.InputStreamReader(new java.io.FileInputStream(file))
        class FlatZincParserRunner extends Callable[FlatZincAst] {
            override def call = {
                FlatZincParser.parse(reader)
            }
        }
        val threadPool = Executors.newFixedThreadPool(1)
        val futureAst = threadPool.submit(new FlatZincParserRunner)
        val ast =
            scoped(new ManagedExecutorService(threadPool, logger)) {
                scoped(new ManagedShutdownHook({logger.log("Received SIGINT"); futureAst.cancel(false)})) {
                    futureAst.get
                }
            }
        ast
    }

}
