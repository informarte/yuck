package yuck.util.testing

import scala.collection.JavaConversions

import java.util.concurrent.Callable
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
class ProcessRunner(logger: LazyLogger, commandLine: Seq[String]) extends Callable[(List[String], List[String])] {

    override def call = {
        val processBuilder = new java.lang.ProcessBuilder(JavaConversions.seqAsJavaList(commandLine))
        val command = JavaConversions.asScalaBuffer(processBuilder.command)
        logger.withLogScope(command.toIterator.mkString(" ")) {
            val process = processBuilder.start
            val stdout = scala.io.Source.fromInputStream(process.getInputStream)
            val stderr = scala.io.Source.fromInputStream(process.getErrorStream)
            val outputLines = stdout.mkString.lines.toList
            val (errorLines, warningLines) = stderr.mkString.lines.toList.partition(_.toLowerCase.contains("error"))
            outputLines.foreach(logger.log(_))
            warningLines.foreach(logger.log(_))
            errorLines.foreach(logger.log(_))
            (outputLines, errorLines)
        }
    }

}
