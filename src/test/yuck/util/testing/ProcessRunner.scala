package yuck.util.testing

import java.util.concurrent.Callable

import scala.collection._
import scala.jdk.CollectionConverters._

import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
class ProcessRunner(logger: LazyLogger, commandLine: Seq[String]) extends Callable[Seq[String]] {

    override def call = {
        val processBuilder = new java.lang.ProcessBuilder(commandLine.asJava)
        processBuilder.redirectErrorStream(true) // merge stderr into stdout
        val command = processBuilder.command.asScala
        logger.withLogScope(command.iterator.mkString(" ")) {
            val process = processBuilder.start
            val stdout = scala.io.Source.fromInputStream(process.getInputStream)
            val outputLines = stdout.getLines.toSeq
            outputLines.foreach(logger.log(_))
            val exitCode = process.waitFor
            assert(exitCode == 0, "Process failed with exit code %d".format(exitCode))
            outputLines
        }
    }

}
