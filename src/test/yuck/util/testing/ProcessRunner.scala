package yuck.util.testing

import java.util.concurrent.Callable

import scala.collection._
import scala.jdk.CollectionConverters._

import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
class ProcessRunner(logger: LazyLogger, commandLine: Seq[String]) extends Callable[(Seq[String], Seq[String])] {

    override def call = {
        val processBuilder = new java.lang.ProcessBuilder(commandLine.asJava)
        val command = processBuilder.command.asScala
        logger.withLogScope(command.iterator.mkString(" ")) {
            val process = processBuilder.start
            val stdout = scala.io.Source.fromInputStream(process.getInputStream)
            val stderr = scala.io.Source.fromInputStream(process.getErrorStream)
            val outputLines = stdout.getLines.toSeq
            val errorLines = stderr.getLines.toSeq
            errorLines.foreach(logger.log(_))
            outputLines.foreach(logger.log(_))
            val exitCode = process.waitFor
            assert(exitCode == 0, "Process failed with exit code %d".format(exitCode))
            (outputLines, errorLines)
        }
    }

}
