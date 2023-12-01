package yuck.test.util

import java.util.concurrent.Callable

import scala.collection.*
import scala.jdk.CollectionConverters.*

import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
class ProcessRunner(logger: LazyLogger, commandLine: Seq[String]) extends Callable[Seq[String]] {

    override def call() = {
        val processBuilder = new java.lang.ProcessBuilder(commandLine.asJava)
        processBuilder.redirectErrorStream(true) // merge stderr into stdout
        val command = processBuilder.command.asScala
        logger.withLogScope(command.iterator.mkString(" ")) {
            val process = processBuilder.start()
            val stdoutReader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
            val outputLines = stdoutReader.lines.iterator.asScala.toList
            outputLines.foreach(logger.log(_))
            val exitCode = process.waitFor()
            assert(exitCode == 0, "Process failed with exit code %d".format(exitCode))
            outputLines
        }
    }

}
