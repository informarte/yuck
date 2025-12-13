package yuck.test.util

import java.io.PrintWriter

import org.junit.jupiter.api.Test
import org.junit.platform.engine.discovery.DiscoverySelectors
import org.junit.platform.launcher.core.{LauncherDiscoveryRequestBuilder, LauncherFactory}
import org.junit.platform.launcher.listeners.SummaryGeneratingListener
import scopt.*

import yuck.util.arm.{StopWatch, scoped}

/**
 * Runs an entire test class or a single test method.
 */
object YuckTestRunner {

    private case class CommandLine(
        testee: String = "",
        listTestMethods: Boolean = false
    )

    private class CommandLineParser extends OptionParser[CommandLine]("yuck-test-runner") {
        val defaultCl = CommandLine()
        head("Yuck test runner")
        help("help").abbr("h").text("Show this help message")
        opt[Boolean]("list-test-methods")
            .text("Default value is %s".format(defaultCl.listTestMethods))
            .action((x, cl) => cl.copy(listTestMethods = x))
        arg[String]("ClassName or ClassName#methodName")
            .required()
            .hidden()
            .action((x, cl) => cl.copy(testee = x))
    }

    def main(args: Array[String]): Unit = {
        val parser = new CommandLineParser
        val maybeCl = parser.parse(args, new CommandLine)
        if maybeCl.isEmpty then {
            System.exit(1)
        }
        val cl = maybeCl.get
        if cl.listTestMethods then {
            System.exit(listTestMethods(cl))
        } else {
            System.exit(runTest(cl))
        }
    }

    private def listTestMethods(cl: CommandLine): Int = {
        val components = cl.testee.split("#").toList
        val methodNames = components match {
            case List(className) =>
                Class.forName(className).getMethods.iterator.filter(_.getAnnotation(classOf[Test]) != null)
                    .map(method => "%s#%s".format(className, method.getName)).toList.sorted
            case List(className, method) => List(cl.testee)
            case _ => throw new IllegalArgumentException("Invalid testee")
        }
        methodNames.foreach(println)
        0
    }

    private def runTest(cl: CommandLine): Int = {
        val components = cl.testee.split("#").toList
        val request = components match {
            case List(className) => LauncherDiscoveryRequestBuilder
                .request
                .selectors(DiscoverySelectors.selectClass(className))
                .build()
            case List(className, methodName) => LauncherDiscoveryRequestBuilder
                .request
                .selectors(DiscoverySelectors.selectMethod(className, methodName))
                .build()
            case _ =>
                throw new IllegalArgumentException("Invalid testee")
        }
        val launcher = LauncherFactory.create
        val summaryGeneratingListener = new SummaryGeneratingListener()
        launcher.registerTestExecutionListeners(summaryGeneratingListener, new DotReporter())
        val stopWatch = new StopWatch
        scoped(stopWatch) {
            launcher.execute(request)
        }
        println("Time: %s".format(stopWatch.duration.toMillis / 1000.0))
        val summary = summaryGeneratingListener.getSummary
        if summary.getTestsFailedCount == 0 then {
            println
            println("OK (%d tests)".format(summary.getTestsSucceededCount))
            0
        } else {
            summary.printFailuresTo(new PrintWriter(System.out, true), 8)
            println
            println("FAILED (%d/%d tests)".format(summary.getTestsFailedCount, summary.getTestsFoundCount))
            1
        }
    }

}
