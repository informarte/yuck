package yuck.test.util

import org.junit.internal.{RealSystem, TextListener}
import org.junit.runner.{JUnitCore, Request}
import org.junit.Test

import scopt.*

/**
 * Runs an entire test class or a single test method.
 *
 * @author Michael Marte
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
        if (maybeCl.isEmpty) {
            System.exit(1)
        }
        val cl = maybeCl.get
        if (cl.listTestMethods) {
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
            case List(className) => Request.aClass(Class.forName(className))
            case List(className, method) => Request.method(Class.forName(className), method)
            case _ => throw new IllegalArgumentException("Invalid testee")
        }
        val core = new JUnitCore
        val system = new RealSystem
        val listener = new TextListener(system)
        core.addListener(listener)
        val result = core.run(request)
        if (result.wasSuccessful) 0 else 1
    }

}
