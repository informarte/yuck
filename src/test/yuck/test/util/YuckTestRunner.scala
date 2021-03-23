package yuck.test.util

import org.junit.internal.{RealSystem, TextListener}
import org.junit.runner.{JUnitCore, Request}

/**
 * Runs an entire test class or a single test method.
 *
 * @author Michael Marte
 */
object YuckTestRunner {

    private val help = "Invoke with className or className#methodName"

    def main(args: Array[String]): Unit = {
        require(args.length == 1, help)
        val components = args(0).split("#").toList
        val request = components match {
            case List(className) => Request.aClass(Class.forName(className))
            case List(className, method) => Request.method(Class.forName(className), method)
            case _ => throw new IllegalArgumentException(help)
        }
        val core = new JUnitCore
        val system = new RealSystem
        val listener = new TextListener(system)
        core.addListener(listener)
        val result = core.run(request)
        System.exit(if (result.wasSuccessful) 0 else 1)
    }

}
