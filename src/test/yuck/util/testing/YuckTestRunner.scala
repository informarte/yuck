package yuck.util.testing

import org.junit.internal.{RealSystem, TextListener}
import org.junit.runner.JUnitCore
import org.junit.runner.Request
import org.junit.runner.notification.RunListener

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
        require(Range.inclusive(1, 2).contains(components.size), help)
        val request = components match {
            case List(className) => Request.aClass(Class.forName(className))
            case List(className, method) => Request.method(Class.forName(className), method)
        }
        val core = new JUnitCore
        val system = new RealSystem
        val listener = new TextListener(system)
        core.addListener(listener)
        val result = core.run(request)
        System.exit(if (result.wasSuccessful) 0 else 1)
    }

}
