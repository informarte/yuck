package yuck.util.testing

import scala.annotation.meta.getter

import org.junit._
import org.junit.rules.RuleChain
import org.junit.rules.TestName

/**
 * @author Michael Marte
 *
 */
class YuckTest {

    val nativeLogger = java.util.logging.Logger.getLogger(this.getClass.getName)
    val logger = new yuck.util.logging.LazyLogger(nativeLogger)

    val formatter = new java.util.logging.Formatter {
        private val startTime = new java.util.Date
        private val dateFormat = new java.text.SimpleDateFormat("HH:mm:ss")
        override def format(logRecord: java.util.logging.LogRecord) = {
            val now = new java.util.Date
            val duration = (now.getTime - startTime.getTime) / 1000
            val prefix =
                "%02d:%02d:%02d Thread %d/ %s: ".format(
                    duration / 3600, (duration % 3600) / 60, (duration % 60),
                    Thread.currentThread.getId, Thread.currentThread.getName)
            val builder = new StringBuilder
            val message = logRecord.getMessage
            val error = logRecord.getThrown
            if (message != null && ! message.isEmpty) {
                builder ++= "%s%s\n".format(prefix, logRecord.getMessage)
            }
            if (error != null) {
                builder ++= "%s%s\n".format(prefix, error.toString)
                error.getStackTrace.foreach(frame => builder ++= "%s%s\n".format(prefix, frame.toString))
            }
            builder.toString
        }
    }

    {
        java.util.logging.LogManager.getLogManager().reset() // remove handlers
        nativeLogger.setLevel(java.util.logging.Level.ALL)
        val consoleHandler = new java.util.logging.ConsoleHandler
        consoleHandler.setFormatter(formatter)
        nativeLogger.addHandler(consoleHandler)
        logger.setThresholdLogLevel(yuck.util.logging.FineLogLevel)
    }

    val testName = new TestName

    @(Rule @getter)
    val runtimeMeasurementAndLogScoping =
        RuleChain
        .outerRule(testName)
        .around(
            new ManagedResourceAsTestRule(
                new yuck.util.logging.DurationLogger(logger, "Running %s".format(testName.getMethodName))))
        .around(
            new ManagedResourceAsTestRule(
                new yuck.util.logging.LogScope(logger)))

    def assert(b: Boolean) {
        Assert.assertTrue(b)
    }

    def assertEq[T](a: T, b: T) {
        Assert.assertTrue("%s (testee) != %s".format(a, b), a == b)
    }

    def assertNe[T](a: T, b: T) {
        Assert.assertTrue("%s (testee) == %s".format(a, b), a != b)
    }

    def assertLe[T](a: T, b: T)(implicit ord: Ordering[T]) {
        Assert.assertTrue("%s (testee) > %s".format(a, b), ord.compare(a, b) <= 0)
    }

    def assertLt[T](a: T, b: T)(implicit ord: Ordering[T]) {
        Assert.assertTrue("%s (testee) >= %s".format(a, b), ord.compare(a, b) < 0)
    }

    def assertGe[T](a: T, b: T)(implicit ord: Ordering[T]) {
        Assert.assertTrue("%s (testee) < %s".format(a, b), ord.compare(a, b) >= 0)
    }

    def assertGt[T](a: T, b: T)(implicit ord: Ordering[T]) {
        Assert.assertTrue("%s (testee) <= %s".format(a, b), ord.compare(a, b) > 0)
    }

    def assertEx(operation: => Unit) {
        var failed = true
        try {
            operation
            failed = false
        }
        catch {
            case _: Throwable =>
        }
        if (! failed) {
            Assert.fail("Expected some exception")
        }
    }

    def assertEx(operation: => Unit, expectedExceptionType: Class[_ <: Exception]) {
        var failed = true
        try {
            operation
            failed = false
        }
        catch {
            case throwable: Throwable =>
                Assert.assertTrue(
                    "Expected %s but got %s".format(expectedExceptionType, throwable.getClass),
                    findExceptionType(throwable, expectedExceptionType))
        }
        if (! failed) {
            Assert.fail("Expected %s".format(expectedExceptionType))
        }
    }

    private def findExceptionType(throwable: Throwable, expectedExceptionType: Class[_ <: Exception]): Boolean =
        throwable.getClass == expectedExceptionType ||
        (throwable.getCause != null && findExceptionType(throwable.getCause, expectedExceptionType))

}
