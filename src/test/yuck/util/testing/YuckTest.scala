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

    System.setProperty("java.util.logging.manager", classOf[yuck.util.logging.ManagedLogManager].getName)
    private val logManager = java.util.logging.LogManager.getLogManager.asInstanceOf[yuck.util.logging.ManagedLogManager]
    val nativeLogger = java.util.logging.Logger.getLogger(this.getClass.getName)
    val logger = new yuck.util.logging.LazyLogger(nativeLogger)
    val formatter = new yuck.util.logging.Formatter
    nativeLogger.setUseParentHandlers(false); // otherwise our console handler would remain unused
    nativeLogger.setLevel(java.util.logging.Level.ALL)
    private val consoleHandler = new java.util.logging.ConsoleHandler
    consoleHandler.setFormatter(formatter)
    nativeLogger.addHandler(consoleHandler)
    logger.setThresholdLogLevel(yuck.util.logging.FineLogLevel)

    val testName = new TestName

    @(Rule @getter)
    val environmentManagement =
        RuleChain
        .outerRule(testName)
        // For the case that the test method under execution initiates a shutdown upon interrupt,
        // we deploy an empty, managed shutdown hook to enforce the completion of the shutdown.
        // (Without it, the JVM would already exit after running the test method's JVM shutdown hook(s).)
        // However, as a side effect, test methods without interrupt handling (e.g. typical unit tests)
        // will ignore interrupts.
        .around(new ManagedResourceAsTestRule(new yuck.util.arm.ManagedShutdownHook({})))
        .around(new ManagedResourceAsTestRule(logManager))
        .around(
            new ManagedResourceAsTestRule(
                new yuck.util.logging.DurationLogger(logger, "Running %s".format(testName.getMethodName))))
        .around(new ManagedResourceAsTestRule(new yuck.util.logging.LogScope(logger)))

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
