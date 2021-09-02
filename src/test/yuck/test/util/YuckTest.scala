package yuck.test.util

import org.junit._
import org.junit.rules.{RuleChain, TestName}

import yuck.util.arm.DummyResource
import yuck.util.logging.YuckLogging

/**
 * @author Michael Marte
 *
 */
abstract class YuckTest extends YuckAssert with YuckLogging {

    protected val formatter = new yuck.util.logging.Formatter
    nativeLogger.setUseParentHandlers(false); // otherwise our console handler would remain unused
    nativeLogger.setLevel(java.util.logging.Level.ALL)
    private val consoleHandler = new java.util.logging.ConsoleHandler
    consoleHandler.setFormatter(formatter)
    logger.setThresholdLogLevel(yuck.util.logging.InfoLogLevel)

    // By default, don't log to console when the test is run in parallel to other tests.
    protected val logToConsole = Thread.currentThread.getName == "main"

    protected val testName = new TestName

    protected val sigint = new yuck.util.arm.SettableSigint

    @Rule
    def environmentManagement =
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
                if (logToConsole) new yuck.util.logging.ManagedLogHandler(nativeLogger, consoleHandler)
                else DummyResource))
        .around(
            new ManagedResourceAsTestRule(
                new yuck.util.logging.DurationLogger(
                    logger, "Running %s.%s".format(getClass.getSimpleName, testName.getMethodName))))
        .around(new ManagedResourceAsTestRule(new yuck.util.logging.LogScope(logger)))

}
