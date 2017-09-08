package yuck.util.testing

import scala.annotation.meta.getter

import org.junit._
import org.junit.rules.RuleChain
import org.junit.rules.TestName

/**
 * @author Michael Marte
 *
 */
abstract class YuckTest extends YuckAssert {

    System.setProperty("java.util.logging.manager", classOf[yuck.util.logging.ManagedLogManager].getName)
    private val logManager = java.util.logging.LogManager.getLogManager.asInstanceOf[yuck.util.logging.ManagedLogManager]
    protected val nativeLogger = java.util.logging.Logger.getLogger(this.getClass.getName)
    protected val logger = new yuck.util.logging.LazyLogger(nativeLogger)
    protected val formatter = new yuck.util.logging.Formatter
    nativeLogger.setUseParentHandlers(false); // otherwise our console handler would remain unused
    nativeLogger.setLevel(java.util.logging.Level.ALL)
    private val consoleHandler = new java.util.logging.ConsoleHandler
    consoleHandler.setFormatter(formatter)
    nativeLogger.addHandler(consoleHandler)
    logger.setThresholdLogLevel(yuck.util.logging.FineLogLevel)

    protected val testName = new TestName

    @(Rule @getter)
    protected val environmentManagement =
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
                new yuck.util.logging.DurationLogger(logger, "Running %s.%s".format(getClass.getSimpleName, testName.getMethodName))))
        .around(new ManagedResourceAsTestRule(new yuck.util.logging.LogScope(logger)))

}
