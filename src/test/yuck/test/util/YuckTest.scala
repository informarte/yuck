package yuck.test.util

import org.junit.jupiter.api.Order
import org.junit.jupiter.api.extension.RegisterExtension

import yuck.util.arm.DummyResource
import yuck.util.logging.YuckLogging

abstract class YuckTest extends YuckAssert with YuckLogging {

    protected val formatter = new yuck.util.logging.Formatter
    nativeLogger.setUseParentHandlers(false); // otherwise our console handler would remain unused
    nativeLogger.setLevel(java.util.logging.Level.ALL)
    private val consoleHandler = new java.util.logging.ConsoleHandler
    consoleHandler.setFormatter(formatter)
    logger.setThresholdLogLevel(yuck.util.logging.LogLevel.InfoLogLevel)

    protected val logToConsole = false

    protected val sigint = new yuck.util.arm.SettableSigint

    // For the case that the test method under execution initiates a shutdown upon interrupt,
    // we deploy an empty, managed shutdown hook to enforce the completion of the shutdown.
    // (Without it, the JVM would already exit after running the test method's JVM shutdown hook(s).)
    // However, as a side effect, test methods without interrupt handling (e.g. typical unit tests)
    // will ignore interrupts.
    @RegisterExtension
    @Order(1)
    val shutdownHook = new ManagedResourceAsExtension(_ => new yuck.util.arm.ManagedShutdownHook("YuckTestShutdownHook", {}))

    @RegisterExtension
    @Order(2)
    val logHandler = new ManagedResourceAsExtension(_ =>
        if logToConsole
        then new yuck.util.logging.ManagedLogHandler(nativeLogger, consoleHandler)
        else DummyResource
    )

    @RegisterExtension
    @Order(3)
    val logScope = new ManagedResourceAsExtension(_ => new yuck.util.logging.LogScope(logger))

    @RegisterExtension
    @Order(4)
    val durationLogger = new ManagedResourceAsExtension(context =>
        new yuck.util.logging.DurationLogger(
            logger,
            "Running %s.%s".format(
                context.getTestClass.map(_.getSimpleName).orElse("unknown"),
                context.getTestMethod.map(_.getName).orElse("unknown")))
    )

    @RegisterExtension
    @Order(5)
    val threadRenaming =
        new ManagedResourceAsExtension(
            _ => new yuck.util.logging.TransientThreadRenaming(Thread.currentThread, getClass.getSimpleName))

}
