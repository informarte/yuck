package yuck.util.logging

import yuck.util.arm.ManagedResource

/**
 * A replacement for the
 * [[http://docs.oracle.com/javase/7/docs/api/java/util/logging/LogManager.html Java default log manager]],
 * which has an annoying feature:
 * It installs a JVM shutdown hook which will disable logging before or during a shutdown (caused by SIGINT)
 * by calling the log manager's reset method, see
 * [[https://stackoverflow.com/questions/13825403/java-how-to-get-logger-to-work-in-shutdown-hook]].
 * To disable the shutdown hook, the replacement ignores calls to reset and instead will disable logging
 * only upon closing.
 *
 * Use only the instance returned by java.util.logging.getLogManager!
 *
 * @author Michael Marte
 */
final class ManagedLogManager extends java.util.logging.LogManager with ManagedResource {
    override def open: Unit = ()
    override def close: Unit = {
        super.reset
    }
    override def reset: Unit = ()
}
