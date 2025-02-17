package yuck.util.logging

/**
 * A replacement for the
 * [[http://docs.oracle.com/javase/7/docs/api/java/util/logging/LogManager.html Java default log manager]],
 * which has an annoying feature:
 * It installs a JVM shutdown hook which will disable logging before or during a shutdown (caused by SIGINT)
 * by calling the log manager's reset method, see
 * [[https://stackoverflow.com/questions/13825403/java-how-to-get-logger-to-work-in-shutdown-hook]].
 * To disable the shutdown hook, the replacement ignores calls to reset.
 *
 * Use only the instance returned by java.util.logging.getLogManager!
 *
 * @author Michael Marte
 */
final class LogManager extends java.util.logging.LogManager {

    override def reset() = {
    }

}
