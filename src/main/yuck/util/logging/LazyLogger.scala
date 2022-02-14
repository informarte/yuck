package yuck.util.logging

import java.util.ArrayDeque
import java.util.concurrent.locks.ReentrantLock
import java.util.logging.{Level, Logger}

import yuck.util.arm.scoped

/**
 * Turns a given [[http://docs.oracle.com/javase/7/docs/api/java/util/logging/Logger.html
 * Java logger]] into a thread-safe lazy logger.
 *
 * Maintains thread-specific indent levels; initially they are zero
 * but can be increased and decreased as required to implement log
 * scopes.
 *
 * Maintains thread-specific log-level reductions; initially the
 * values are zero but can be increased and restored as required. If a
 * reduction is configured, incoming messages will be considered less
 * important accordingly. With this feature, the importance of log
 * messages from subcomponents can be lowered by the calling component.
 * As a consequence of this approach, the interface does not provide info,
 * debug, and so on, as usual, but log, logg, logg, and so on, the more g's,
 * the less important the message is. (These methods take a message factory
 * instead of a message, which implements lazy logging.)
 *
 * @author Michael Marte
 */
final class LazyLogger(logger: Logger) {

    private val lock = new ReentrantLock

    private var thresholdLogLevel: LogLevel = InfoLogLevel

    /** Sets the log level below which no messages will be logged. */
    def setThresholdLogLevel(logLevel: LogLevel): Unit = {
        this.thresholdLogLevel = logLevel
    }

    private var indent = "  "

    /** Sets the whitespace to indent one level; the default value is two spaces. */
    def setIndent(indent: String): Unit = {
        this.indent = indent
    }

    private val indentLevel = new ThreadLocal[Int] {
        override def initialValue = 0
    }
    private val indents = new scala.collection.mutable.HashMap[Int, String]
    private def currentIndent(indentLevel: Int): String = synchronized {
        indents.getOrElse(indentLevel, {
            val indent = if (indentLevel == 0) "" else currentIndent(indentLevel - 1) + this.indent
            indents += indentLevel -> indent
            indent
        })
    }

    /** Increases the indentation by one level. */
    def increaseIndentation(): Unit = {
        indentLevel.set(indentLevel.get + 1)
    }

    /** Decreases the indentation by one level. */
    def decreaseIndentation(): Unit = {
        require(indentLevel.get > 0)
        indentLevel.set(indentLevel.get - 1)
    }

    /** Returns the current indentation level. */
    def currentIndentation: Int =
        indentLevel.get

    private type Stack[E] = ArrayDeque[E]

    private val logLevelReductions = new ThreadLocal[Stack[Int]] {
        override def initialValue = {
            val stack = new Stack[Int]
            stack.push(0)
            stack
        }
    }

    /** Increases dynamic log-level reduction by the given value. */
    def increaseLogLevelReduction(reduction: Int): Unit = {
        require(reduction > 0)
        val stack = logLevelReductions.get
        stack.push(stack.peek + reduction)
    }

    /** Undoes the previous change to dynamic log-level reduction. */
    def restoreLogLevelReduction(): Unit = {
        val stack = logLevelReductions.get
        stack.pop
        assert(! stack.isEmpty)
    }

    /** Returns the current log-level reduction. */
    def currentLogLevelReduction: Int = logLevelReductions.get.peek

    /** Logs on InfoLogLevel - currentLogLevelReduction. */
    def log(msg: => String): Unit = {
        write(InfoLogLevel, msg)
    }
    /** Logs on FineLogLevel - currentLogLevelReduction. */
    def logg(msg: => String): Unit = {
        write(FineLogLevel, msg)
    }
    /** Logs on FinerLogLevel - currentLogLevelReduction. */
    def loggg(msg: => String): Unit = {
        write(FinerLogLevel, msg)
    }
    /** Logs on FinestLogLevel - currentLogLevelReduction. */
    def logggg(msg: => String): Unit = {
        write(FinestLogLevel, msg)
    }

    // ConsoleHandler does not print anything when logging on level ALL!?
    // So we use level INFO.
    private def write(logLevel: LogLevel, msg: => String): Unit = {
        if (thresholdLogLevel.intValue <= logLevel.intValue - currentLogLevelReduction) {
            yuck.util.arm.criticalSection(lock)(logger.log(Level.INFO, currentIndent(indentLevel.get) + msg))
        }
    }

    /**
     * Logs the given operation name and then runs the given operation in
     * a new log scope. In the resulting log, the given operation name will
     * appear as title and the log messages of the given operation will be
     * indented one level more than the title.
     */
    def withLogScope
        [Result]
        (operationName: String)
        (operation: => Result): Result =
    {
        log(operationName)
        scoped(new LogScope(this))(operation)
    }

    /** Like withLogScope but also logs the duration of the given operation. */
    def withTimedLogScope
        [Result]
        (operationName: String)
        (operation: => Result): Result =
    {
        scoped(new DurationLogger(this, operationName)) {
            scoped(new LogScope(this)) {
                operation
            }
        }
    }

    /**
     * Reduces the importance of all log messages from the given operation
     * such that InfoLogLevel maps to the given root log level.
     */
    def withRootLogLevel
        [Result]
        (rootLogLevel: LogLevel)
        (operation: => Result): Result =
    {
        scoped(new TransientLogLevelReduction(this, InfoLogLevel.intValue - rootLogLevel.intValue)) {
            operation
        }
    }

    /** Provides exclusive access to this logger. */
    def criticalSection
        [Result]
        (operation: => Result): Result =
    {
        yuck.util.arm.criticalSection(lock)(operation)
    }

}
