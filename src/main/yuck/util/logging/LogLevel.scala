package yuck.util.logging

/**
 * [[http://docs.oracle.com/javase/7/docs/api/java/util/logging/Level.html
 * Java log levels]]
 * have an issue with regard to rebasing of message trees by means of
 * [[yuck.util.logging.LazyLogger#withRootLogLevel LazyLogger.withRootLogLevel]]:
 * The intValue distance between INFO and FINE is 300 instead of 100 as between all the
 * other levels.
 * Therefore the Java log levels could not be reused.
 *
 * @author Michael Marte
 */
enum LogLevel(val intValue: Int, val name: String) {

    case NoLogging extends LogLevel(Int.MaxValue, "OFF")
    case InfoLogLevel extends LogLevel(1000, "INFO")
    case FineLogLevel extends LogLevel(900, "FINE")
    case FinerLogLevel extends LogLevel(800, "FINER")
    case FinestLogLevel extends LogLevel(700, "FINEST")
    case LogEverything extends LogLevel(Int.MinValue, "ALL")

    override def toString = name

}
