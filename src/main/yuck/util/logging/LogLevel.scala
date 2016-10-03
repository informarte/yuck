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
trait LogLevel {
    def intValue: Int
    def name: String
    override def toString = name
}

case object NoLogging extends LogLevel {
    override def intValue = Int.MaxValue
    override def name = "OFF"
}

case object InfoLogLevel extends LogLevel {
    override def intValue = 1000
    override def name = "INFO"
}

case object FineLogLevel extends LogLevel {
    override def intValue = 900
    override def name = "FINE"
}

case object FinerLogLevel extends LogLevel {
    override def intValue = 800
    override def name = "FINER"
}

case object FinestLogLevel extends LogLevel {
    override def intValue = 700
    override def name = "FINEST"
}

case object LogEverything extends LogLevel {
    override def intValue = Int.MinValue
    override def name = "ALL"
}
