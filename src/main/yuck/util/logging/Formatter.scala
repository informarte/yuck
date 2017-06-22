package yuck.util.logging

/**
 * @author Michael Marte
 *
 */
class Formatter extends java.util.logging.Formatter {

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
