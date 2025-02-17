package yuck.util.logging

/**
 * @author Michael Marte
 *
 */
trait YuckLogging {

    // To control logging upon ^C, we install a special log manager.
    // (See ManagedLogManager for the technical background.)
    System.setProperty("java.util.logging.manager", classOf[LogManager].getName)
    protected val logManager = java.util.logging.LogManager.getLogManager.asInstanceOf[LogManager]
    protected val nativeLogger = java.util.logging.Logger.getLogger("%s-%d".format(this.getClass.getName, Thread.currentThread.getId))
    protected val logger = new LazyLogger(nativeLogger)

}
