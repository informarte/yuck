package yuck.util.logging

/**
 * @author Michael Marte
 *
 */
trait YuckLogging {

    // To control logging upon ^C, we install a special log manager.
    // (See ManagedLogManager for the technical background.)
    // GraalVM's native-image, however, does not support setting system properties at runtime.
    // Instead, system properties can be set at compile time with the -D option.
    // (native-image will use this setting when compiling the static initializer of
    // java.util.logging.LogManager.)
    // However, this approach does not work reliably:
    // Often (circumstances evaded investigation) we end up with a corrupted ManagedLogManger instance:
    // Casting this instance to ManagedLogManager fails with a ClassCastException.
    // To work around this issue, we create a new instance of MangedLogManager when
    // the cast is not possible.
    // (This work-around defeats the whole purpose of ManageLogManager but it does not matter
    // because native-image does not support signal handling either: Upon ^C, the program gets
    // killed immediately.)
    System.setProperty("java.util.logging.manager", classOf[ManagedLogManager].getName)
    protected val logManager =
        java.util.logging.LogManager.getLogManager match {
            case logManager: ManagedLogManager => logManager
            case _ => new ManagedLogManager
        }
    protected val nativeLogger = java.util.logging.Logger.getLogger(this.getClass.getName)
    protected val logger = new LazyLogger(nativeLogger)

}
