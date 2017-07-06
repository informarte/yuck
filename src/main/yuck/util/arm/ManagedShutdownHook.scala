package yuck.util.arm

import scala.concurrent.SyncVar

/**
 * Thrown when adding a JVM shutdown hook failed because a JVM shutdown is already in progress.
 *
 * @author Michael Marte
 */
final class ShutdownInProgressException(error: IllegalStateException) extends RuntimeException(error)

/**
 * A managed resource that installs a JVM shutdown hook upon opening and removes it
 * upon closing.
 *
 * You may nest managed shutdown hooks but notice that the JVM will run the associated shutdown
 * actions in an undefined order.
 *
 * When a JVM shutdown is already in progress, open will throw a [[yuck.util.arm.ShutdownInProgressException
 * ShutdownInProgressException]].
 *
 * @param shutdownAction is the action to run upon SIGINT.
 *
 * @author Michael Marte
 */
final class ManagedShutdownHook(shutdownAction: => Unit) extends ManagedResource {

    private val stop = new SyncVar[Boolean]

    private val shutdownHook =
        new Thread(
            new Runnable {
                override def run {
                    shutdownAction
                    // The JVM will terminate right after running all shutdown hooks and finalizers.
                    // Hence we have to keep this thread running until close tells us that
                    // it is time to exit.
                    stop.take
                }
            }
        )

    override def open {
        // Here we register our shutdown hook with the JVM.
        // This will fail in case a JVM shutdown is already in progress.
        try {
            Runtime.getRuntime.addShutdownHook(shutdownHook)
        }
        catch {
            case error: IllegalStateException => throw new ShutdownInProgressException(error)
        }
    }

    override def close {
        // First we try to remove our shutdown hook.
        // This will fail in case a JVM shutdown is already in progress.
        try {
            Runtime.getRuntime.removeShutdownHook(shutdownHook)
        }
        catch {
            case error: IllegalStateException =>
        }
        // Second we tell our shutdown hook that it is time to exit.
        stop.put(true)
        // There are three cases:
        // 1. If the hook was not and will not be called, the signal will do no harm.
        // 2. If the hook is already waiting for this signal, it will exit right away.
        // 3. If the hook is to be called or already in the course of processing the
        //    user-defined shutdown action, it will exit right after the end of the
        //    shutdown action.
    }

}
