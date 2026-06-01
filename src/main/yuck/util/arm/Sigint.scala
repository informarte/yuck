package yuck.util.arm

import scala.collection.*

/**
 * Provides a read-only channel for interrupt signals.
 *
 * (The Java interrupt mechanism (Thread.interrupt, Future.cancel) is not suitable
 * for anytime algorithms: When a Future gets cancelled in one way or another, it
 * will yield no result, even when the interrupted computation has provided one.
 * Therefore we have to provide and use our own interruption mechanism.)
 */
abstract class Sigint {
    @volatile protected var interrupted = false
    inline final def isSet: Boolean = interrupted
    def registerListener(listener: Thread): Unit
}

/**
 * Provides a means to interrupt a computation (by sending a signal).
 */
class SettableSigint extends Sigint {

    private val listeners = new mutable.ArrayBuffer[Thread]

    override def registerListener(listener: Thread): Unit = synchronized {
        if interrupted then {
            listener.interrupt()
        } else {
            listeners += listener
        }
    }

    final def set(): Unit = synchronized {
        interrupted = true
        for listener <- listeners do {
            listener.interrupt()
        }
        listeners.clear()
    }

}

/**
 * Provides a means to interrupt a computation (by sending a signal)
 * and to later resume the computation (by revoking the signal).
 */
final class RevocableSigint extends SettableSigint {
    def revoke(): Unit = synchronized {
        interrupted = false
    }
}
