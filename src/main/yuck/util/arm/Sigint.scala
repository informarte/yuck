package yuck.util.arm

/**
 * Provides a read-only channel for interrupt signals.
 *
 * (The Java interrupt mechanism (Thread.interrupt, Future.cancel) is not suitable
 * for anytime algorithms: When a Future gets cancelled in one way or another, it
 * will yield no result, even when the interrupted computation has provided one.
 * Therefore we have to provide and use our own interruption mechanism.)
 *
 * @author Michael Marte
 */
abstract class Sigint {
    protected var interrupted = false
    @inline final def isSet: Boolean = interrupted
}

/**
 * Provides a means to interrupt a computation (by sending a signal).
 *
 * @author Michael Marte
 */
class SettableSigint extends Sigint {
    final def set(): Unit = {
        interrupted = true
    }
}

/**
 * Provides a means to interrupt a computation (by sending a signal)
 * and to later resume the computation (by revoking the signal).
 *
 * @author Michael Marte
 */
final class RevocableSigint extends SettableSigint {
    def revoke(): Unit = {
        interrupted = false
    }
}
