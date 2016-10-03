package yuck.util.arm

import java.util.concurrent.locks.Lock

/**
 * Manages a [[http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/locks/Lock.html Java lock]].
 *
 * @author Michael Marte
 */
final class ManagedLock(val lock: Lock) extends ManagedResource {
    override def open = lock.lock
    override def close = lock.unlock
}
