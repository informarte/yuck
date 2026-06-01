package yuck.util

import java.time.Duration
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.locks.Lock

import yuck.util.logging.LazyLogger

/**
 * This package contains exception-safe functions for working with managed resources.
 */
package object arm {

    /**
     * Opens the given resource, runs the given operation on the resource,
     * closes the resource, and returns the operation's result.
     */
    inline def using
        [Resource <: ManagedResource, Result]
        (resource: Resource)
        (inline operation: Resource => Result):
        Result =
    {
        resource.open()
        try {
           operation(resource)
        } finally {
           resource.close()
        }
    }

    /**
     * Opens the given resource, runs the given operation,
     * closes the resource, and returns the operation's result.
     */
    inline def scoped
        [Resource <: ManagedResource, Result]
        (resource: Resource)
        (inline operation: => Result):
        Result =
    {
        resource.open()
        try {
            operation
        } finally {
            resource.close()
        }
    }

    /**
     * Locks the given Java lock, runs the given operation,
     * unlocks the lock, and returns the operation's result.
     */
    inline def criticalSection
        [Result]
        (lock: Lock)
        (inline operation: => Result): Result =
    {
        scoped(new ManagedLock(lock))(operation)
    }

    /**
     * When a runtime limit is given, runs the given operation and interrupts it
     * after reaching the runtime limit.
     * Otherwise just runs the operation.
     */
    inline def maybeTimeboxed
        [Result]
        (maybeRuntimeLimitInMillis: Option[AtomicLong], sigint: SettableSigint, logger: LazyLogger)
        (operation: => Result):
        Result =
    {
        if maybeRuntimeLimitInMillis.isDefined then {
            val timebox = new Timebox(maybeRuntimeLimitInMillis.get, sigint, Thread.currentThread.getName, logger)
            val timeboxThread = new Thread(timebox, "%sTimebox".format(Thread.currentThread.getName))
            scoped(new ManagedThread(timeboxThread, logger)) {
                operation
            }
        }
        else operation
    }

    /** Runs the given operation and returns its result and its runtime. */
    inline def runtime
        [Result]
        (inline operation: => Result):
        (Result, Duration) =
    {
        val stopWatch = new StopWatch
        val result = scoped(stopWatch)(operation)
        (result, stopWatch.duration)
    }

}
