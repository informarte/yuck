package yuck.util

import java.time.Duration
import java.util.concurrent.locks.Lock

import yuck.util.logging.LazyLogger

/**
 * This package contains exception-safe functions for working with managed resources.
 *
 * @author Michael Marte
 */
package object arm {

    /**
     * Opens the given resource, runs the given operation on the resource,
     * closes the resource, and returns the operation's result.
     */
    def using
        [Resource <: ManagedResource, Result]
        (resource: Resource)
        (operation: Resource => Result):
        Result =
    {
        resource.open
        try {
           operation(resource)
        } finally {
           resource.close
        }
    }

    /**
     * Opens the given resource, runs the given operation,
     * closes the resource, and returns the operation's result.
     */
    def scoped
        [Resource <: ManagedResource, Result]
        (resource: Resource)
        (operation: => Result):
        Result =
    {
        using(resource)(_ => operation)
    }

    /**
     * Locks the given Java lock, runs the given operation,
     * unlocks the lock, and returns the operation's result.
     */
    def criticalSection
        [Result]
        (lock: Lock)
        (operation: => Result): Result =
    {
        scoped(new ManagedLock(lock))(operation)
    }

    /**
     * When a runtime limit is given, runs the given operation and interrupts it
     * after reaching the runtime limit.
     * Otherwise just runs the operation.
     */
    def maybeTimeboxed
        [Result]
        (maybeRuntimeLimitInSeconds: Option[Int],
         sigint: SettableSigint,
         operationName: String,
         logger: LazyLogger)
        (operation: => Result):
        Result =
    {
        if (maybeRuntimeLimitInSeconds.isDefined) {
            new TimeboxedOperation(operation, maybeRuntimeLimitInSeconds.get, sigint, operationName, logger).call
        } else {
            operation
        }
    }

    /** Runs the given operation and returns its result and its runtime. */
    def runtime
        [Result]
        (operation: => Result):
        (Result, Duration) =
    {
        val stopWatch = new StopWatch
        val result = scoped(stopWatch)(operation)
        (result, stopWatch.duration)
    }

}
