package yuck.util

import java.util.concurrent.locks.Lock

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

}
