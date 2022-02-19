package yuck.core

/**
 * Provides ids for objects of the given type.
 *
 * @author Michael Marte
 */
final class IdFactory[T] {
    private var nextRawId = -1
    def nextId(): Id[T] = {
        nextRawId += 1
        new Id[T](nextRawId)
    }
}
