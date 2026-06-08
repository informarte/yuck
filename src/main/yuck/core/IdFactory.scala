package yuck.core

/**
 * Provides ids for objects of the given type.
 */
final class IdFactory[T] {
    private var nextRawId = -1
    def setNextId(other: IdFactory[T]): Unit = {
        nextRawId = other.nextRawId
    }
    def nextId(): Id[T] = {
        nextRawId += 1
        new Id[T](nextRawId)
    }
}
