package yuck.core

/**
 * Provides ids for objects of the given type.
 *
 * @author Michael Marte
 */
final class IdFactory[Type] {
    private var nextRawId = -1
    def nextId: Id[Type] = {
        nextRawId += 1
        new Id[Type](nextRawId)
    }
}
