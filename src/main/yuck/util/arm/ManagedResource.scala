package yuck.util.arm

/**
 * A managed resource is something that can be opened and closed, like a file.
 *
 * @author Michael Marte
 */
trait ManagedResource extends AutoCloseable {
    def open(): Unit
    override def close(): Unit
}
