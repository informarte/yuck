package yuck.util.arm

/**
 * Can be used in place of a real resource.
 *
 * @author Michael Marte
 */
final object DummyResource extends ManagedResource {
    override def open {}
    override def close {}
}
