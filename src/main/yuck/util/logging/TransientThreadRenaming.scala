package yuck.util.logging

import yuck.util.arm.ManagedResource

/**
 * A managed resource that renames the given thread to the given name
 * upon opening and undoes the renaming upon closing.
 *
 * @author Michael Marte
 */
final class TransientThreadRenaming(thread: Thread, name: String) extends ManagedResource {

    private var previousName = ""

    override def open = {
        previousName = thread.getName
        thread.setName(name)
    }

    override def close = {
        thread.setName(previousName)
    }

}
