package yuck.core

import java.util.concurrent.atomic.AtomicReference

/**
 * Provides the cost vector of the best solution seen so far.
 *
 * @author Michael Marte
 */
final class SharedBound(holder: AtomicReference[Costs]) {

    override def toString = maybeBound().toString

    def maybeBound(): Option[Costs] = {
        val bound = holder.get()
        if bound == null then None else Some(bound)
    }

}
