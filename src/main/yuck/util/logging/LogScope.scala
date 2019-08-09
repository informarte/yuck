package yuck.util.logging

import yuck.util.arm.ManagedResource

/**
 * A managed resource that increases indendation upon opening and undoes
 * the change upon closing.
 *
 * @author Michael Marte
 *
 * @see [[yuck.util.logging.LazyLogger]]
 */
final class LogScope(logger: LazyLogger, indentation: Int = 1) extends ManagedResource {

    require(indentation >= 0)

    override def open: Unit = {
        for (i <- 1 to indentation) {
            logger.increaseIndentation
        }
    }

    override def close: Unit = {
        for (i <- 1 to indentation) {
            logger.decreaseIndentation
        }
    }

}
