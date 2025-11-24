package yuck.util.logging

import yuck.util.arm.ManagedResource

/**
 * A managed resource that reduces the log level upon opening and undoes
 * the change upon closing.
 *
 * @see [[yuck.util.logging.LazyLogger]]
 */
final class TransientLogLevelReduction(logger: LazyLogger, logLevelReduction: Int = 0) extends ManagedResource {

    override def open() = {
        if logLevelReduction > 0 then {
            logger.increaseLogLevelReduction(logLevelReduction)
        }
    }

    override def close() = {
        if logLevelReduction > 0 then {
            logger.restoreLogLevelReduction()
        }
    }

}
