package yuck.core

/**
 * Informs about which operations are fully implemented.
 *
 * All non-listed operations can be assumed to be available.
 *
 * @author Michael Marte
 */
case class DomainCapabilities(
    createDomain: Boolean = true,
    diff: Boolean = true,
    randomSubdomain: Boolean = true,
    size: Boolean = true,
    union: Boolean = true
)
