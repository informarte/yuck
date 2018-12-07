package yuck.core

/**
 * @author Michael Marte
 *
 */
final class DomainWipeOutException(val x: AnyVariable)
extends RuntimeException("Domain of %s was wiped out".format(x))
