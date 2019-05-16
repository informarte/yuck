package yuck.core

/**
 * @author Michael Marte
 *
 */
abstract class InconsistentProblemException(msg: String) extends RuntimeException(msg)

/**
 * @author Michael Marte
 *
 */
final class DomainWipeOutException(val x: AnyVariable)
extends InconsistentProblemException("Domain of %s was wiped out".format(x))
