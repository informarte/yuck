package yuck.core

abstract class InconsistentProblemException(msg: String) extends RuntimeException(msg)

final class DomainWipeOutException(val x: AnyVariable)
extends InconsistentProblemException("Domain of %s was wiped out".format(x))
