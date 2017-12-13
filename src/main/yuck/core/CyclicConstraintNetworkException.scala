package yuck.core

/**
 * @author Michael Marte
 *
 */
final class CyclicConstraintNetworkException(val constraint: Constraint)
extends IllegalArgumentException("%s would introduce a cycle".format(constraint))
