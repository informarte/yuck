package yuck.core

/**
 * @author Michael Marte
 *
 */
final class CyclicConstraintNetworkException
extends IllegalStateException("Constraint network is not a DAG")
