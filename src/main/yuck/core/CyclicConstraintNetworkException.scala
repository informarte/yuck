package yuck.core

final class CyclicConstraintNetworkException
extends IllegalStateException("Constraint network is not a DAG")
