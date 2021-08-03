package yuck.core

/**
 * Implements the cost model for ordering operations on integer-set values.
 *
 * @author Michael Marte
 */
object IntegerSetValueOrderingCostModel extends OrderingCostModel[IntegerSetValue] {
    override def eqViolation(lhs: IntegerSetValue, rhs: IntegerSetValue) =
        safeAdd(lhs.set.maybeResidueSize(rhs.set).getOrElse(1), rhs.set.maybeResidueSize(lhs.set).getOrElse(1))
}
