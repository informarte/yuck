package yuck.core

/**
 * Implements the cost model for ordering operations on integer-set values.
 *
 * @author Michael Marte
 */
final object IntegerSetValueOrderingCostModel extends OrderingCostModel[IntegerSetValue] {
    override def eq(lhs: IntegerSetValue, rhs: IntegerSetValue) =
        BooleanValue.get(
            safeAdd(lhs.set.maybeResidueSize(rhs.set).getOrElse(1), rhs.set.maybeResidueSize(lhs.set).getOrElse(1)))
}
