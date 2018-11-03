package yuck.core

/**
 * Provides traits of integer values.
 *
 * @author Michael Marte
 */
final object IntegerValueTraits extends NumericalValueTraits[IntegerValue] {
    override val valueType = classOf[IntegerValue]
    override val orderingCostModel = IntegerOrderingCostModel
    override def safeDowncast(x: Domain[IntegerValue]): IntegerDomain = x.asInstanceOf[IntegerDomain]
    override def createDomain(values: Set[IntegerValue]): IntegerDomain = IntegerDomain.createDomain(values)
    override def createDomain(lb: IntegerValue, ub: IntegerValue) = IntegerDomain.createRange(lb, ub)
    override val emptyDomain: IntegerDomain = EmptyIntegerRange
    override val completeDomain: IntegerDomain = CompleteIntegerRange
    override val nonNegativeDomain: IntegerDomain = NonNegativeIntegerRange
    override val zero = Zero
    override val one = One
}
