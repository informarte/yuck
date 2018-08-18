package yuck.core

/**
 * Provides traits of integer-set values.
 *
 * @author Michael Marte
 */
final object IntegerSetValueTraits extends OrderedValueTraits[IntegerSetValue] {
    override val valueType = classOf[IntegerSetValue]
    override def safeDowncast(x: Domain[IntegerSetValue]): IntegerSetDomain = x.asInstanceOf[IntegerSetDomain]
    override def createDomain(values: Set[IntegerSetValue]): IntegerSetDomain = !!!
    override def createDomain(lb: IntegerSetValue, ub: IntegerSetValue): IntegerSetDomain = !!!
    override lazy val emptyDomain: IntegerSetDomain = !!!
    override val completeDomain: IntegerSetDomain = CompleteIntegerSetDomain
}
