package yuck.core

/**
 * Provides traits of integer-set values.
 *
 * @author Michael Marte
 */
final object IntegerSetValueTraits extends OrderedValueTraits[IntegerSetValue] {
    override val valueType = classOf[IntegerSetValue]
    override val orderingCostModel = IntegerSetOrderingCostModel
    override def createDomain(values: Set[IntegerSetValue]): IntegerSetDomain = !!!
    override def createDomain(lb: IntegerSetValue, ub: IntegerSetValue): IntegerSetDomain = !!!
    override lazy val emptyDomain: IntegerSetDomain = !!!
    override val completeDomain: IntegerSetDomain = CompleteIntegerSetDomain
    override val domainPruner = IntegerSetDomainPruner
    override def createVariable(space: Space, name: String, domain: Domain[IntegerSetValue]): IntegerSetVariable =
        new IntegerSetVariable(space.variableIdFactory.nextId, name, safeDowncast(domain))
    override def createChannel(space: Space): IntegerSetVariable =
        new IntegerSetVariable(space.variableIdFactory.nextId, "", completeDomain)
    override def safeDowncast(a: AnyValue): IntegerSetValue = a.asInstanceOf[IntegerSetValue]
    override def safeDowncast(x: AnyDomain): IntegerSetDomain = x.asInstanceOf[IntegerSetDomain]
    override def safeDowncast(x: AnyVariable): IntegerSetVariable = x.asInstanceOf[IntegerSetVariable]
}
