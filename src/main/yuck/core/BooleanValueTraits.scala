package yuck.core

/**
 * Provides traits of Boolean values.
 *
 * @author Michael Marte
 */
final object BooleanValueTraits extends OrderedValueTraits[BooleanValue] {
    override val valueType = classOf[BooleanValue]
    override def createDomain(values: Set[BooleanValue]) =
        new BooleanDomain(values.contains(False), values.contains(True))
    override def createDomain(lb: BooleanValue, ub: BooleanValue) = (lb, ub) match {
        case (False, False) => FalseDomain
        case (False, True) => CompleteBooleanDomain
        case (True, True) => TrueDomain
        case (True, False) => EmptyBooleanDomain
    }
    override val emptyDomain = EmptyBooleanDomain
    override val completeDomain = CompleteBooleanDomain
}
