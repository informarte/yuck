package yuck.core

/**
 * PolymorphicListValue instances are to be produced by HierarchicalObjective.costs and
 * to be compared by HierarchicalObjective.compareCosts.
 *
 * @see [[yuck.core.HierarchicalObjective]]
 *
 * @author Michael Marte
 */
final class PolymorphicListValue(val value: List[AnyValue]) extends AnyValue {
    override def hashCode = value.hashCode
    override def toString = "(%s)".format(value.mkString(", "))
    override def equals(that: Any) = that match {
        case rhs: PolymorphicListValue =>
            val lhs = this
            lhs.value.equals(rhs.value)
        case _ => false
    }
}
