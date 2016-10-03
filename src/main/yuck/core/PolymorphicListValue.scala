package yuck.core

/**
 * @author Michael Marte
 *
 * PolymorphicListValue instances are to be produced by HierarchicalObjective.costs and
 * to be compared by HierarchicalObjective.compareCosts.
 * 
 * @see HierarchicalObjective 
 */
final class PolymorphicListValue(val value: List[AnyValue]) extends AnyValue {
    override def hashCode = value.hashCode
    override def toString = "(%s)".format(value.map(_.toString).mkString(", "))
    override def equals(that: Any) = that match {
        case rhs: PolymorphicListValue => {
            val lhs = this
            lhs.value.equals(rhs.value)
        }
        case _ => false
    }
}
