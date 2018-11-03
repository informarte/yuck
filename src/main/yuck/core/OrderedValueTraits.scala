package yuck.core

/**
 * Provides properties of ordered values.
 *
 * @author Michael Marte
 */
abstract class OrderedValueTraits[Value <: OrderedValue[Value]] extends AnyValueTraits[Value] {

    /** A total ordering induced by Value.compare. */
    implicit final val valueOrdering: Ordering[Value] = new Ordering[Value] {
        override def compare(x: Value, y: Value) = x.compare(y)
    }

    /** A total ordering induced by OrderedDomain[Value].compare. */
    implicit final val domainOrdering: Ordering[OrderedDomain[Value]] = new Ordering[OrderedDomain[Value]] {
        override def compare(x: OrderedDomain[Value], y: OrderedDomain[Value]) = x.compare(y)
    }

    /** The cost model for ordering operations over Value. */
    val orderingCostModel: OrderingCostModel[Value]

    /** Creates a domain from the given bounds. */
    def createDomain(lb: Value, ub: Value): OrderedDomain[Value]

    /** Casts the given domain to an ordered domain over Value. */
    def safeDowncast(x: Domain[Value]): OrderedDomain[Value] =
        x.asInstanceOf[OrderedDomain[Value]]

}
