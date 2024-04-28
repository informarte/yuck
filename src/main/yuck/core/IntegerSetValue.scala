package yuck.core

import scala.annotation.tailrec

/**
 * Implements immutable integer-set values.
 *
 * @author Michael Marte
 */
final class IntegerSetValue(val set: IntegerDomain) extends OrderedValue[IntegerSetValue] {
    import IntegerSetValue.*
    override def hashCode = set.hashCode
    override def equals(that: Any) = that match {
        case rhs: IntegerSetValue =>
            val lhs = this
            lhs.eq(rhs) || lhs.set.eq(rhs.set) || lhs.set == rhs.set
        case _ => false
    }
    override def compare(that: IntegerSetValue) = ValueListOrdering.compare(this.set, that.set)
    override def toString = set.toString
    inline override def ==(that: Value[IntegerSetValue]) = this == that.asInstanceOf[IntegerSetValue]
    inline def ==(that: IntegerSetValue): Boolean = this.set == that.set
    inline def !=(that: IntegerSetValue): Boolean = this.set != that.set
}

/**
 * Companion object to IntegerSetValue.
 *
 * @author Michael Marte
 */
object IntegerSetValue {

    given ordering: Ordering[IntegerSetValue] = IntegerSetValueOrdering
    given traits: OrderedValueTraits[IntegerSetValue] = IntegerSetValueTraits

    @tailrec
    private def compareRangeLists(lhs: IndexedSeq[IntegerRange], rhs: IndexedSeq[IntegerRange], i: Int): Int =
        if (i == lhs.size && i == rhs.size) 0
        else if (i == lhs.size) -1
        else if (i == rhs.size) 1
        else if (lhs(i).startsBefore(rhs(i))) -1
        else if (rhs(i).startsBefore(lhs(i))) 1
        else if (lhs(i).endsBefore(rhs(i))) (if (i == lhs.size - 1) -1 else 1)
        else if (rhs(i).endsBefore(lhs(i))) (if (i == rhs.size - 1) 1 else -1)
        else compareRangeLists(lhs, rhs, i + 1)

    // The lexicographic ordering on sorted lists of domain elements.
    private val ValueListOrdering = new Ordering[IntegerDomain] {
        override def compare(lhs: IntegerDomain, rhs: IntegerDomain) = (lhs, rhs) match {
            case (lhs: IntegerRange, rhs: IntegerRange) =>
                if (lhs.isEmpty && rhs.isEmpty) 0
                else if (lhs.isEmpty) -1
                else if (rhs.isEmpty) 1
                else if (lhs.startsBefore(rhs)) -1
                else if (rhs.startsBefore(lhs)) 1
                else if (lhs.endsBefore(rhs)) -1
                else if (rhs.endsBefore(lhs)) 1
                else 0
            case _ =>
                import IntegerDomain.ensureRangeList
                compareRangeLists(ensureRangeList(lhs).ranges, ensureRangeList(rhs).ranges, 0)
        }
    }

}
