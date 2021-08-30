package yuck.core

/**
 * Represents a scalar-variable pair for use in linear combinations.
 *
 * @author Michael Marte
 */
final case class AX
    [Value <: NumericalValue[Value]]
    (val a: Value, val x: NumericalVariable[Value])
{
    override def hashCode = x.hashCode
    override def equals(that: Any) = that match {
        case rhs: AX[_] => {
            val lhs = this
            // First compare variables to ensure that scalar comparison does not throw due to type mismatch!
            lhs.x == rhs.x && lhs.a == rhs.a
        }
        case _ => false
    }
    override def toString = "%s * %s".format(a, x)
}

/**
 * Provides helper methods for working with [[yuck.core.AX AX]] instances.
 *
 * @author Michael Marte
 */
object AX {

    /**
     * Interprets the given sequence of scalar-variable pairs as linear combination
     * and transforms it into an equivalent linear combination of minimal size.
     */
    def compact[Value <: NumericalValue[Value]](axs: Iterable[AX[Value]]): List[AX[Value]] =
        axs.toSeq.sortBy(_.x).foldLeft(Nil: List[AX[Value]]) {
            case (Nil, ax) => ax :: Nil
            case (h :: t, ax) if h.x == ax.x => new AX[Value](h.a + ax.a, h.x) :: t
            case (h :: t, ax) => ax :: h :: t
        }

}

