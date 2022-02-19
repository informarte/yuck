package yuck.core

/**
 * Represents a scalar-variable pair for use in linear combinations.
 *
 * @author Michael Marte
 */
final case class AX
    [V <: NumericalValue[V]]
    (a: V, x: NumericalVariable[V])
{
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
    def normalize[V <: NumericalValue[V]](axs: Iterable[AX[V]]): List[AX[V]] =
        axs.toSeq.sortBy(_.x).foldLeft(Nil: List[AX[V]]) {
            case (Nil, ax) => ax :: Nil
            case (h :: t, ax) if h.x == ax.x => new AX[V](h.a + ax.a, h.x) :: t
            case (h :: t, ax) => ax :: h :: t
        }

}
