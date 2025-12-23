package yuck.core

/**
 * Represents a scalar-variable pair for use in linear combinations.
 */
final case class AX
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (a: A, x: X)
{
    override def toString = "%s * %s".format(a, x)
}

/**
 * Provides helper methods for working with [[yuck.core.AX AX]] instances.
 */
object AX {

    /**
     * Interprets the given sequence of scalar-variable pairs as linear combination
     * and transforms it into an equivalent linear combination of minimal size.
     */
    def normalize
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (axs: Iterable[AX[A, D, X]]):
        List[AX[A, D, X]] =
        axs.toSeq.sortBy(_.x).foldLeft(Nil: List[AX[A, D, X]]) {
            case (Nil, ax) => ax :: Nil
            case (h :: t, ax) if h.x == ax.x => new AX[A, D, X](h.a + ax.a, h.x) :: t
            case (h :: t, ax) => ax :: h :: t
        }

}
