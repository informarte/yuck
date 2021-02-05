package yuck.core

/**
 * Value class for type-safe representation of probabilities.
 *
 * Use Probability.from to construct instances.
 *
 * @author Michael Marte
 */
final class Probability private(val value: Double) extends AnyVal {
    override def toString = "%s".format(value)
}

/**
 * Factory for probability values.
 *
 * @author Michael Marte
 */
object Probability {

    /**
     * Turns the given percentage into a probability value.
     *
     * Throws when the given percentage is lower than 0 or greater than 100.
     */
    def apply(percentage: Int): Probability = {
        require(percentage >= 0 && percentage <= 100)
        new Probability(percentage / 100.0)
    }

    /**
     * Turns the given double value into a probability value.
     *
     * Throws when the given double value is lower than 0 or greater than 1.
     */
    def apply(p: Double): Probability = {
        require(p >= 0 && p <= 1)
        new Probability(p)
    }

    def unapply(wrapped: Probability): Option[Double] = Some(wrapped.value)

}
