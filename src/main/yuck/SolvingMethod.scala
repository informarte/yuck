package yuck

/**
 * @author Michael Marte
 *
 */
enum SolvingMethod(val abbreviation: String) {
    case SimulatedAnnealing extends SolvingMethod("SA")
    case FeasibilityJump extends SolvingMethod("FJ")

    override def toString = abbreviation
}

/**
 * @author Michael Marte
 *
 */
object SolvingMethod {

    def fromAbbreviation(str: String): SolvingMethod =
        values.find(_.abbreviation == str).getOrElse {
            throw new IllegalArgumentException("Unknown abbreviation: %s".format(str))
        }

}
