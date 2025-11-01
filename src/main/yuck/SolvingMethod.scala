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
