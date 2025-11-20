package yuck

enum SolvingMethod(val abbreviation: String) {
    case SimulatedAnnealing extends SolvingMethod("SA")
    case FeasibilityJump extends SolvingMethod("FJ")

    override def toString = abbreviation
}

object SolvingMethod {

    def fromAbbreviation(str: String): SolvingMethod =
        values.find(_.abbreviation == str).getOrElse {
            throw new IllegalArgumentException("Unknown abbreviation: %s".format(str))
        }

}
