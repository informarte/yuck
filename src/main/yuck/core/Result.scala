package yuck.core

/**
 * @param maybeUserData can be used to pass information from a preprocessing to a postprocessing phase.
 *
 * @author Michael Marte
 */
abstract class Result {
    val maybeUserData: Option[Object]
    val solverName: String
    val objective: AnyObjective
    val bestProposal: SearchState
    def costsOfBestProposal: Costs =
        objective.costs(bestProposal)
    def isSolution: Boolean =
        objective.isSolution(costsOfBestProposal)
    def isGoodEnough: Boolean =
        objective.isGoodEnough(costsOfBestProposal)
    def isOptimal: Boolean =
        objective.isOptimal(costsOfBestProposal)
    def isBetterThan(that: Result): Boolean =
        objective.isLowerThan(this.costsOfBestProposal, that.costsOfBestProposal)
}
