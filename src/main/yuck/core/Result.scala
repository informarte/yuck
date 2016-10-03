package yuck.core

/**
 * @param userData can be used to pass information from a preprocessing to a postprocessing phase.
 *
 * @author Michael Marte
 */
class Result(
    val solverName: String,
    val space: Space,
    val objective: AnyObjective,
    val userData: Object)
{
    var bestProposal: SearchState = null
    var costsOfBestProposal: Costs = null
    def isSolution: Boolean =
        objective.isSolution(costsOfBestProposal)
    def isGoodEnough: Boolean =
        objective.isGoodEnough(costsOfBestProposal)
    def isBetterThan(that: Result): Boolean =
        objective.isLowerThan(this.costsOfBestProposal, that.costsOfBestProposal)
}
