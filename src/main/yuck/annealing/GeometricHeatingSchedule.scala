package yuck.annealing

/**
 * Increases the temperature after every round by multiplying it with the given heating rate.
 *
 * Freezes up when the solver (that uses this schedule) has reported a successful round
 * or when "enough" uphill moves were accepted during the last round, that is when the
 * acceptance ratio has reached the current target ratio.
 *
 * The target acceptance ratio (for uphill moves) is set to a given maximum during
 * construction and may be changed by an invocation of start.
 *
 * @author Michael Marte
 */
final class GeometricHeatingSchedule(
    maximumUphillAcceptanceRatio: Double,
    heatingRate: Double,
    override val numberOfMonteCarloAttempts: Int)
    extends AnnealingSchedule
{

    require(maximumUphillAcceptanceRatio >= 0.0 && maximumUphillAcceptanceRatio <= 1.0)
    require(heatingRate > 1.0)

    private var currentTemperature = 1.0
    private var targetUphillAcceptanceRatio = maximumUphillAcceptanceRatio
    private var lastRoundLog: RoundLog = null

    override def nextRound(roundLog: RoundLog): Unit = {
        require(! isFrozen)
        lastRoundLog = roundLog
        if (! isFrozen) {
            currentTemperature *= heatingRate
        }
    }

    override def temperature =
        currentTemperature

    override def isFrozen =
        lastRoundLog != null &&
        (lastRoundLog.bestProposalWasImproved ||
         lastRoundLog.uphillAcceptanceRatio >= targetUphillAcceptanceRatio)

    /**
     * Prepares the schedule for the next run.
     *  - Multiplies the given temperature with the heating rate to obtain the
     *    temperature for the first round of the next run.
     *  - Multiplies the given progress with the maximum acceptance ratio (for
     *    uphill moves) to obtain the target acceptance ratio for the next run.
     */
    override def start(temperature: Double, progress: Double): Unit = {
        require(temperature >= 0.0)
        require(progress > 0.0 && progress <= 1.0)
        currentTemperature = temperature * heatingRate
        targetUphillAcceptanceRatio = maximumUphillAcceptanceRatio * progress
        lastRoundLog = null
    }

}
