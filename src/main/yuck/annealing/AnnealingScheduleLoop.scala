package yuck.annealing

/**
 * Repeats the given annealing schedule until another run seems pointless.
 *
 * Counts the number of futile runs in succession; the counter is reset
 * when a run improves on the best proposal.
 *
 * Terminates after a given number of futile runs.
 *
 * To repeat the child schedule, it gets restarted with the temperature it had
 * ended with and with a progress proportional to the number of futile repetitions.
 *
 * @author Michael Marte
 */
final class AnnealingScheduleLoop(
    child: AnnealingSchedule,
    maximumNumberOfSuccessiveFutileIterations: Int)
    extends AnnealingSchedule
{

    require(! child.isFrozen)
    require(maximumNumberOfSuccessiveFutileIterations > 0)

    private var numberOfSuccessiveFutileIterations = 0
    private var iterationWasFutile = true

    override def nextRound(roundLog: RoundLog): Unit = {
        require(! isFrozen)
        if (roundLog.bestProposalWasImproved) {
            iterationWasFutile = false
        }
        val temperature = child.temperature
        child.nextRound(roundLog)
        if (child.isFrozen) {
            if (iterationWasFutile) {
                numberOfSuccessiveFutileIterations += 1
            } else {
                numberOfSuccessiveFutileIterations = 0
            }
            if (numberOfSuccessiveFutileIterations < maximumNumberOfSuccessiveFutileIterations) {
                iterationWasFutile = true
                child.start(
                    temperature,
                    (1.0 + numberOfSuccessiveFutileIterations.toDouble) /
                    maximumNumberOfSuccessiveFutileIterations.toDouble)
            }
        }
    }

    override def temperature =
        child.temperature
    override def isFrozen =
       child.isFrozen &&
       numberOfSuccessiveFutileIterations == maximumNumberOfSuccessiveFutileIterations
    override def numberOfMonteCarloAttempts =
        child.numberOfMonteCarloAttempts

    /**
     * Starts the child schedule from the given temperature and with positive progress.
     *
     * Ignores the given progress, so be careful when nesting loops!
     */
    override def start(temperature: Double, progress: Double): Unit = {
        numberOfSuccessiveFutileIterations = 0
        iterationWasFutile = true
        child.start(temperature, 1.0 / maximumNumberOfSuccessiveFutileIterations.toDouble)
    }

}
