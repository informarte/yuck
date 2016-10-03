package yuck.annealing

/**
 * An annealing schedule is a strategy that controls the course of simulated annealing
 * by supplying a temperature for each round.
 *
 * The design of this interface allows for feedback such that the schedule can analyse the
 * progress of search and adjust the temperature accordingly.
 *
 * @author Michael Marte
 */
abstract class AnnealingSchedule {

    /**
     * Tells the schedule that the current round finished with the given round log
     * and that the next round is about to start.
     */
    def nextRound(roundLog: RoundLog): Unit

    /** Returns the temperature for the current round. */
    def temperature: Double

    /** Returns true when the schedule has terminated. */
    def isFrozen: Boolean

    /** Returns the number of moves to generate and consider in the current round. */
    def numberOfMonteCarloAttempts: Int

    /**
     * Tells the schedule to start or restart.
     *
     * Due to the option of restarting, a schedule may be executed several times
     * during its life.
     * For the sake of presentation, let's refer to the time between the
     * call to start until the schedule freezes up (terminates) as a ''run''
     * of the schedule.
     *
     * @param temperature is the temperature to start out from.
     * @param progress is a number in [0, 1] and indicates the progress when running in a loop.
     */
    def start(temperature: Double, progress: Double): Unit

}
