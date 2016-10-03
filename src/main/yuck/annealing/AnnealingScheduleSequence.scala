package yuck.annealing

import scala.collection._

/**
 * Runs the given annealing schedules in sequence.
 *
 * When schedule i gets frozen, schedule i + 1 is asked to start out
 * from its predecessor's current temperature.
 *
 * @author Michael Marte
 */
final class AnnealingScheduleSequence(
    children: immutable.IndexedSeq[AnnealingSchedule])
    extends AnnealingSchedule
{

    for (child <- children) {
        require(! child.isFrozen)
    }

    private var i = 0
    private var progress = 0.0

    override def nextRound(roundLog: RoundLog) {
        require(! isFrozen)
        val child = children(i)
        val temperature = child.temperature
        child.nextRound(roundLog)
        if (child.isFrozen) {
            i += 1
            if (i < children.size) {
                children(i).start(temperature, progress)
            }
        }
    }

    override def temperature = {
        require(! isFrozen)
        children(i).temperature
    }

    override def isFrozen =
        i >= children.size

    override def numberOfMonteCarloAttempts = {
        require(! isFrozen)
        children(i).numberOfMonteCarloAttempts
    }

    override def start(temperature: Double, progress: Double) {
        i = 0
        this.progress = progress
        (children(0)).start(temperature, progress)
    }

}
