package yuck.annealing

import java.util.concurrent.Callable

import scala.collection._
import scala.math._

import yuck.core._

/**
 * Creates an annealing schedule for a problem of the given size.
 *
 * @author Michael Marte
 */
final class StandardAnnealingScheduleFactory
    (numberOfSearchVariables: Int, randomGenerator: RandomGenerator)
    extends Callable[AnnealingSchedule]
{

    private val numberOfMovesPerVariableAndRound =
        numberOfSearchVariables * (256.0 / (log(numberOfSearchVariables) * log(2))).toInt
    private val finalTemperature = 0.0001
    private val maximumUphillAcceptanceRatio = 0.15
    private val minimumUphillAcceptanceRatio = 0.0001
    private val heatingRate = 1.2
    private val coolingRate = 0.95

    private def createAnnealingSchedule(divisor: Int): AnnealingSchedule = {
        new AnnealingScheduleLoop(
            new AnnealingScheduleSequence(
                Vector(
                    new AdaptiveCoolingSchedule(
                        finalTemperature, minimumUphillAcceptanceRatio, coolingRate,
                        numberOfMovesPerVariableAndRound / divisor, 4 / divisor),
                    new GeometricHeatingSchedule(
                        maximumUphillAcceptanceRatio, heatingRate,
                        numberOfMovesPerVariableAndRound / divisor))),
           4 / divisor)
    }

    override def call = {
        val fastSchedule = createAnnealingSchedule(2)
        val slowSchedule = createAnnealingSchedule(1)
        val schedule = randomGenerator.nextInt(3) match {
            case 0 => fastSchedule
            case 1 => slowSchedule
            case 2 => new AnnealingScheduleSequence(Vector(fastSchedule, slowSchedule))
        }
        def startTemperatures: Stream[Double] = Stream.cons(randomGenerator.nextProbability / 10, startTemperatures)
        val startTemperature = startTemperatures.dropWhile(_ <= finalTemperature).head
        schedule.start(startTemperature, 0)
        schedule
    }

}
