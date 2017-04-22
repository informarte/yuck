package yuck.annealing

import java.util.concurrent.Callable

import scala.collection._
import scala.math._

import yuck.core._

/**
 * Creates annealing schedules for problems of the given size.
 *
 * @author Michael Marte
 */
final class StandardAnnealingScheduleFactory
    (numberOfSearchVariables: Int, randomGenerator: RandomGenerator)
{

    private val numberOfMovesPerVariableAndRound =
        (numberOfSearchVariables * 256.0 / (log(numberOfSearchVariables) * log(2))).toInt
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

    def startScheduleWithRandomTemperature(schedule: AnnealingSchedule) {
        def startTemperatures: Stream[Double] = Stream.cons(randomGenerator.nextProbability / 10, startTemperatures)
        val startTemperature = startTemperatures.dropWhile(_ <= finalTemperature).head
        schedule.start(startTemperature, 0)
    }

    def createFastSchedule: AnnealingSchedule = createAnnealingSchedule(2)

    def createSlowSchedule: AnnealingSchedule = createAnnealingSchedule(1)

    def createHybridSchedule: AnnealingSchedule =
        new AnnealingScheduleSequence(Vector(createFastSchedule, createSlowSchedule))

    def createRandomSchedule: AnnealingSchedule =
        randomGenerator.nextInt(3) match {
            case 0 => createFastSchedule
            case 1 => createSlowSchedule
            case 2 => createHybridSchedule
        }

}
