package yuck.annealing

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

    private def createAnnealingSchedule(n: Int, m: Int): AnnealingSchedule = {
        val numberOfMovesPerRound =
            (n * numberOfSearchVariables / (log(numberOfSearchVariables) / log(2))).toInt
        new AnnealingScheduleLoop(
            new AnnealingScheduleSequence(
                Vector(
                    new GeometricHeatingSchedule(
                        DefaultMaximumUphillAcceptanceRatio, DefaultHeatingRate,
                        numberOfMovesPerRound),
                    new AdaptiveCoolingSchedule(
                        DefaultFinalTemperature, DefaultMinimumUphillAcceptanceRatio, DefaultCoolingRate,
                        numberOfMovesPerRound, m))),
            m)
    }

    def createFastSchedule: AnnealingSchedule = createAnnealingSchedule(128, 2)

    def createSlowSchedule: AnnealingSchedule = createAnnealingSchedule(256, 4)

    def createHybridSchedule: AnnealingSchedule =
        new AnnealingScheduleSequence(Vector(createFastSchedule, createSlowSchedule))

}
