package yuck.annealing

import yuck.core.*

/**
 * Creates annealing schedules for problems of the given size.
 *
 * @author Michael Marte
 */
final class AnnealingScheduleFactory
    (numberOfSearchVariables: Int, randomGenerator: RandomGenerator)
{

    require(numberOfSearchVariables > 0)

    private def createAnnealingSchedule(n: Int, m: Int): AnnealingSchedule = {
        val numberOfMovesPerRound =
            if numberOfSearchVariables == 1
            then n
            else (n * numberOfSearchVariables / ld(numberOfSearchVariables)).round.toInt
        assert(numberOfMovesPerRound > 0)
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

    def createFastSchedule(): AnnealingSchedule = createAnnealingSchedule(128, 2)

    def createSlowSchedule(): AnnealingSchedule = createAnnealingSchedule(256, 4)

    def createHybridSchedule(): AnnealingSchedule =
        new AnnealingScheduleSequence(Vector(createFastSchedule(), createSlowSchedule()))

}
