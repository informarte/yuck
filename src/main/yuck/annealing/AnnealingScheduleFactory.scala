package yuck.annealing

import yuck.core.*

/**
 * Creates annealing schedules for problems of the given size.
 */
final class AnnealingScheduleFactory
    (numberOfSearchVariables: Int, randomGenerator: RandomGenerator)
{

    require(numberOfSearchVariables > 0)

    private def createAnnealingSchedule(n: Int, m: Int, performWarmStart: Boolean): AnnealingSchedule = {
        val numberOfMovesPerRound =
            if numberOfSearchVariables == 1
            then n
            else (n * numberOfSearchVariables / ld(numberOfSearchVariables)).round.toInt
        assert(numberOfMovesPerRound > 0)
        val heatingSchedule =
            new GeometricHeatingSchedule(
                DefaultMaximumUphillAcceptanceRatio, DefaultHeatingRate,
                numberOfMovesPerRound)
        val coolingSchedule =
            new AdaptiveCoolingSchedule(
                DefaultFinalTemperature, DefaultMinimumUphillAcceptanceRatio, DefaultCoolingRate,
                numberOfMovesPerRound, m)
        val loop =
            new AnnealingScheduleLoop(
                new AnnealingScheduleSequence(Vector(heatingSchedule, coolingSchedule)),
                m)
        if performWarmStart
        then new AnnealingScheduleSequence(Vector(coolingSchedule, loop))
        else loop
    }

    def createFastSchedule(performWarmStart: Boolean = false): AnnealingSchedule =
        createAnnealingSchedule(128, 2, performWarmStart)

    def createSlowSchedule(performWarmStart: Boolean = false): AnnealingSchedule =
        createAnnealingSchedule(256, 4, performWarmStart)

    def createHybridSchedule(performWarmStart: Boolean = false): AnnealingSchedule =
        new AnnealingScheduleSequence(
            Vector(createFastSchedule(performWarmStart), createSlowSchedule(performWarmStart)))

}
