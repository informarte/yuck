package yuck.test.util

import yuck.annealing.*
import yuck.core.{given, *}

/**
 * @author Michael Marte
 *
 */
abstract class IntegrationTest extends YuckTest {

    protected def createAnnealingSchedule(numberOfSearchVariables: Int, randomGenerator: RandomGenerator): AnnealingSchedule = {
        val scheduleFactory = new StandardAnnealingScheduleFactory(numberOfSearchVariables, randomGenerator.nextGen())
        val schedule = scheduleFactory.createHybridSchedule
        schedule.start(DefaultStartTemperature, 0)
        schedule
    }

}
