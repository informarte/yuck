package yuck.util.testing

import yuck.annealing._
import yuck.core._

/**
 * @author Michael Marte
 *
 */
class IntegrationTest extends YuckTest {

    def createAnnealingSchedule(numberOfSearchVariables: Int, randomGenerator: RandomGenerator): AnnealingSchedule = {
        val scheduleFactory = new StandardAnnealingScheduleFactory(numberOfSearchVariables, randomGenerator.nextGen)
        val schedule = scheduleFactory.createRandomSchedule
        scheduleFactory.startScheduleWithRandomTemperature(schedule)
        schedule
    }

}
