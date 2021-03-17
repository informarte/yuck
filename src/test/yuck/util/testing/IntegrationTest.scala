package yuck.util.testing

import yuck.annealing._
import yuck.core._

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
