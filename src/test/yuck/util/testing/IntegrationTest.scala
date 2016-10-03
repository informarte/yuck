package yuck.util.testing

import yuck.annealing._
import yuck.core._

/**
 * @author Michael Marte
 *
 */
class IntegrationTest extends YuckTest {

    def createAnnealingSchedule(numberOfSearchVariables: Int, randomGenerator: RandomGenerator): AnnealingSchedule =
        new StandardAnnealingScheduleFactory(numberOfSearchVariables, randomGenerator).call

}
