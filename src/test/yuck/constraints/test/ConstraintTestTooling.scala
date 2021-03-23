package yuck.constraints.test

import yuck.core.Space
import yuck.test.util.YuckAssert
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
trait ConstraintTestTooling extends YuckAssert {

    protected val logger: LazyLogger

    protected abstract class TestStep {
        def run(space: Space): Unit
    }
    protected case class TestScenario(space: Space, steps: TestStep*)

    protected def runScenario(scenario: TestScenario): Unit = {
        for (step <- scenario.steps) {
            step.run(scenario.space)
        }
    }

}
