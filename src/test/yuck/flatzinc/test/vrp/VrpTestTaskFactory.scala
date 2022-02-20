package yuck.flatzinc.test.vrp

import yuck.flatzinc.test.util.{MiniZincTestTask, MiniZincTestTaskFactory}

/**
 * @author Michael Marte
 *
 */
abstract class VrpTestTaskFactory extends MiniZincTestTaskFactory {

    override protected val SuitePath = "resources/mzn/tests/minizinc-benchmarks"

    protected final case class ObjectiveValue(value: Int, isOptimal: Boolean = false)

    protected val Results: Map[String, ObjectiveValue]

    protected def amendKnownBestResult(task: MiniZincTestTask) = {
        val maybeObjectiveValue = Results.get(task.instanceName)
        if (maybeObjectiveValue.isDefined) {
            val objectiveValue = maybeObjectiveValue.get
            if (objectiveValue.isOptimal) task.copy(maybeOptimum = Some(objectiveValue.value))
            else task.copy(maybeHighScore = Some(objectiveValue.value))
        } else {
            task
        }
    }

}
