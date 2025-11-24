package yuck.flatzinc.test.vrp

import yuck.SolvingMethod
import yuck.flatzinc.test.util.{MiniZincTestTaskFactory, ZincTestTask}

abstract class VrpTestTaskFactory extends MiniZincTestTaskFactory {

    override protected val suitePath = "resources/mzn/tests/minizinc-benchmarks"

    override protected val baseTask =
        ZincTestTask(
            solverConfiguration =
                ZincTestTask().solverConfiguration.copy(
                    numberOfSolvers = 1,
                    maybePreferredSolvingMethod = Some(SolvingMethod.SimulatedAnnealing),
                    maybeRuntimeLimitInSeconds = Some(60)),
            keepFlatZincFile = false)

    protected final case class ObjectiveValue(value: Int, isOptimal: Boolean = false)

    protected val results: Map[String, ObjectiveValue]

    protected def amendKnownBestResult(task: ZincTestTask) = {
        val maybeObjectiveValue = results.get(task.instanceName)
        if maybeObjectiveValue.isDefined then {
            val objectiveValue = maybeObjectiveValue.get
            if objectiveValue.isOptimal
            then task.copy(maybeOptimum = Some(objectiveValue.value))
            else task.copy(maybeHighScore = Some(objectiveValue.value))
        } else {
            task
        }
    }

}
