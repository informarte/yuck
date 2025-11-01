package yuck.flatzinc.test.util

import scala.language.implicitConversions

import yuck.SolvingMethod
import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*

/**
 * @author Michael Marte
 *
 */
abstract class FrontEndTest extends ZincBasedTest {

    protected val task =
        ZincTestTask(
            directoryLayout = MiniZincExamplesLayout,
            suitePath = "resources/mzn/tests/front-end-tests",
            solverConfiguration =
                ZincTestTask().solverConfiguration.copy(
                    attachGoals = true,
                    numberOfSolvers = 1,
                    maybePreferredSolvingMethod = Some(SolvingMethod.SimulatedAnnealing),
                    maybeRuntimeLimitInSeconds = Some(5)),
            throwWhenUnsolved = true,
            miniZincCompilerRenamesVariables = false,
            createDotFile = true)

    protected final def wasIntroducedByMiniZincCompiler(x: AnyVariable): Boolean =
        x.name.startsWith("X_INTRODUCED")

    protected final def wasIntroducedByYuck(x: AnyVariable): Boolean =
        x.name.isEmpty || x.name == "_YUCK_LB" || x.name == "_YUCK_UB"

    protected final def isUserDefined(x: AnyVariable): Boolean =
        ! wasIntroducedByMiniZincCompiler(x) && ! wasIntroducedByYuck(x)

}
