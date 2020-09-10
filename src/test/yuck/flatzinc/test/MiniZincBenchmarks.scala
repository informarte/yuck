package yuck.flatzinc.test

import scala.jdk.CollectionConverters._

import org.junit._

import yuck.flatzinc.test.util._

/**
 * Runs Yuck on five instances of each problem of the MiniZinc benchmarks suite.
 *
 * @author Michael Marte
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class MiniZincBenchmarks(task: MiniZincTestTask) extends MiniZincBasedTest {

    @Test
    def solve: Unit = {
        super.solve(task)
    }

}

/**
 * @author Michael Marte
 *
 */
object MiniZincBenchmarks extends MiniZincTestTaskFactory {

    override protected val SuitePath = "resources/mzn/tests/minizinc-benchmarks"
    override protected val MaybeInstancesPerProblem = Some(5)

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(Array(_)).asJava

}
