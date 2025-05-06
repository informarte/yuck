package yuck.flatzinc.test

import java.io.File

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.flatzinc.test.util.*

@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class RegularBenchmarks(task: ZincTestTask) extends ZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task)
    }

}

/**
 * @author Michael Marte
 *
 */
object RegularBenchmarks extends MiniZincTestTaskFactory {

    override protected val suitePath = "resources/mzn/tests/minizinc-benchmarks"
    override protected val maybeNumberOfInstancesPerProblem = None

    override protected def problemFilter(file: File) =
        List("generalized-peacable-queens", "peaceable_queens", "rotating-workforce-scheduling", "traveling-tppv")
            .contains(file.getName)

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(Array(_)).asJava

}
