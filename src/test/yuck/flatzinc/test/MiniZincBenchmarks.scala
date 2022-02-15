package yuck.flatzinc.test

import java.io.File

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.flatzinc.test.util.*

/**
 * Runs Yuck on five instances of each problem of the MiniZinc benchmarks suite.
 *
 * @author Michael Marte
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class MiniZincBenchmarks(task: MiniZincTestTask) extends MiniZincBasedTest {

    @Test
    def solve(): Unit = {
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

    private val incompleteModels =
        List(
            "cvrp", "cvrp_cp", "cvrp_yuck",
            "cvrptw", "cvrptw_cp", "cvrptw_yuck",
            "pattern_set_mining",
            "tsptw",
            "vrp")
    private val brokenModels =
        List(
            "search_stress2",
            "zephyrus_15_10", "zephyrus_20_20", "zephyrus_5_20", "zephyrus_5_4", "zephyrus-FH-2-15")

    override protected def modelFilter(file: File) =
        super.modelFilter(file) &&
            ! (incompleteModels ++ brokenModels).exists(model => file.getName == model ++ ".mzn")

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(Array(_)).asJava

}
