package yuck.flatzinc.test

import java.io.File

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.flatzinc.test.util.*

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class TableBenchmarks(task: ZincTestTask) extends ZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task)
    }

}

/**
 * @author Michael Marte
 *
 */
object TableBenchmarks extends MiniZincTestTaskFactory {

    override protected val suitePath = "resources/mzn/tests/minizinc-benchmarks"
    override protected val maybeInstancesPerProblem = None

    override protected def problemFilter(file: File) =
        List("black-hole", "code-generator", "groupsplitter", "is", "opt-cryptanalysis", "proteindesign12", "spot5")
            .contains(file.getName)

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(Array(_)).asJava

}
