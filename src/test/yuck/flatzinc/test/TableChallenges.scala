package yuck.flatzinc.test

import java.io.File

import scala.jdk.CollectionConverters._
import org.junit._

import yuck.flatzinc.test.util._

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class TableBenchmarks(task: MiniZincTestTask) extends MiniZincBasedTest {

    @Test
    def solve: Unit = {
        super.solve(task)
    }

}

/**
 * @author Michael Marte
 *
 */
object TableBenchmarks extends MiniZincTestTaskFactory {

    override protected val SuitePath = "resources/mzn/tests/minizinc-benchmarks"
    override protected val MaybeInstancesPerProblem = None
    override protected def problemFilter(file: File): Boolean =
        List("black-hole", "code-generator", "groupsplitter", "is", "opt-cryptanalysis", "proteindesign12", "spot5")
            .contains(file.getName)

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(Array(_)).asJava

}
