package yuck.flatzinc.test

import java.io.File

import org.junit.*

import yuck.flatzinc.test.util.*

@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class TableBenchmarks(task: ZincTestTask) extends ZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task)
    }

}

object TableBenchmarks extends MiniZincTestTaskFactory {

    override protected val suitePath = "resources/mzn/tests/minizinc-benchmarks"
    override protected val maybeNumberOfInstancesPerProblem = None

    override protected def problemFilter(file: File) =
        List("black-hole", "code-generator", "groupsplitter", "is", "opt-cryptanalysis", "proteindesign12", "spot5")
            .contains(file.getName)

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(Array(_)).toArray

}
