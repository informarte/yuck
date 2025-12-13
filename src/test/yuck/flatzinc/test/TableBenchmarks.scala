package yuck.flatzinc.test

import java.io.File

import org.junit.jupiter.api.{MethodOrderer, Test, TestMethodOrder}
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.flatzinc.test.util.*

@TestMethodOrder(classOf[MethodOrderer.MethodName])
@ParameterizedClass
@MethodSource(Array("parameters"))
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

    def parameters = tasks.map(Array(_)).toArray

}
