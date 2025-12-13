package yuck.flatzinc.test

import java.io.File

import org.junit.jupiter.api.{MethodOrderer, Test, TestMethodOrder}
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.flatzinc.test.util.*

@TestMethodOrder(classOf[MethodOrderer.MethodName])
@ParameterizedClass
@MethodSource(Array("parameters"))
final class RegularBenchmarks(task: ZincTestTask) extends ZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task)
    }

}

object RegularBenchmarks extends MiniZincTestTaskFactory {

    override protected val suitePath = "resources/mzn/tests/minizinc-benchmarks"
    override protected val maybeNumberOfInstancesPerProblem = None

    override protected def problemFilter(file: File) =
        List("generalized-peacable-queens", "peaceable_queens", "rotating-workforce-scheduling", "traveling-tppv")
            .contains(file.getName)

    def parameters = tasks.map(Array(_)).toArray

}
