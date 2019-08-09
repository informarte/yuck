package yuck.flatzinc.test

import java.io.{File, FileFilter}

import scala.collection._
import scala.jdk.CollectionConverters._

import org.junit._

import yuck.core._
import yuck.flatzinc.test.util._

/**
 * Runs Yuck on five instances of each problem of the MiniZinc benchmarks suite.
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class MiniZincBenchmarks(task: MiniZincTestTask) extends MiniZincBasedTest {

    @Test
    def solve: Unit = super.solve(task)
}

/**
 * @author Michael Marte
 *
 */
final object MiniZincBenchmarks {

    private val InstancesPerProblem = 5

    private final class SuffixFilter(suffix: String) extends FileFilter {
        override def accept(file: File) = file.getName.endsWith(suffix)
    }

    private val tasks = {
        val randomGenerator = new JavaRandomGenerator
        val suitePath = "resources/mzn/benchmarks"
        val problemDir = new File(suitePath + "/problems")
        assert(problemDir.exists)
        val problems = problemDir.listFiles.filter(_.isDirectory).sorted
        var buf = new mutable.ArrayBuffer[MiniZincTestTask]
        for (problem <- problems) {
            val mznFiles = problem.listFiles(new SuffixFilter(".mzn")).toList.sorted
            val dznFiles = problem.listFiles(new SuffixFilter(".dzn")).toList.sorted
            if (dznFiles.isEmpty) {
                val mznFileSelection = randomGenerator.shuffle(mznFiles).take(InstancesPerProblem).sorted
                for (mznFile <- mznFileSelection) {
                    buf +=
                        MiniZincTestTask(
                            directoryLayout = NonStandardMiniZincBenchmarksLayout,
                            suitePath = suitePath,
                            problemName = problem.getName,
                            instanceName = mznFile.getName.replace(".mzn", ""))
                }
            } else {
                val dznFileSelection = randomGenerator.shuffle(dznFiles).take(InstancesPerProblem).sorted
                for (mznFile <- mznFiles) {
                    for (dznFile <- dznFileSelection) {
                        buf +=
                            MiniZincTestTask(
                                directoryLayout = StandardMiniZincBenchmarksLayout,
                                suitePath = suitePath,
                                problemName = problem.getName,
                                modelName = mznFile.getName.replace(".mzn", ""),
                                instanceName = dznFile.getName.replace(".dzn", ""))
                    }
                }
            }
        }
        buf.toList
    }

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(Array(_)).asJava

}
