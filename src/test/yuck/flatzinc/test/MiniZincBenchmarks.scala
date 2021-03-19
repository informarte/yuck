package yuck.flatzinc.test

import java.io.File

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
object MiniZincBenchmarks {

    private val InstancesPerProblem = 5

    private def listFiles(base: File, fileFilter: File => Boolean, recursive: Boolean = true): Seq[File] = {
        val files = base.listFiles.toSeq
        files
            .filter(_.isFile)
            .filter(fileFilter)
            .concat(files.filter(_ => recursive).filter(_.isDirectory).flatMap(listFiles(_, fileFilter)))
    }

    private def mznFilter(file: File) = file.getName.endsWith(".mzn")

    private def dznFilter(file: File) = file.getName.endsWith(".dzn")

    private val tasks = {
        val randomGenerator = new JavaRandomGenerator
        val suitePath = "resources/mzn/tests/minizinc-benchmarks"
        val suiteDir = new File(suitePath)
        assert(suiteDir.exists)
        val problems = suiteDir.listFiles.filter(_.isDirectory).sorted
        var buf = new mutable.ArrayBuffer[MiniZincTestTask]
        for (problem <- problems) {
            val mznFiles = listFiles(problem, mznFilter).sorted
            val dznFiles = listFiles(problem, dznFilter).sorted
            if (dznFiles.isEmpty) {
                val mznFileSelection = randomGenerator.shuffle(mznFiles).take(InstancesPerProblem).sorted
                for (mznFile <- mznFileSelection) {
                    buf +=
                        MiniZincTestTask(
                            directoryLayout = NonStandardMiniZincBenchmarksLayout,
                            suitePath = suitePath,
                            problemName = problem.getName,
                            instanceName = mznFile.getPath.replace(problem.getPath + "/", "").replace(".mzn", ""))
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
                                modelName = mznFile.getPath.replace(problem.getPath + "/", "").replace(".mzn", ""),
                                instanceName = dznFile.getPath.replace(problem.getPath + "/", "").replace(".dzn", ""))
                    }
                }
            }
        }
        buf.toList
    }

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(Array(_)).asJava

}
