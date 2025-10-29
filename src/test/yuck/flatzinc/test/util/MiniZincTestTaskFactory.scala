package yuck.flatzinc.test.util

import java.io.File

import scala.collection.mutable.ArrayBuffer

import yuck.core.JavaRandomGenerator
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*

/**
 * Generates test tasks from a given test suite.
 *
 * Recursively searches for model and instance files.
 *
 * Supports custom model and instance filters and allows to limit the number of instances
 * per problem.
 *
 * @author Michael Marte
 */
abstract class MiniZincTestTaskFactory {

    protected val randomGenerator = new JavaRandomGenerator

    protected val suitePath: String
    protected val maybeNumberOfInstancesPerProblem: Option[Int] = None
    protected val baseTask: ZincTestTask = ZincTestTask()

    private def listFiles(base: File, fileFilter: File => Boolean, recursive: Boolean = true): Seq[File] = {
        val files = base.listFiles.toSeq
        files
            .filter(_.isFile)
            .filter(fileFilter)
            .concat(files.filter(_ => recursive).filter(_.isDirectory).flatMap(listFiles(_, fileFilter)))
    }

    protected def problemFilter(file: File): Boolean = true

    protected def modelFilter(file: File): Boolean = file.getName.endsWith(".mzn")

    protected def instanceFilter(file: File): Boolean = file.getName.endsWith(".dzn") || file.getName.endsWith(".json")

    protected lazy val tasks: IndexedSeq[ZincTestTask] = {
        val suiteDir = new File(suitePath)
        assert(suiteDir.exists)
        val problems = suiteDir.listFiles.filter(_.isDirectory).filter(problemFilter).sorted
        val buf = new ArrayBuffer[ZincTestTask]
        for (problem <- problems) {
            val modelFiles = listFiles(problem, modelFilter).sorted
            val dataFiles = listFiles(problem, instanceFilter).sorted
            if (dataFiles.isEmpty) {
                val modelFileSelection =
                    randomGenerator.shuffle(modelFiles).take(maybeNumberOfInstancesPerProblem.getOrElse(modelFiles.size)).sorted
                for (modelFile <- modelFileSelection) {
                    buf +=
                        baseTask.copy(
                            directoryLayout = NonStandardMiniZincBenchmarksLayout,
                            suitePath = suitePath,
                            problemName = problem.getName,
                            instanceName = modelFile.getPath.replace(problem.getPath + "/", "").replace(".mzn", "").replace(".json", ""))
                }
            } else {
                val dataFileSelection =
                    randomGenerator.shuffle(dataFiles).take(maybeNumberOfInstancesPerProblem.getOrElse(dataFiles.size)).sorted
                for (modelFile <- modelFiles) {
                    for (dataFile <- dataFileSelection) {
                        buf +=
                            baseTask.copy(
                                directoryLayout = StandardMiniZincBenchmarksLayout,
                                suitePath = suitePath,
                                problemName = problem.getName,
                                modelName = modelFile.getPath.replace(problem.getPath + "/", "").replace(".mzn", ""),
                                instanceName = dataFile.getPath.replace(problem.getPath + "/", "").replace(".dzn", "").replace(".json", ""))
                    }
                }
            }
        }
        buf.toVector
    }

}
