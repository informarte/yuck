package yuck.flatzinc.test.util

import java.io.File

import scala.collection.{Seq, mutable}

import yuck.core.JavaRandomGenerator

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

    protected val SuitePath: String
    protected val MaybeInstancesPerProblem: Option[Int] = None

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

    protected def tasks: List[MiniZincTestTask] = {
        val randomGenerator = new JavaRandomGenerator
        val suiteDir = new File(SuitePath)
        assert(suiteDir.exists)
        val problems = suiteDir.listFiles.filter(_.isDirectory).filter(problemFilter).sorted
        val buf = new mutable.ArrayBuffer[MiniZincTestTask]
        for (problem <- problems) {
            val modelFiles = listFiles(problem, modelFilter).sorted
            val dataFiles = listFiles(problem, instanceFilter).sorted
            if (dataFiles.isEmpty) {
                val modelFileSelection =
                    randomGenerator.shuffle(modelFiles).take(MaybeInstancesPerProblem.getOrElse(modelFiles.size)).sorted
                for (modelFile <- modelFileSelection) {
                    buf +=
                        MiniZincTestTask(
                            directoryLayout = NonStandardMiniZincBenchmarksLayout,
                            suitePath = SuitePath,
                            problemName = problem.getName,
                            instanceName = modelFile.getPath.replace(problem.getPath + "/", "").replace(".mzn", "").replace(".json", ""))
                }
            } else {
                val dataFileSelection =
                    randomGenerator.shuffle(dataFiles).take(MaybeInstancesPerProblem.getOrElse(dataFiles.size)).sorted
                for (modelFile <- modelFiles) {
                    for (dataFile <- dataFileSelection) {
                        buf +=
                            MiniZincTestTask(
                                directoryLayout = StandardMiniZincBenchmarksLayout,
                                suitePath = SuitePath,
                                problemName = problem.getName,
                                modelName = modelFile.getPath.replace(problem.getPath + "/", "").replace(".mzn", ""),
                                instanceName = dataFile.getPath.replace(problem.getPath + "/", "").replace(".dzn", "").replace(".json", ""))
                    }
                }
            }
        }
        buf.toList
    }

}
