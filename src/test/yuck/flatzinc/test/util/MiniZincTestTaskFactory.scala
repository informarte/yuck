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

    protected def instanceFilter(file: File): Boolean = file.getName.endsWith(".dzn")

    protected def tasks: List[MiniZincTestTask] = {
        val randomGenerator = new JavaRandomGenerator
        val suiteDir = new File(SuitePath)
        assert(suiteDir.exists)
        val problems = suiteDir.listFiles.filter(_.isDirectory).filter(problemFilter).sorted
        val buf = new mutable.ArrayBuffer[MiniZincTestTask]
        for (problem <- problems) {
            val mznFiles = listFiles(problem, modelFilter).sorted
            val dznFiles = listFiles(problem, instanceFilter).sorted
            if (dznFiles.isEmpty) {
                val mznFileSelection =
                    randomGenerator.shuffle(mznFiles).take(MaybeInstancesPerProblem.getOrElse(mznFiles.size)).sorted
                for (mznFile <- mznFileSelection) {
                    buf +=
                        MiniZincTestTask(
                            directoryLayout = NonStandardMiniZincBenchmarksLayout,
                            suitePath = SuitePath,
                            problemName = problem.getName,
                            instanceName = mznFile.getPath.replace(problem.getPath + "/", "").replace(".mzn", ""))
                }
            } else {
                val dznFileSelection =
                    randomGenerator.shuffle(dznFiles).take(MaybeInstancesPerProblem.getOrElse(dznFiles.size)).sorted
                for (mznFile <- mznFiles) {
                    for (dznFile <- dznFileSelection) {
                        buf +=
                            MiniZincTestTask(
                                directoryLayout = StandardMiniZincBenchmarksLayout,
                                suitePath = SuitePath,
                                problemName = problem.getName,
                                modelName = mznFile.getPath.replace(problem.getPath + "/", "").replace(".mzn", ""),
                                instanceName = dznFile.getPath.replace(problem.getPath + "/", "").replace(".dzn", ""))
                    }
                }
            }
        }
        buf.toList
    }

}
