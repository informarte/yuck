package yuck.flatzinc.test.util

import java.util.concurrent.Callable

import yuck.core._
import yuck.flatzinc.ast._
import yuck.flatzinc.compiler.FlatZincCompilerResult
import yuck.flatzinc.runner._
import yuck.util.logging.LazyLogger
import yuck.util.testing.ProcessRunner

/**
 * @author Michael Marte
 *
 */
class MiniZincSolutionVerifier(
    task: MiniZincTestTask, result: Result, logger: LazyLogger)
    extends Callable[Boolean]
{

    private val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]

    override def call =
        logger.withTimedLogScope("Checking expectations")(checkExpectations) ||
        logger.withTimedLogScope("Consulting MiniZinc")(consultMiniZinc)

    // We compare the solution to the expectations provided by the MiniZinc distribution.
    // Note that some problems have many solutions and only a few are provided in terms of expectations.
    private def checkExpectations: Boolean = {
        val (searchPath, instanceName) = task.directoryLayout match {
            case MiniZincExamplesLayout =>
                ("%s/problems".format(task.suitePath),
                 task.problemName)
            case _ =>
                ("%s/problems/%s".format(task.suitePath, task.problemName),
                 task.instanceName)
        }
        val expectationFiles =
            new java.io.File(searchPath)
            .listFiles
            .toList
            .filter(_.getName.matches("%s\\.exp.*".format(instanceName)))
        logger.log("Found %d expectation files in %s".format(expectationFiles.size, searchPath))
        val actualLines = new FlatZincResultFormatter(result).call
        val witnessFile = expectationFiles.find(expectationFile => {
            val expectedLines = scala.io.Source.fromFile(expectationFile).mkString.linesIterator.toList
            actualLines.zip(expectedLines).forall {case (a, b) => a == b}
        })
        val verified = witnessFile.isDefined
        if (verified) {
            logger.log("%s matches".format(witnessFile.get.getName))
        } else {
            logger.log("No expectation file matches")
        }
        verified
    }

    // Creates a solution file that includes the MiniZinc model and that contains the assignments to output
    // variables in terms of equality constraints.
    // Then runs the NICTA MiniZinc tool chain in the tmp dir with the problem dir on the include path.
    // (minizinc will find the included .mzn files there.)
    // If the solution is ok, Gecode will print the solution followed by the solution separator
    // "----------" to stdout (this behaviour complies to the FlatZinc spec), so we check for this string.
    // To check our objective value, we compare it to the one computed by Gecode.
    // The solution files are not deleted to allow for manual re-checking.
    private def consultMiniZinc: Boolean = {
        val suitePath = task.suitePath
        val suiteName = if (task.suiteName.isEmpty) new java.io.File(suitePath).getName else task.suiteName
        val problemName = task.problemName
        val modelName = if (task.modelName.isEmpty) problemName else task.modelName
        val instanceName = task.instanceName
        val (includePath, mznFileName, dznFileName, outputDirectoryPath) = task.directoryLayout match {
            case MiniZincExamplesLayout =>
                ("%s/problems".format(suitePath),
                 "%s.mzn".format(problemName),
                 "",
                 "tmp/%s/%s".format(suiteName, problemName))
            case StandardMiniZincBenchmarksLayout =>
                ("%s/problems/%s".format(suitePath, problemName),
                 "%s.mzn".format(modelName),
                 "%s.dzn".format(instanceName),
                 "tmp/%s/%s/%s/%s".format(suiteName, problemName, modelName, instanceName))
            case NonStandardMiniZincBenchmarksLayout =>
                ("%s/problems/%s".format(suitePath, problemName),
                 "%s.mzn".format(instanceName),
                 "",
                 "tmp/%s/%s/%s".format(suiteName, problemName, instanceName))
        }
        new java.io.File(outputDirectoryPath).mkdirs
        val solutionFilePath = "%s/solution.mzn".format(outputDirectoryPath)
        val solutionWriter = new java.io.FileWriter(solutionFilePath, false /* do not append */)
        val solutionFormatter = new FlatZincResultFormatter(result)
        val solution = solutionFormatter.call
        assert(checkIndicators(solution))
        for (assignment <- solution.toIterator.takeWhile(_ != FlatZincSolutionSeparator)) {
            solutionWriter.write("constraint %s\n".format(assignment))
        }
        // We include the MiniZinc model in the end because a few of them don't have a semicolon
        // after the last line.
        solutionWriter.write("include \"%s\";".format(mznFileName));
        solutionWriter.close
        val minizincCommand = scala.collection.mutable.ArrayBuffer(
            "minizinc",
            "-v",
            // Tell minizinc where to find the MiniZinc model.
            "-I", includePath,
            // To facilitate the verification of solutions found by Yuck, the following directory
            // contains a redefinitions file that causes minizinc to ignore redundant constraints.
            "-I", "resources/mzn/lib/std",
            "--solver", "Gecode",
            "--output-mode", "dzn",
            "--output-objective",
            "--statistics")
        minizincCommand += solutionFilePath
        if (! dznFileName.isEmpty) minizincCommand += "%s/%s".format(includePath, dznFileName)
        val (outputLines, _) = new ProcessRunner(logger, minizincCommand.toList).call
        val verified =
            ! outputLines.contains(FlatZincInconsistentProblemIndicator) &&
            checkObjective(outputLines)
        verified
    }

    private def checkIndicators(outputLines: Seq[String]): Boolean = {
        val separatorIndex = outputLines.indexOf(FlatZincSolutionSeparator)
        val bestSolutionFoundIndicatorIndex = outputLines.indexOf(FlatZincBestSolutionFoundIndicator)
        if (result.objective.isInstanceOf[HierarchicalObjective]) {
            if (result.isGoodEnough) {
                separatorIndex == outputLines.size - 2 &&
                bestSolutionFoundIndicatorIndex == outputLines.size -1
            } else {
                separatorIndex == outputLines.size - 1
            }
        } else {
            separatorIndex == outputLines.size - 1
        }
    }

    private def checkObjective(outputLines: Seq[String]): Boolean = {
        compilerResult.ast.solveGoal match {
            case Satisfy(_) => true
            case Minimize(Term(id, _), _) =>
                // trucking:
                // var 0..600: obj :: output_var = INT____00001;
                // solve minimize INT____00001;
                checkObjective(outputLines, compilerResult.vars(id))
            case Minimize(ArrayAccess(id, IntConst(idx)), _) =>
                // ghoulomb:
                // var 0..81: objective :: output_var = mark2[9];
                // solve  :: int_search(...) minimize mark2[9];
                checkObjective(outputLines, compilerResult.arrays(id)(idx - 1))
            case Maximize(Term(id, _), _) =>
                // photo:
                // var 0..17: satisfies :: output_var = INT____00018;
                // solve :: int_search(...) maximize INT____00018;
                checkObjective(outputLines, compilerResult.vars(id))
            case Maximize(ArrayAccess(id, IntConst(idx)), _) =>
                checkObjective(outputLines, compilerResult.arrays(id)(idx - 1))
        }
    }

    private def checkObjective(outputLines: Seq[String], x: AnyVariable): Boolean = {
        if (compilerResult.space.isSearchVariable(x)) {
            true
        } else {
            val expectation1 = "_objective = "
            if (outputLines.exists(_.startsWith(expectation1))) {
                val expectation2 = "_objective = %s;".format(result.bestProposal.anyValue(x))
                outputLines.contains(expectation2)
            } else {
                logger.log("Could not verify objective value because minizinc did not print it")
                false
            }
        }
    }

}
