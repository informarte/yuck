package yuck.flatzinc.test.util

import java.util.concurrent.Callable

import scala.collection._

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

    override def call = checkExpectations || consultMiniZinc

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
        val actualLines = new FlatZincResultFormatter(result).call
        val verified = expectationFiles.exists(expectationFile => {
            val expectedLines = scala.io.Source.fromFile(expectationFile).mkString.lines.toList
            actualLines.zip(expectedLines).forall {case (a, b) => a == b}
        })
        verified
    }

    // Creates a solution file that includes the MiniZinc model and that contains the assignments to output
    // variables in terms of equality constraints.
    // Then runs the NICTA MiniZinc tool chain in the tmp dir with the problem dir on the include path.
    // (mzn2fzn will find the included .mzn files there.)
    // If the solution is ok, the NICTA solver will print the solution followed by the solution separator
    // "----------" to stdout (this behaviour complies to the FlatZinc spec), so we check for this string.
    // To check our objective value, we compare it to the one computed by the NICTA solver.
    // To this end, we call mzn2fzn and flatzinc ourselves because the frontends (e.g. mzn-g12fd)
    // do not list the assignments in a machine-readable way but format the output as required by
    // the .mzn file.
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
        for (assignment <- solution.toIterator.takeWhile(_ != FLATZINC_SOLUTION_SEPARATOR)) {
            solutionWriter.write("constraint %s\n".format(assignment))
        }
        // We include the MiniZinc model in the end because a few of them don't have a semicolon
        // after the last line.
        solutionWriter.write("include \"%s\";".format(mznFileName));
        solutionWriter.close
        val flattenedSolutionFilePath = solutionFilePath.replace(".mzn", ".fzn")
        val mzn2fznCommand = mutable.ArrayBuffer(
            "mzn2fzn",
            "-I", includePath,
            // To facilitate the verification of solutions found by Yuck, the following directory
            // contains a redefinitions file that causes mzn2fzn to ignore redundant constraints.
            "-I", "resources/mzn/lib/std",
            "--globals-dir", "g12_fd",
            "--no-output-ozn", "--output-fzn-to-file", flattenedSolutionFilePath)
        mzn2fznCommand += solutionFilePath
        if (! dznFileName.isEmpty) mzn2fznCommand += "%s/%s".format(includePath, dznFileName)
        val flatzincCommand = List("flatzinc", "--backend", "fd", "--solver-stats", flattenedSolutionFilePath)
        val verified =
            checkIndicators(solution) &&
            new ProcessRunner(logger, mzn2fznCommand).call._2.isEmpty && {
                val (outputLines, errorLines) = new ProcessRunner(logger, flatzincCommand).call
                ! outputLines.contains(FLATZINC_INCONSISTENT_PROBLEM_INDICATOR) && checkObjective(outputLines)
            }
        verified
    }

    private def checkObjective(outputLines: Seq[String]): Boolean = {
        compilerResult.ast.solveGoal match {
            case Satisfy(_) => true
            case Minimize(Term(id, _), _) =>
                // trucking:
                // var 0..600: obj :: output_var = INT____00001;
                // solve minimize INT____00001;
                checkAssignment(outputLines, compilerResult.vars(id))
            case Minimize(ArrayAccess(id, IntConst(idx)), _) =>
                // ghoulomb:
                // var 0..81: objective :: output_var = mark2[9];
                // solve  :: int_search(...) minimize mark2[9];
                checkAssignment(outputLines, compilerResult.arrays(id)(idx - 1))
            case Maximize(Term(id, _), _) =>
                // photo:
                // var 0..17: satisfies :: output_var = INT____00018;
                // solve :: int_search(...) maximize INT____00018;
                checkAssignment(outputLines, compilerResult.vars(id))
            case Maximize(ArrayAccess(id, IntConst(idx)), _) =>
                checkAssignment(outputLines, compilerResult.arrays(id)(idx - 1))
         }
    }

    private def checkIndicators(outputLines: Seq[String]): Boolean = {
        val separatorIndex = outputLines.indexOf(FLATZINC_SOLUTION_SEPARATOR)
        val bestSolutionFoundIndicatorIndex = outputLines.indexOf(FLATZINC_BEST_SOLUTION_FOUND_INDICATOR)
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

    private def checkAssignment(outputLines: Seq[String], x: AnyVariable): Boolean = {
        if (compilerResult.space.isSearchVariable(x)) {
            true
        } else {
            val alias = findOutputVariableName(x).getOrElse(x.name)
            val expectation1 = "%s = ".format(alias)
            if (outputLines.exists(_.startsWith(expectation1))) {
                val expectation2 = "%s = %s;".format(alias, result.bestProposal.anyValue(x))
                outputLines.contains(expectation2)
            } else {
                logger.log("Could not verify objective value because flatzinc did not print it")
                true
            }
        }
    }

    // Given the variable x to minimize or maximize, the task is to find the name of this variable
    // in the original formulation as given by the .mzn file.
    // This is not straightforward (x.name) because x may be a replacement for the original variable
    // introduced by the MiniZinc compiler (mzn2fzn) or by our FlatZinc compiler.
    // So we have to scan the AST for an output variable that translates to the given x.
    // Notice that in some problems (like jobshop2x2 and perfsq), the objective variable was not declared
    // as output variable.
    private def findOutputVariableName(x: AnyVariable): Option[String] = {
        var sortedMap = new immutable.TreeMap[String, String]() // id -> value
        var maybeName: Option[String] = None
        for (decl <- compilerResult.ast.varDecls if maybeName.isEmpty) {
            for (annotation <- decl.annotations) {
                annotation match {
                    case Annotation(Term("output_var", Nil)) if (compilerResult.vars(decl.id) == x) =>
                        maybeName = Some(decl.id)
                    case _ =>
                }
            }
        }
        maybeName
    }

}
