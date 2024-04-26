package yuck.flatzinc.test.util

import java.util.concurrent.Callable

import scala.collection.*
import scala.jdk.CollectionConverters.*

import yuck.core.{*, given}
import yuck.flatzinc.ast.*
import yuck.flatzinc.compiler.FlatZincCompilerResult
import yuck.flatzinc.runner.*
import yuck.test.util.{DefaultRuntimeLimitInSeconds, ProcessRunner}
import yuck.util.logging.{FineLogLevel, LazyLogger}

/**
 * @author Michael Marte
 *
 */
class MiniZincSolutionVerifier(
    task: ZincTestTask, result: Result, logger: LazyLogger)
    extends Callable[Boolean]
{

    private val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]

    override def call() =
        logger.withTimedLogScope("Checking expectations")(checkExpectations)._1 ||
        logger.withTimedLogScope("Consulting MiniZinc")(consultMiniZinc)._1

    // We compare the solution to the expectations provided by the MiniZinc distribution.
    // Note that some problems have many solutions and only a few are provided in terms of expectations.
    private def checkExpectations: Boolean = {
        val (searchPath, instanceName) = task.directoryLayout match {
            case MiniZincExamplesLayout =>
                ("%s".format(task.suitePath),
                 task.problemName)
            case _ =>
                ("%s/%s".format(task.suitePath, task.problemName),
                 task.instanceName)
        }
        val expectationFiles =
            new java.io.File(searchPath)
            .listFiles
            .toList
            .filter(_.getName.matches("%s\\.exp.*".format(instanceName)))
        logger.log("Found %d expectation files in %s".format(expectationFiles.size, searchPath))
        val actualLines = new FlatZincResultFormatter(result).call()
        val witnessFile = expectationFiles.find(expectationFile => {
            val fileReader = new java.io.BufferedReader(new java.io.FileReader(expectationFile))
            val expectedLines = fileReader.lines.iterator.asScala.toList
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

    // Creates a solution.mzn file that includes the MiniZinc model and contains the assignments to output
    // variables in terms of equality constraints.
    // (The assignments are obtained by piping Yuck's solution through minizinc --ozn-file problem.ozn.
    // This is necessary because the MiniZinc compiler sometimes renames decision variables.)
    // Then runs the MiniZinc tool chain in the tmp dir with the problem dir on the include path.
    // If the solution is ok, Gecode will print the solution followed by the solution separator
    // "----------" to stdout (this behaviour complies to the FlatZinc spec), so we check for this string.
    // To check our objective value, we compare it to the one computed by Gecode.
    // The solution files are not deleted to allow for manual re-checking.
    //
    // Notice that the MiniZinc compiler accepts assignments to decision variables in data files
    // and allows for several data files on the command line. So it seems that we could just
    // dump the assignments to a solution.dzn file and compile it together with the model and
    // the instance data. However, the MiniZinc compiler forbids multiple assignments to the
    // same variable, even if they are identical. This restriction is of concern when the model
    // or the instance data contain an assignment to a decision variable (like array [1..9] of
    // var -100..100: x = [6, 7, _, 8, _, 9, _, 8, 6], for example) and cannot be worked around
    // by equality constraints because data files must not contain constraints.
    private def consultMiniZinc: Boolean = {
        val suitePath = task.suitePath
        val suiteName = if (task.suiteName.isEmpty) new java.io.File(suitePath).getName else task.suiteName
        val problemName = task.problemName
        val modelName = if (task.modelName.isEmpty) problemName else task.modelName
        val instanceName = task.instanceName
        val (includePath, modelFileName, dataFileName, outputDirectoryPath0) = task.directoryLayout match {
            case MiniZincExamplesLayout =>
                ("%s".format(suitePath),
                 "%s.mzn".format(problemName),
                 "",
                 "tmp/%s/%s".format(suiteName, problemName))
            case StandardMiniZincBenchmarksLayout =>
                ("%s/%s".format(suitePath, problemName),
                 "%s.mzn".format(modelName),
                 {
                     val dataFilePath = "%s/%s/%s.dzn".format(suitePath, problemName, instanceName)
                     (if (new java.io.File(dataFilePath).exists()) "%s.dzn" else "%s.json").format(instanceName)
                 },
                 "tmp/%s/%s/%s/%s".format(suiteName, problemName, modelName, instanceName))
            case NonStandardMiniZincBenchmarksLayout =>
                ("%s/%s".format(suitePath, problemName),
                 "%s.mzn".format(instanceName),
                 "",
                 "tmp/%s/%s/%s".format(suiteName, problemName, instanceName))
        }
        val outputDirectoryPath = task.solverConfiguration.maybeName
            .map(name => "%s/%s".format(outputDirectoryPath0, name))
            .getOrElse(outputDirectoryPath0)
        new java.io.File(outputDirectoryPath).mkdirs
        val solutionFilePath =
            if task.verificationFrequency == VerifyEverySolution
            then "%s/solution-%s.mzn".format(outputDirectoryPath, Thread.currentThread.getName)
            else "%s/solution.mzn".format(outputDirectoryPath)
        val solutionWriter = new java.io.FileWriter(solutionFilePath, false /* do not append */)
        val solutionFormatter = new FlatZincResultFormatter(result)
        val solution =
            if task.miniZincCompilerRenamesVariables
            then undoVariableRenamings("%s/problem.ozn".format(outputDirectoryPath), solutionFormatter.call())
            else solutionFormatter.call()
        assert(checkDelimiters(solution), "Issue with delimiters")
        val assignments = solution.takeWhile(_ != FlatZincSolutionSeparator)
        for (assignment <- assignments) {
            solutionWriter.write("constraint %s\n".format(assignment))
        }
        // We include the MiniZinc model in the end because a few of them don't have a semicolon
        // after the last line.
        if (task.verificationModelName.isEmpty) {
            solutionWriter.write("include \"%s\";\n".format(modelFileName))
        } else {
            solutionWriter.write("include \"%s.mzn\";\n".format(task.verificationModelName))
        }
        solutionWriter.close()
        val solver = task.verificationTool match {
            case Chuffed => "org.chuffed.chuffed"
            case Gecode => "org.gecode.gecode"
            case OrTools => "com.google.ortools.sat"
        }
        val miniZincCommand = mutable.ArrayBuffer(
            "minizinc",
            "-v",
            // Tell minizinc where to find the MiniZinc model.
            "-I", includePath,
            // The following directory contains definitions of predicates and functions supported
            // by Yuck but not defined by the standard library.
            "-I", "resources/mzn/lib/verification/yuck",
            "-D", "mzn_ignore_symmetry_breaking_constraints=true",
            "-D", "mzn_ignore_redundant_constraints=true",
            "--solver", solver,
            "--output-mode", "dzn",
            "--output-objective",
            "--statistics",
            // Verification should never take longer than solving.
            "--time-limit", (task.maybeRuntimeLimitInSeconds.getOrElse(DefaultRuntimeLimitInSeconds) * 1000L).toString)
        for ((key, value) <- task.dataAssignments) {
            miniZincCommand ++= List("-D", "%s=%s".format(key, value))
        }
        miniZincCommand += solutionFilePath
        if (! dataFileName.isEmpty) {
            miniZincCommand += "%s/%s".format(includePath, dataFileName)
        }
        val outputLines = new ProcessRunner(logger, miniZincCommand).call()
        assert(! outputLines.contains(FlatZincNoSolutionFoundIndicator), "Solution seems to be underspecified")
        val verified =
            ! outputLines.contains(FlatZincInconsistentProblemIndicator) &&
            checkObjective(outputLines)
        verified
    }

    private def checkDelimiters(outputLines: Seq[String]): Boolean = {
        val separatorIndex = outputLines.indexOf(FlatZincSolutionSeparator)
        val bestSolutionFoundIndicatorIndex = outputLines.indexOf(FlatZincBestSolutionFoundIndicator)
        if (result.objective.isInstanceOf[HierarchicalObjective]) {
            if (result.isOptimal) {
                separatorIndex == outputLines.size - 2 &&
                bestSolutionFoundIndicatorIndex == outputLines.size - 1
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
                checkObjective(outputLines, compilerResult.arrays(id)(idx.toInt - 1))
            case Maximize(Term(id, _), _) =>
                // photo:
                // var 0..17: satisfies :: output_var = INT____00018;
                // solve :: int_search(...) maximize INT____00018;
                checkObjective(outputLines, compilerResult.vars(id))
            case Maximize(ArrayAccess(id, IntConst(idx)), _) =>
                checkObjective(outputLines, compilerResult.arrays(id)(idx.toInt - 1))
        }
    }

    private def checkObjective(outputLines: Seq[String], x: AnyVariable): Boolean = {
        if (compilerResult.space.isSearchVariable(x)) {
            true
        } else {
            val expectation1 = "_objective = "
            if (outputLines.exists(_.startsWith(expectation1))) {
                val expectation2 = "_objective = %s;".format(result.bestProposal.value(x))
                outputLines.contains(expectation2)
            } else {
                logger.log("Could not verify objective value because minizinc did not print it")
                false
            }
        }
    }

    def undoVariableRenamings(oznFilePath: String, solution: Seq[String]): Seq[String] = {
        val commandLine = List("minizinc", "--ozn-file", oznFilePath)
        val processBuilder = new java.lang.ProcessBuilder(commandLine.asJava)
        processBuilder.redirectErrorStream(true) // merge stderr into stdout
        logger.withLogScope(processBuilder.command.asScala.mkString(" ")) {
            val process = processBuilder.start()
            val stdin = new java.io.PrintStream(process.getOutputStream)
            solution.foreach(line => stdin.println(line))
            stdin.flush()
            stdin.close()
            val stdoutReader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
            val outputLines = stdoutReader.lines.iterator.asScala.toList
            outputLines.foreach(logger.log(_))
            val exitCode = process.waitFor()
            assert(exitCode == 0, "Process failed with exit code %d".format(exitCode))
            val (separators, assignments) =
                outputLines.partition(line => line == FlatZincSolutionSeparator || line == FlatZincBestSolutionFoundIndicator)
            val result =
                assignments
                    .mkString // eliminate newlines from assignments (in case 2-dim arrays are pretty printed)
                    .replaceAll("\\s\\s*", " ") // remove superfluous whitespace
                    .split(";") // split at end of assignments
                    .filterNot(_.isEmpty) // remove empty line (in case there is no assignment)
                    .map(_.concat(";")) // put the semicolons back
                    .appendedAll(separators) // finally re-append the separators
            result
        }

    }

}
