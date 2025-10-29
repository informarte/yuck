package yuck.flatzinc.test

import scala.collection.immutable.{SortedMap, TreeMap}
import scala.jdk.CollectionConverters.*

import org.junit.*
import spray.json.*
import yuck.flatzinc.test.util.*

/**
 * Runs Yuck on five randomly chosen instances for each problem of the MiniZinc benchmarks suite.
 *
 * Prefers satisfiable instances and those with a proven optimum.
 *
 * Exploits known optima to reduce runtime.
 *
 * If a problem has less than five satisfiable instances, then unsatisfiable instances are added
 * to test flattening, parsing, and compilation, but search will not be started on them.
 *
 * @author Michael Marte
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class MiniZincBenchmarks(task: ZincTestTask) extends ZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task)
    }

}

/**
 * @author Michael Marte
 *
 */
object MiniZincBenchmarks extends MiniZincTestTaskFactory {

    override protected val suitePath = "resources/mzn/tests/minizinc-benchmarks"

    private val numberOfInstancesPerProblem = 5

    private val problemBlacklist = List(
        // Heavily modified for VRP research, unusable in this context
        "cvrp",
        // Added for VRP research, never used in challenges
        "cvrptw", "tsptw", "vrp",
        // Have two models with different cost functions, both of which were used in challenges
        "community-detection",
        "league",
        // Doesn't flatten, never used in challenges
        "search_stress2",
        // Has search variables with infinite domains
        "wwtpp-real"
    )

    override protected def problemFilter(file: java.io.File) =
        super.problemFilter(file) && ! problemBlacklist.exists(problemName => file.getName == problemName)

    // Problems with more than one model need special attention.
    private val modelBlacklist = List(
        // Superseded in 2019 by amaze3
        "amaze", "amaze2",
        // Superseded in 2016 by gfd-schedule2
        "gfd-schedule",
        // Incomplete base model, gets included by other models
        "pattern_set_mining",
        // Never used in challenges
        "rect_packing", "rect_packing_opt",
        "ship-schedule.mip",
        // Stand-alone models in a directory with data files
        "zephyrus_15_10", "zephyrus_20_20", "zephyrus_5_20", "zephyrus_5_4", "zephyrus-FH-2-15"
    )

    override protected def modelFilter(file: java.io.File) =
        super.modelFilter(file) && ! modelBlacklist.exists(modelName => file.getName == modelName ++ ".mzn")

    // Problem and instance names sometimes differ across the databases.
    private val nameTranslationTable = Map(
        "fjsp" -> "flexible-jobshop",
        "MZNC_connect" -> "connect",
        "javarouting" -> "java-routing",
        "l2p" -> "linear-to-program",
        "hoist-benchmark-for-minizinc" -> "hoist-scheduling",
        "p1f-pjs" -> "p1f",
        "peacable_queens" -> "peaceable-queens",
        "tower_challenge" -> "tower"
    )

    private def normalizeName(name: String): String =
        nameTranslationTable.getOrElse(name, name.replace('_', '-').toLowerCase())

    case class Summary(
        year: Int,
        problemName: String,
        instanceName: String,
        problemType: String,
        maybeSatisfiable: Option[Boolean],
        maybeMinObjectiveValue: Option[Long],
        maybeMaxObjectiveValue: Option[Long],
        maybeSolvedToOptimality: Option[Boolean])
    {
        def maybeOptimum: Option[Long] =
            problemType match {
                case "SAT" => None
                case "MIN" => if maybeSolvedToOptimality.getOrElse(false) then maybeMinObjectiveValue else None
                case "MAX" => if maybeSolvedToOptimality.getOrElse(false) then maybeMaxObjectiveValue else None
            }
        def maybeHighScore: Option[Long] =
            problemType match {
                case "SAT" => None
                case "MIN" => maybeMinObjectiveValue
                case "MAX" => maybeMaxObjectiveValue
            }
    }

    private val miniZincChallengeResultsPath = "resources/mzn/tests/minizinc-challenge-results.json"

    private def toSummary(value: JsValue): Summary = {
        val JsObject(fields) = value: @unchecked
        val JsNumber(year) = fields("year"): @unchecked
        val JsString(problem) = fields("problem"): @unchecked
        val JsString(instance) = fields("instance"): @unchecked
        val JsString(problemType) = fields("problem-type"): @unchecked
        val maybeSatisfiable = fields.get("satisfiable").map(_.asInstanceOf[JsBoolean].value)
        val maybeMinObjectiveValue = fields.get("min-objective-value").map(_.asInstanceOf[JsNumber].value.toLong)
        val maybeMaxObjectiveValue = fields.get("max-objective-value").map(_.asInstanceOf[JsNumber].value.toLong)
        val maybeSolvedToOptimality = fields.get("solved-to-optimality").map(_.asInstanceOf[JsBoolean].value)
        Summary(
            year.toInt, normalizeName(problem), normalizeName(instance), problemType,
            maybeSatisfiable, maybeMinObjectiveValue, maybeMaxObjectiveValue, maybeSolvedToOptimality)
    }

    private val summaryByProblemAndInstanceNames: SortedMap[String, SortedMap[String, Summary]] = {
        val path = java.nio.file.Paths.get(miniZincChallengeResultsPath)
        val input = java.nio.file.Files.readAllBytes(path)
        val JsArray(elements) = new JsonParser(input).parseJsValue(): @unchecked
        elements
            .map(toSummary)
            .groupBy(_.problemName)
            .view
            .mapValues(_
                .groupBy(_.instanceName)
                .view
                // For some instances, there are two or more reference results; we choose the latest.
                .mapValues(_.maxBy(_.year))
                .to(TreeMap))
            .to(TreeMap)
    }

    private def findSummary(task: ZincTestTask): Option[Summary] =
        summaryByProblemAndInstanceNames
            .get(normalizeName(task.problemName))
            .orElse(summaryByProblemAndInstanceNames.get(normalizeName(task.modelName.split("/").last)))
            .flatMap(_.get(normalizeName(task.instanceName.split("/").last)))

    private val summaryByTask: Map[ZincTestTask, Summary] =
        tasks.iterator
            .map(task => (task, findSummary(task)))
            .collect{case (task, Some(summary)) => (task, summary)}
            .toMap

    private def chooseTasks(tasks: IndexedSeq[ZincTestTask]): IndexedSeq[ZincTestTask] = {
        val n = numberOfInstancesPerProblem
        val (satisfiableTasks, unsatisfiableTasks) =
            tasks.partition(task => summaryByTask(task).maybeSatisfiable.getOrElse(true))
        val chosenSatisfiableTasks =
            randomGenerator
                .shuffle(satisfiableTasks)
                .sortBy(task =>
                    val summary = summaryByTask(task)
                    (
                        // To reduce runtime, we prefer instances with a target objective value.
                        summary.maybeOptimum.orElse(summary.maybeHighScore).isEmpty,
                        // To facilitate feature scaling in later analysis, we prefer instances which provide us
                        // with at least two objective values.
                        summary.maybeMinObjectiveValue == summary.maybeMaxObjectiveValue
                    )
                )
                .take(n)
        val chosenUnsatisfiableTasks =
            randomGenerator.shuffle(unsatisfiableTasks).take(n - chosenSatisfiableTasks.size)
        (chosenSatisfiableTasks ++ chosenUnsatisfiableTasks).sortBy(_.instanceName)
    }

    private val chosenTasksByProblemName: SortedMap[String, Seq[ZincTestTask]] =
        tasks.filter(summaryByTask.contains).groupBy(_.problemName).view.mapValues(chooseTasks).to(TreeMap)

    private def amendTask(task: ZincTestTask): ZincTestTask = {
        val summary = summaryByTask(task)
        if summary.maybeSatisfiable == Some(false)
        then task.copy(maybeNumberOfSolvers = Some(1), maybeRoundLimit = Some(0))
        else if summary.maybeOptimum.isDefined
        then task.copy(maybeOptimum = summary.maybeOptimum)
        else if summary.maybeHighScore.isDefined
        then task.copy(maybeHighScore = summary.maybeHighScore)
        else task
    }

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = chosenTasksByProblemName.valuesIterator.flatten.map(amendTask).map(Array(_)).toVector.asJava

}
