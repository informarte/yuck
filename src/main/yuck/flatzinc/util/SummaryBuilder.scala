package yuck.flatzinc.util

import java.time.Duration

import scala.collection.*
import scala.language.implicitConversions

import spray.json.*

import yuck.BuildInfo
import yuck.annealing.AnnealingStatisticsCollector
import yuck.core.*
import yuck.core.profiling.{ConstraintPerformanceMetrics, SpacePerformanceMetrics}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.flatzinc.compiler.FlatZincCompilerResult

/**
 * @author Michael Marte
 *
 */
final class SummaryBuilder {

    import SummaryBuilder.*

    private val rootNode = new JsObjectBuilder
    private val envNode = new JsObjectBuilder
    private val resultNode = new JsObjectBuilder

    private def addEnvNode(): Unit = {
        rootNode += "env" -> envNode
    }

    private def addResultNode(): Unit = {
        rootNode += "result" -> resultNode
    }

    def extendRoot(key: String, value: JsValueBuilder): SummaryBuilder = {
        rootNode += key -> value
        this
    }

    def extendEnv(key: String, value: JsValueBuilder): SummaryBuilder = {
        envNode += key -> value
        addEnvNode()
        this
    }

    def extendResult(key: String, value: JsValueBuilder): SummaryBuilder = {
        resultNode += key -> value
        addResultNode()
        this
    }

    def addOsEnv(): SummaryBuilder = {
        envNode +=
            "os" -> JsObjectBuilder(
                "arch" -> JsString(System.getProperty("os.arch", "")),
                "name" -> JsString(System.getProperty("os.name", "")),
                "version" -> JsString(System.getProperty("os.version", ""))
            )
        addEnvNode()
        this
    }

    def addJavaEnv(): SummaryBuilder = {
        val runtime = java.lang.Runtime.getRuntime
        envNode +=
            "java" -> JsObjectBuilder(
                "runtime" -> JsObjectBuilder(
                    "number-of-virtual-cores" -> JsNumber(runtime.availableProcessors),
                    "max-memory" -> JsNumber(runtime.maxMemory)
                ),
                "vm" -> JsObjectBuilder(
                    "name" -> JsString(System.getProperty("java.vm.name", "")),
                    "version" -> JsString(System.getProperty("java.vm.version", ""))
                ),
                "vendor" -> JsObjectBuilder(
                    "name" -> JsString(System.getProperty("java.vendor", "")),
                    "version" -> JsString(System.getProperty("java.vendor.version", ""))
                ),
                "version" -> JsString(System.getProperty("java.version", ""))
            )
        addEnvNode()
        this
    }

    def addYuckVersion(): SummaryBuilder = {
        rootNode +=
            "solver" -> JsObjectBuilder(
                "name" -> JsString("yuck"),
                "branch" -> JsString(BuildInfo.gitBranch),
                "commit-date" -> JsString(BuildInfo.gitCommitDate),
                "commit-hash" -> JsString(BuildInfo.gitCommitHash),
                "version" -> JsString(BuildInfo.version))
        this
    }

    def addSolverConfiguration(cfg: FlatZincSolverConfiguration): SummaryBuilder = {
        val cfgNode =
            JsObjectBuilder(
                "seed" -> JsNumber(cfg.seed),
                "number-of-solvers" -> JsNumber(cfg.numberOfSolvers),
                "number-of-threads" -> JsNumber(cfg.numberOfThreads),
                "focus-on-top-objective" -> JsBoolean(cfg.focusOnTopObjective),
                "stop-on-first-solution" -> JsBoolean(cfg.stopOnFirstSolution),
                "optimize-array-access" -> JsBoolean(cfg.optimizeArrayAccess),
                "prune-constraint-network" -> JsBoolean(cfg.pruneConstraintNetwork),
                "run-presolver" -> JsBoolean(cfg.runPresolver),
                "use-implicit-solving" -> JsBoolean(cfg.useImplicitSolving),
                "use-progressive-tightening" -> JsBoolean(cfg.useProgressiveTightening),
                "check-assignments-to-non-channel-variables" -> JsBoolean(cfg.checkAssignmentsToNonChannelVariables),
                "delay-cycle-checking-until-initialization" -> JsBoolean(cfg.delayCycleCheckingUntilInitialization)
            )
        if (cfg.maybeRoundLimit.isDefined) {
            cfgNode += "round-limit" -> JsNumber(cfg.maybeRoundLimit.get)
        }
        if (cfg.maybeRuntimeLimitInSeconds.isDefined) {
            cfgNode += "runtime-limit-in-seconds" -> JsNumber(cfg.maybeRuntimeLimitInSeconds.get)
        }
        if (cfg.maybeTargetObjectiveValue.isDefined) {
            cfgNode += "target-objective-value" -> JsNumber(cfg.maybeTargetObjectiveValue.get)
        }
        if (cfg.maybeSpaceProfilingMode.isDefined) {
            cfgNode += "space-profiling-mode" -> JsString(cfg.maybeSpaceProfilingMode.get.toString)
        }
        rootNode +="solver-configuration" -> cfgNode
        this
    }

    def addFlatZincModelStatistics(ast: FlatZincAst, md5Sum: String): SummaryBuilder = {
        rootNode +=
            "flatzinc-model-statistics" -> JsObjectBuilder(
                "md5sum" -> JsString(md5Sum),
                "number-of-predicate-declarations" -> JsNumber(ast.predDecls.size),
                "number-of-parameter-declarations" -> JsNumber(ast.paramDecls.size),
                "number-of-variable-declarations" -> JsNumber(ast.varDecls.size),
                "number-of-constraints" -> JsNumber(ast.constraints.size)
            )
        this
    }

    def addYuckModelStatistics(space: Space): SummaryBuilder = {
        rootNode +=
            "yuck-model-statistics" -> JsObjectBuilder(
                "number-of-search-variables" -> JsNumber(space.searchVariables.size),
                "number-of-implicitly-constrained-search-variables" ->
                    JsNumber(space.implicitlyConstrainedSearchVariables.size),
                "number-of-channel-variables" -> JsNumber(space.channelVariables.size),
                // dangling variables are not readily available
                "number-of-constraints" -> JsNumber(space.numberOfConstraints),
                "number-of-implicit-constraints" -> JsNumber(space.numberOfImplicitConstraints),
                "number-of-layers" -> JsNumber(space.numberOfLayers)
            )
        this
    }

    def addResult(result: Result): SummaryBuilder = {
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        addCompilerStatistics(compilerResult)
        val objectiveVariables = compilerResult.objective.objectiveVariables
        resultNode += "solved" -> JsBoolean(result.isSolution)
        if (! result.isSolution) {
            val costVar = objectiveVariables(0).asInstanceOf[BooleanVariable]
            resultNode += "violation" -> JsNumber(result.bestProposal.value(costVar).violation)
        }
        if (objectiveVariables.size > 1) {
            objectiveVariables(1) match {
                case objectiveVar: IntegerVariable =>
                    resultNode += "objective-value" -> JsNumber(result.bestProposal.value(objectiveVar).value)
                    if (result.isOptimal) {
                        resultNode += "optimal" -> JsBoolean(true)
                    }
                case _ =>
            }
        }
        addResultNode()
        this
    }

    def addParserStatistics(runtime: Duration): SummaryBuilder = {
        rootNode += "parser-statistics" ->
            JsObjectBuilder(
                "runtime-in-seconds" -> JsValueWrapper(JsNumber(runtime.toMillis / 1000.0))
            )
        this
    }

    def addCompilerStatistics(compilerResult: FlatZincCompilerResult): SummaryBuilder = {
        rootNode += "compiler-statistics" ->
            JsObjectBuilder(
                "runtime-in-seconds" -> JsValueWrapper(JsNumber(compilerResult.runtime.toMillis / 1000.0))
            )
        this
    }

    def addSearchStatistics(monitor: AnnealingStatisticsCollector): SummaryBuilder = {
        val statsNode =
            if (monitor.wasSearchRequired) {
                JsObjectBuilder(
                    "number-of-restarts" -> JsNumber(monitor.numberOfRestarts),
                    "moves-per-second" -> JsNumber(monitor.movesPerSecond),
                    "consultations-per-second" -> JsNumber(monitor.consultationsPerSecond),
                    "consultations-per-move" -> JsNumber(monitor.consultationsPerMove),
                    "commitments-per-second" -> JsNumber(monitor.commitmentsPerSecond),
                    "commitments-per-move" -> JsNumber(monitor.commitmentsPerMove)
                )
            } else {
                JsObjectBuilder()
            }
        if (monitor.maybeRuntimeToFirstSolutionInSeconds.isDefined) {
            statsNode += "runtime-to-first-solution-in-seconds" -> JsNumber(monitor.maybeRuntimeToFirstSolutionInSeconds.get)
        }
        if (monitor.maybeRuntimeToBestSolutionInSeconds.isDefined) {
            statsNode += "runtime-to-best-solution-in-seconds" -> JsNumber(monitor.maybeRuntimeToBestSolutionInSeconds.get)
        }
        statsNode += "runtime-in-seconds" -> JsNumber(monitor.runtimeInSeconds)
        if (monitor.maybeArea.isDefined) {
            statsNode += "area" -> JsNumber(monitor.maybeArea.get)
        }
        if (monitor.maybeObjectiveStepFunction.isDefined) {
            val array =
                monitor.maybeObjectiveStepFunction.get.flatMap(
                    step => Vector(JsNumber(step.runtimeInMillis),
                        JsNumber(step.objectiveValue.asInstanceOf[IntegerValue].value)))
            statsNode += "objective-step-function" -> JsArray(array.toVector)
        }
        rootNode += "search-statistics" -> statsNode
        this
    }

    def addSpacePerformanceMetrics(metrics: SpacePerformanceMetrics): SummaryBuilder = {
        val byConstraintNode = convertConstraintPerformanceMetrics(metrics.performanceMetricsByConstraint)
        val metricsNode =
            JsObjectBuilder(
                "number-of-consultations" -> JsNumber(metrics.numberOfConsultations),
                "consultation-effort-in-seconds" -> convertDuration(metrics.consultationEffort),
                "number-of-commitments" -> JsNumber(metrics.numberOfCommitments),
                "commitment-effort-in-seconds" -> convertDuration(metrics.commitmentEffort),
                "by-constraint" -> byConstraintNode
            )
        if (metrics.maybePerformanceMetricsByGoalAndConstraint.isDefined) {
            val byGoalNode = metrics.maybePerformanceMetricsByGoalAndConstraint.get
                .foldLeft(new JsObjectBuilder) {
                    case (builder, (goal, byConstraint)) =>
                        builder += goal.toString -> convertConstraintPerformanceMetrics(byConstraint)
                }
            metricsNode += "by-goal" -> byGoalNode
        }
        rootNode += "space-performance-metrics" -> metricsNode
        this
    }

    def addError(throwable: Throwable): SummaryBuilder =
        extendResult("error", JsObjectBuilder(throwable))

    def addWarning(throwable: Throwable): SummaryBuilder =
        extendResult("warning", JsObjectBuilder(throwable))

    def build(): JsValue = rootNode.build()

}

/**
 * @author Michael Marte
 *
 */
object SummaryBuilder {

    abstract class JsValueBuilder {
        def build(): JsValue
    }

    private case class JsValueWrapper(value: JsValue) extends JsValueBuilder {
        override def build() = value
    }

    implicit def asJsValueBuilder(value: JsValue): JsValueBuilder = JsValueWrapper(value)

    class JsObjectBuilder extends JsValueBuilder {
        val fields = new mutable.HashMap[String, JsValueBuilder]
        override def build() =
            JsObject(fields.iterator.map((name, node) => (name, node.build())).to(immutable.TreeMap))
        def +=(field: (String, JsValueBuilder)): JsObjectBuilder = {
            fields += field
            this
        }
        def ++=(fields: IterableOnce[(String, JsValueBuilder)]): JsObjectBuilder = {
            this.fields ++= fields
            this
        }
    }

    object JsObjectBuilder {
        def apply(fields: (String, JsValueBuilder)*): JsObjectBuilder =
            new JsObjectBuilder ++= fields
        def apply(throwable: Throwable): JsObjectBuilder = {
            val builder = new JsObjectBuilder
            builder += "type" -> JsString(throwable.getClass.getName)
            if (throwable.getMessage.ne(null) && ! throwable.getMessage.isEmpty) {
                builder += "message" -> JsString(throwable.getMessage)
            }
            builder
        }
    }

    def computeMd5Sum(filePath: String): String = {
        import java.math.BigInteger
        import java.nio.file.{Files, Paths}
        import java.security.MessageDigest
        val md = MessageDigest.getInstance("MD5")
        md.update(Files.readAllBytes(Paths.get(filePath)))
        new BigInteger(1, md.digest).toString(16)
    }

    private def convertConstraintPerformanceMetrics(
        byConstraint: immutable.Map[Class[? <: Constraint], ConstraintPerformanceMetrics]): JsValueBuilder =
    {
        byConstraint
            .foldLeft(new JsObjectBuilder) {
                case (builder, (clazz, metrics)) =>
                    builder += clazz.getSimpleName -> convertConstraintPerformanceMetrics(metrics)
            }
    }

    private def convertConstraintPerformanceMetrics(metrics: ConstraintPerformanceMetrics): JsValueBuilder =
        JsObjectBuilder(
            "number-of-consultations" -> JsNumber(metrics.numberOfConsultations),
            "consultation-effort-in-seconds" -> convertDuration(metrics.consultationEffort),
            "number-of-commitments" -> JsNumber(metrics.numberOfCommitments),
            "commitment-effort-in-seconds" -> convertDuration(metrics.commitmentEffort)
        )

    private def convertDuration(duration: Duration): JsNumber =
        JsNumber(duration.getSeconds + duration.getNano / 1e9)

}
