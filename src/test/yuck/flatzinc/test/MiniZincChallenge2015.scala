package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * The smallest problems from the MiniZinc challenge 2016
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenge2015 extends MiniZincBasedTest {

    private val task = MiniZincTestTask(directoryLayout = StandardMiniZincBenchmarksLayout, suitePath = "resources/mzn/benchmarks", suiteName = "mznc15")

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def costas_array_16: Unit = {
        solve(task.copy(problemName = "costas-array", modelName = "CostasArray", instanceName = "16", maybeRuntimeLimitInSeconds = Some(360)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint]))
    def cvrp_simple2: Unit = {
        solve(task.copy(problemName = "cvrp", instanceName = "simple2", maybeOptimum = Some(34)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def freepizza_6: Unit = {
        solve(task.copy(problemName = "freepizza", instanceName = "pizza6", maybeOptimum = Some(210)))
    }

    // The objective variable is dangling.
    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasNValueConstraint], classOf[HasAtMostConstraint]))
    def gfd_schedule_n30f3d30m7k4: Unit = {
        solve(task.copy(problemName = "gfd-schedule", instanceName = "n30f3d30m7k4", maybeOptimum = Some(1)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def grid_colouring_4_8: Unit = {
        solve(task.copy(problemName = "grid-colouring", modelName = "GridColoring", instanceName = "4_8", maybeOptimum = Some(3)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasTableConstraint]))
    def is_jZ9pQqRxJ2: Unit = {
        solve(task.copy(problemName = "is", modelName = "model", instanceName = "jZ9pQqRxJ2", maybeOptimum = Some(210944), maybeRuntimeLimitInSeconds = Some(420)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def largescheduling_0100_1: Unit = {
        solve(task.copy(problemName = "largescheduling", modelName = "largecumulative", instanceName = "instance-0100-1", maybeHighScore = Some(230502)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasCountConstraint], classOf[HasMaximumConstraint], classOf[HasNetworkFlowCostConstraint]))
    def mapping_full2x2: Unit = {
        solve(task.copy(problemName = "mapping", instanceName = "full2x2", maybeOptimum = Some(793)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasKnapsackConstraint]))
    def multi_knapsack_1_6: Unit = {
        solve(task.copy(problemName = "multi-knapsack", modelName = "mknapsack_global", instanceName = "mknap1-6", maybeOptimum = Some(16537)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def nmseq_83: Unit = {
        solve(task.copy(problemName = "nmseq", instanceName = "83"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def opd_small_bibd_11_22_10: Unit = {
        solve(task.copy(problemName = "opd", instanceName = "small_bibd_11_22_10", maybeOptimum = Some(4)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def open_stacks_wbo_10_20_1: Unit = {
        solve(task.copy(problemName = "open_stacks", modelName = "open_stacks_01", instanceName = "wbo_10_20_1", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCircuitConstraint], classOf[HasInverseConstraint]))
    def p1f_12: Unit = {
        solve(task.copy(problemName = "p1f", instanceName = "12", maybeOptimum = Some(484)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def project_planning_12_7: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "project-planning", instanceName = "ProjectPlannertest_12_7", maybeOptimum = Some(17)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def radiation_i7_9: Unit = {
        solve(task.copy(problemName = "radiation", instanceName = "i7-9", maybeOptimum = Some(1007)))
    }

    // The original problem definition was buggy and hence trivial,
    // see https://github.com/MiniZinc/minizinc-benchmarks/pull/1.
    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAtLeastConstraint], classOf[HasAtMostConstraint], classOf[HasExactlyConstraint]))
    def roster_chicroster_dataset_2: Unit = {
        solve(task.copy(problemName = "roster", modelName = "roster_model", instanceName = "chicroster_dataset_2", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def spot5_54: Unit = {
        solve(task.copy(problemName = "spot5", instanceName = "54", maybeOptimum = Some(37)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasInverseConstraint]))
    def tdtsp_10_34_00: Unit = {
        solve(task.copy(problemName = "tdtsp", instanceName = "inst_10_34_00", maybeOptimum = Some(6662)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def triangular_n10: Unit = {
        solve(task.copy(problemName = "triangular", instanceName = "n10", maybeOptimum = Some(20)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def zephyrus_5_4: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "zephyrus", instanceName = "zephyrus_5_4", maybeOptimum = Some(18)))
    }

}
