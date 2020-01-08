package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * The smallest problems from the MiniZinc challenge 2014
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenge2014 extends MiniZincBasedTest {

    private val task =
        MiniZincTestTask(
            directoryLayout = StandardMiniZincBenchmarksLayout,
            suitePath = "resources/mzn/tests/minizinc-benchmarks",
            suiteName = "mznc14")

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def amaze_2012_03_09: Unit = {
        solve(task.copy(problemName = "amaze", modelName = "amaze3", instanceName = "2012-03-09"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def cyclic_rcpsp_medium_2: Unit = {
        solve(task.copy(problemName = "cyclic-rcpsp", modelName = "rcmsp", instanceName = "medium_2", maybeOptimum = Some(516)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasInverseConstraint], classOf[HasMemberConstraint], classOf[HasRegularConstraint]))
    def elitserien_handball11: Unit = {
        solve(task.copy(problemName = "elitserien", modelName = "handball", instanceName = "handball11", maybeOptimum = Some(3)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def fillomino_5x5_1: Unit = {
        solve(task.copy(problemName = "fillomino", instanceName = "5x5_1"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def jp_encoding_data100: Unit = {
        solve(task.copy(problemName = "jp-encoding", instanceName = "data100", maybeOptimum = Some(5338)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasAlldifferentExcept0Constraint]))
    def liner_sf_repositioning_tp7_0: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "liner-sf-repositioning", instanceName = "tp7_0", maybeOptimum = Some(125988), maybeRuntimeLimitInSeconds = Some(360)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasSubcircuitConstraint]))
    def mario_easy_5: Unit = {
        solve(task.copy(problemName = "mario", instanceName = "mario_easy_5", maybeOptimum = Some(445)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def mqueens_n11: Unit = {
        solve(task.copy(problemName = "mqueens", modelName = "mqueens2", instanceName = "n11", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def multi_knapsack_1_6: Unit = {
        solve(task.copy(problemName = "multi-knapsack", modelName = "mknapsack", instanceName = "mknap1-6"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def openshop_gp10_4: Unit = {
        solve(task.copy(problemName = "openshop", instanceName = "gp10-4", maybeOptimum = Some(1077)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def rectangle_packing_18_true: Unit = {
        solve(task.copy(problemName = "rectangle-packing", modelName = "rect_packing_mznc2014", instanceName = "data_mznc2014/rpp18_true"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def road_cons_11: Unit = {
        solve(task.copy(problemName = "road-cons", modelName = "road_naive", instanceName = "road_11", maybeOptimum = Some(5020)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def ship_schedule_3Ships: Unit = {
        solve(task.copy(problemName = "ship-schedule", modelName = "ship-schedule.cp", instanceName = "3Ships", maybeOptimum = Some(265650)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def smelt_2: Unit = {
        solve(task.copy(problemName = "smelt", instanceName = "smelt_2", maybeOptimum = Some(69)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def solbat_13_13_6_5: Unit = {
        solve(task.copy(problemName = "solbat", modelName = "sb", instanceName = "sb_13_13_6_5"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def spot5_29: Unit = {
        solve(task.copy(problemName = "spot5", instanceName = "29", maybeOptimum = Some(8059)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def stochastic_fjsp_a1_s4_fjsp_t8_j2_m3_a1_det: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "stochastic-fjsp", instanceName = "fjsp-a1-s4_fjsp-t8-j2-m3-a1_det", maybeOptimum = Some(242)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint]))
    def stochastic_vrp_s2_v2_c7_vrp_v2_c7_det: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "stochastic-vrp", instanceName = "vrp-s2-v2-c7_vrp-v2-c7_det", maybeOptimum = Some(160)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def train_1: Unit = {
        solve(task.copy(problemName = "train", instanceName = "instance.1", maybeOptimum = Some(6630)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasRegularConstraint]))
    def traveling_tppv_circ8bbal: Unit = {
        solve(task.copy(problemName = "traveling-tppv", modelName = "ttppv", instanceName = "circ8bbal", maybeOptimum = Some(80)))
    }

}
