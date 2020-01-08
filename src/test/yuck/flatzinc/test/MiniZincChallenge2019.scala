package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * The smallest problems from the MiniZinc challenge 2019
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenge2019 extends MiniZincBasedTest {

    private val task =
        MiniZincTestTask(
            directoryLayout = StandardMiniZincBenchmarksLayout,
            suitePath = "resources/mzn/tests/minizinc-benchmarks",
            suiteName = "mznc19")

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint]))
    def accap_instance3: Unit = {
        solve(task.copy(problemName = "accap", instanceName = "accap_instance3", maybeOptimum = Some(89)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def amaze_2012_03_29: Unit = {
        solve(task.copy(problemName = "amaze", modelName = "amaze3", instanceName = "2012-03-29"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCumulativeConstraint], classOf[HasDecreasingConstraint], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint], classOf[HasMinimumConstraint], classOf[HasTableConstraint]))
    def code_generator_mips_gcc_cfgrtl_update_br_prob_note: Unit = {
        solve(task.copy(problemName = "code-generator", modelName = "unison", instanceName = "gcc.cfgrtl.update_br_prob_note", maybeOptimum = Some(3565)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def fox_geese_corn_06_07_08_00: Unit = {
        solve(task.copy(problemName = "fox-geese-corn", modelName = "foxgeesecorn", instanceName = "fgc_06_07_08_00", maybeOptimum = Some(148)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCountConstraint], classOf[HasTableConstraint]))
    def groupsplitter_u6g1pref1: Unit = {
        solve(task.copy(problemName = "groupsplitter", modelName = "group", instanceName = "u6g1pref1", maybeOptimum = Some(120)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def hrc_exp1_1_5110: Unit = {
        solve(task.copy(problemName = "hrc", instanceName = "exp1-1-5110", maybeOptimum = Some(6)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint], classOf[HasBinPackingConstraint]))
    def kidney_exchange_3_20_0_25_2: Unit = {
        solve(task.copy(problemName = "kidney-exchange", modelName = "ccmcp", instanceName = "3_20_0.25_5", maybeOptimum = Some(1008)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasAlldifferentExcept0Constraint]))
    def liner_sf_repositioning_fm3_0: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "liner-sf-repositioning", instanceName = "fm3_0", maybeOptimum = Some(165922), maybeRuntimeLimitInSeconds = Some(360)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasAtLeastConstraint], classOf[HasAtMostConstraint], classOf[HasGlobalCardinalityConstraint]))
    def lot_sizing_pigment15a_psp: Unit = {
        solve(task.copy(problemName = "lot-sizing", modelName = "lot_sizing_cp", instanceName = "pigment15a.psp", maybeOptimum = Some(1195)))
    }

    @Test
    @Ignore("Has introduced search variables with infinite domains")
    @Category(Array(classOf[MinimizationProblem]))
    def median_string_median_p2_10_8_3: Unit = {
        solve(task.copy(problemName = "median-string", modelName = "median_string_dp", instanceName = "p2_10_8-3", maybeOptimum = Some(36)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasKnapsackConstraint]))
    def multi_knapsack_2_1: Unit = {
        solve(task.copy(problemName = "multi-knapsack", modelName = "mknapsack_global", instanceName = "mknap2-1", maybeOptimum = Some(7772)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCumulativeConstraint], classOf[HasGlobalCardinalityConstraint]))
    def nside_EASY_200_50: Unit = {
        solve(task.copy(problemName = "nside", modelName = "full", instanceName = "EASY_200_50", maybeOptimum = Some(2916), maybeMaximumNumberOfThreads = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def ptv_pax_period_20_2b: Unit = {
        solve(task.copy(problemName = "ptv", modelName = "pax_model", instanceName = "pax_period_20_2b", maybeOptimum = Some(3077)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCumulativeConstraint]))
    def rcpsp_j30_27_5_wet_diverse_3: Unit = {
        solve(task.copy(problemName = "rcpsp-wet-diverse", instanceName = "j30_27_5-wet-diverse-3", maybeOptimum = Some(6708)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasRegularConstraint]))
    def rotating_workforce_Example1242: Unit = {
        solve(task.copy(problemName = "rotating-workforce", instanceName = "Example1242"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def stack_cuttingstock_d5: Unit = {
        solve(task.copy(problemName = "stack-cuttingstock", modelName = "stack-cutstock-cumu", instanceName = "d5", maybeOptimum = Some(19)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint]))
    def steelmillslab_bench_17_7: Unit = {
        solve(task.copy(problemName = "steelmillslab", instanceName = "bench_17_7", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint]))
    def stochastic_vrp_s3_v2_c7_vrp_v2_c7_det: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "stochastic-vrp", instanceName = "vrp-s3-v2-c7_vrp-v2-c7_det", maybeOptimum = Some(230)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def triangular_n17: Unit = {
        solve(task.copy(problemName = "triangular", instanceName = "n17", maybeHighScore = Some(40)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def zephyrus_12_6_6_3: Unit = {
        solve(task.copy(problemName = "zephyrus", instanceName = "12__6__6__3", maybeOptimum = Some(780)))
    }

}
