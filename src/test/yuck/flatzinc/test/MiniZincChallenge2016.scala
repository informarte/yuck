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
final class MiniZincChallenge2016 extends MiniZincBasedTest {

    private val task = MiniZincTestTask(directoryLayout = StandardMiniZincBenchmarksLayout, suitePath = "resources/mzn/benchmarks", suiteName = "mznc16")

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def carpet_cutting_02 {
        solve(task.copy(problemName = "carpet-cutting", modelName = "cc_base", instanceName = "mzn_rnd_test.02", maybeOptimum = Some(919)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def celar_6_sub0 {
        solve(task.copy(problemName = "celar", instanceName = "CELAR6-SUB0", maybeOptimum = Some(159)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def cryptanalysis_kb128_n5_obj16 {
        solve(task.copy(problemName = "cryptanalysis", modelName = "step1_aes", instanceName = "kb128_n5_obj16"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def depot_placement_rat99_5 {
        solve(task.copy(problemName = "depot-placement", modelName = "depot_placement", instanceName = "rat99_5", maybeOptimum = Some(107)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def diameterc_mst_s_v20_a50_d4 {
        solve(task.copy(problemName = "diameterc-mst", modelName = "dcmst", instanceName = "s_v20_a50_d4", maybeOptimum = Some(442)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasGlobalCardinalityConstraint], classOf[HasInverseConstraint], classOf[HasMemberConstraint], classOf[HasRegularConstraint]))
    def elitserien_handball20 {
        solve(task.copy(problemName = "elitserien", modelName = "handball", instanceName = "handball20", maybeOptimum = Some(3)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint]))
    def filters_fir_1_3 {
        solve(task.copy(problemName = "filters", modelName = "filter", instanceName = "fir_1_3", maybeOptimum = Some(15)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasGlobalCardinalityConstraint]))
    def gbac_reduced_UD10 {
        solve(task.copy(problemName = "gbac", instanceName = "reduced_UD10-gbac", maybeOptimum = Some(1015)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAtMostConstraint], classOf[HasCumulativeConstraint], classOf[HasNValueConstraint]))
    def gfd_schedule_n25f5d20m10k3 {
        solve(task.copy(problemName = "gfd-schedule", modelName = "gfd-schedule2", instanceName = "n25f5d20m10k3", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasElementConstraint]))
    def java_auto_gen_plusexample_6 {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "java-auto-gen", instanceName = "plusexample_6", maybeOptimum = Some(-43)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasCountConstraint], classOf[HasMaximumConstraint], classOf[HasNetworkFlowCostConstraint]))
    def mapping_mesh2x2_1 {
        solve(task.copy(problemName = "mapping", instanceName = "mesh2x2_1", maybeOptimum = Some(1000)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def maximum_dag_25_04 {
        solve(task.copy(problemName = "maximum-dag", instanceName = "25_04", maybeOptimum = Some(70)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasElementConstraint]))
    def mrcpsp_j30_1_10 {
        solve(task.copy(problemName = "mrcpsp", instanceName = "mm_j30/j30_1_10", maybeOptimum = Some(24)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasNetworkFlowCostConstraint]))
    def nfc_12_2_10 {
        solve(task.copy(problemName = "nfc", instanceName = "12_2_10", maybeOptimum = Some(848)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasIncreasingConstraint]))
    def oocsp_racks_030_e6_cc {
        solve(task.copy(problemName = "oocsp_racks", instanceName = "oocsp_racks_030_e6_cc"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def prize_collecting_28_4_7_1 {
        solve(task.copy(problemName = "prize-collecting", modelName = "pc", instanceName = "28-4-7-1"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def rcpsp_wet_j30_27_5 {
        solve(task.copy(problemName = "rcpsp-wet", instanceName = "j30_27_5-wet", maybeOptimum = Some(84)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def solbat_13_13_5_1 {
        solve(task.copy(problemName = "solbat", modelName = "sb", instanceName = "sb_13_13_5_1"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasSubcircuitConstraint]))
    def tpp_5_3_30_1 {
        solve(task.copy(problemName = "tpp", instanceName = "tpp_5_3_30_1", maybeOptimum = Some(173)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def zephyrus_12_6_8_3 {
        solve(task.copy(problemName = "zephyrus", instanceName = "12__6__8__3", maybeOptimum = Some(1300)))
    }

}
