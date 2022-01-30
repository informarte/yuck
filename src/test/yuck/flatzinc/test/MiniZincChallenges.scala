package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * A collection of problems from the MiniZinc challenges 2012 - 2020
 *
 * @author Michael Marte
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenges extends MiniZincBasedTest {

    private val task =
        MiniZincTestTask(
            directoryLayout = StandardMiniZincBenchmarksLayout,
            suitePath = "resources/mzn/tests/minizinc-benchmarks",
            suiteName = "minizinc-challenges")

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint]))
    def accap_instance3: Unit = {
        solve(task.copy(problemName = "accap", instanceName = "accap_instance3", maybeOptimum = Some(89)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def amaze2_2012_06_28: Unit = {
        solve(task.copy(problemName = "amaze", modelName = "amaze2", instanceName = "2012-06-28"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def amaze3_2012_03_09: Unit = {
        solve(task.copy(problemName = "amaze", modelName = "amaze3", instanceName = "2012-03-09"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def amaze_2012_03_19: Unit = {
        solve(task.copy(problemName = "amaze", instanceName = "2012-03-19", maybeOptimum = Some(447)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint], classOf[HasTableConstraint]))
    def black_hole_4: Unit = {
        solve(task.copy(problemName = "black-hole", instanceName = "4"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def bnn_planner_sysadmin_4_2s: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "bnn-planner", instanceName = "sysadmin_4_2s", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def cable_tree_wiring_A031: Unit = {
        solve(task.copy(problemName = "cable-tree-wiring", modelName = "ctw", instanceName = "A031", maybeOptimum = Some(24104)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def cargo_01_0s_1913: Unit = {
        solve(task.copy(problemName = "cargo", modelName = "cargo_coarsePiles", instanceName = "challenge01_0s_1913", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def carpet_cutting_02: Unit = {
        solve(task.copy(problemName = "carpet-cutting", modelName = "cc_base", instanceName = "mzn_rnd_test.02", maybeOptimum = Some(919)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def celar_6_sub0: Unit = {
        solve(task.copy(problemName = "celar", instanceName = "CELAR6-SUB0", maybeOptimum = Some(159)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def city_position_5_06: Unit = {
        solve(task.copy(problemName = "city-position", instanceName = "city-5-06", maybeOptimum = Some(3493)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCumulativeConstraint], classOf[HasDecreasingConstraint], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint], classOf[HasMinimumConstraint], classOf[HasTableConstraint]))
    def code_generator_mips_gcc_cfgbuild_control_flow_insn_p: Unit = {
        solve(task.copy(problemName = "code-generator", modelName = "unison", instanceName = "mips_gcc.cfgbuild.control_flow_insn_p", maybeMaximumNumberOfThreads = Some(1), maybeOptimum = Some(274663947)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def collaborative_construction_37: Unit = {
        solve(task.copy(problemName = "collaborative-construction", modelName = "macc", instanceName = "37", maybeMaximumNumberOfThreads = Some(1), maybeOptimum = Some(9)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasValuePrecedeConstraint]))
    def community_detection_Sampson_s10_k3: Unit = {
        solve(task.copy(problemName = "community-detection", instanceName = "Sampson.s10.k3", maybeOptimum = Some(10)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def concert_hall_cap_mznc2018_02: Unit = {
        solve(task.copy(problemName = "concert-hall-cap", instanceName = "concert-cap.mznc2018.02", maybeOptimum = Some(48278)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def costas_array_16: Unit = {
        solve(task.copy(problemName = "costas-array", modelName = "CostasArray", instanceName = "16", maybeRuntimeLimitInSeconds = Some(360)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def crosswords_grid_05_02_dict_55: Unit = {
        solve(task.copy(problemName = "crosswords", modelName = "crossword_opt", instanceName = "grid-05.02_dict-55", maybeOptimum = Some(62)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def cryptanalysis_kb128_n5_obj16: Unit = {
        solve(task.copy(problemName = "cryptanalysis", modelName = "step1_aes", instanceName = "kb128_n5_obj16"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def cvrp_A_n38_k5: Unit = {
        solve(task.copy(problemName = "cvrp", modelName = "cvrp_yuck", instanceName = "Augerat/A/A-n38-k5", dataAssignments = Map(("MaxKToMinKRatio", "1")), maybeOptimum = Some(730)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def cyclic_rcpsp_medium_2: Unit = {
        solve(task.copy(problemName = "cyclic-rcpsp", modelName = "rcmsp", instanceName = "medium_2", maybeOptimum = Some(516)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def depot_placement_rat99_5: Unit = {
        solve(task.copy(problemName = "depot-placement", modelName = "depot_placement", instanceName = "rat99_5", maybeOptimum = Some(107)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def diameterc_mst_s_v20_a50_d4: Unit = {
        solve(task.copy(problemName = "diameterc-mst", modelName = "dcmst", instanceName = "s_v20_a50_d4", maybeOptimum = Some(442)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasInverseConstraint], classOf[HasMemberConstraint], classOf[HasRegularConstraint]))
    def elitserien_handball1: Unit = {
        solve(task.copy(problemName = "elitserien", modelName = "handball", instanceName = "handball1", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def fast_food_3: Unit = {
        solve(task.copy(problemName = "fast-food", modelName = "fastfood", instanceName = "ff3", maybeOptimum = Some(1330)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def fillomino_5x5_1: Unit = {
        solve(task.copy(problemName = "fillomino", instanceName = "5x5_1"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint]))
    def filter_fir_1_1: Unit = {
        solve(task.copy(problemName = "filters", modelName = "filter", instanceName = "fir_1_1", maybeOptimum = Some(18)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def fjsp_easy01: Unit = {
        solve(task.copy(problemName = "flexible-jobshop", modelName = "fjsp", instanceName = "easy01", maybeOptimum = Some(253)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def fox_geese_corn_06_07_08_00: Unit = {
        solve(task.copy(problemName = "fox-geese-corn", modelName = "foxgeesecorn", instanceName = "fgc_06_07_08_00", maybeOptimum = Some(148)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def freepizza_6: Unit = {
        solve(task.copy(problemName = "freepizza", instanceName = "pizza6", maybeOptimum = Some(210)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasGlobalCardinalityConstraint]))
    def gbac_UD2: Unit = {
        solve(task.copy(problemName = "gbac", instanceName = "UD2-gbac", maybeOptimum = Some(146)))
    }

    // The objective variable is dangling.
    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasNValueConstraint], classOf[HasAtMostConstraint]))
    def gfd_schedule_n30f3d30m7k4: Unit = {
        solve(task.copy(problemName = "gfd-schedule", instanceName = "n30f3d30m7k4", maybeOptimum = Some(1)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def ghoulomb_4_9_10: Unit = {
        solve(task.copy(problemName = "ghoulomb", instanceName = "4-9-10", maybeOptimum = Some(33)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def grid_colouring_4_8: Unit = {
        solve(task.copy(problemName = "grid-colouring", modelName = "GridColoring", instanceName = "4_8", maybeOptimum = Some(3)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCountConstraint], classOf[HasTableConstraint]))
    def groupsplitter_u5g1pref0: Unit = {
        solve(task.copy(problemName = "groupsplitter", modelName = "group", instanceName = "u5g1pref0", maybeOptimum = Some(90)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def hoist_scheduling_PU_1_2_2: Unit = {
        solve(task.copy(problemName = "hoist-scheduling", modelName = "hoist-benchmark", instanceName = "PU_1_2_2", maybeOptimum = Some(221)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def hrc_exp1_1_5110: Unit = {
        solve(task.copy(problemName = "hrc", instanceName = "exp1-1-5110", maybeOptimum = Some(6)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasTableConstraint]))
    def is_jZ9pQqRxJ2: Unit = {
        solve(task.copy(problemName = "is", modelName = "model", instanceName = "jZ9pQqRxJ2", maybeOptimum = Some(210944), maybeRuntimeLimitInSeconds = Some(420)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasElementConstraint]))
    def java_auto_gen_plusexample_6: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "java-auto-gen", instanceName = "plusexample_6", maybeOptimum = Some(-43)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasElementConstraint]))
    def java_routing_trip_6_3: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "java-routing", instanceName = "trip_6_3", maybeOptimum = Some(67)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def jp_encoding_data100: Unit = {
        solve(task.copy(problemName = "jp-encoding", instanceName = "data100", maybeOptimum = Some(5338)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint], classOf[HasBinPackingConstraint]))
    def kidney_exchange_3_20_0_25_2: Unit = {
        solve(task.copy(problemName = "kidney-exchange", modelName = "ccmcp", instanceName = "3_20_0.25_5", maybeOptimum = Some(1008)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def largescheduling_0100_1: Unit = {
        solve(task.copy(problemName = "largescheduling", modelName = "largecumulative", instanceName = "instance-0100-1", maybeHighScore = Some(230502)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def league_20_3_5: Unit = {
        solve(task.copy(problemName = "league", instanceName = "model20-3-5", maybeOptimum = Some(49984)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCountConstraint]))
    def linear_to_program_l2p1: Unit = {
        solve(task.copy(problemName = "linear-to-program", instanceName = "l2p1", maybeOptimum = Some(6)))
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
    @Category(Array(classOf[MinimizationProblem]))
    def ma_path_finding_ins_g16_p10_a10: Unit = {
        solve(task.copy(problemName = "ma-path-finding", modelName = "mapf", instanceName = "ins_g16_p10_a10", maybeOptimum = Some(112), maybeMaximumNumberOfThreads = Some(1)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasCountConstraint], classOf[HasMaximumConstraint], classOf[HasNetworkFlowCostConstraint]))
    def mapping_full2x2: Unit = {
        solve(task.copy(problemName = "mapping", instanceName = "full2x2", maybeOptimum = Some(793)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasSubcircuitConstraint]))
    def mario_easy_2: Unit = {
        solve(task.copy(problemName = "mario", instanceName = "mario_easy_2", maybeOptimum = Some(628)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def maximum_dag_25_04: Unit = {
        solve(task.copy(problemName = "maximum-dag", instanceName = "25_04", maybeOptimum = Some(70)))
    }

    @Test
    @Ignore("Has introduced search variables with infinite domains")
    @Category(Array(classOf[MinimizationProblem]))
    def median_string_median_p2_10_8_3: Unit = {
        solve(task.copy(problemName = "median-string", modelName = "median_string_dp", instanceName = "p2_10_8-3", maybeOptimum = Some(36)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def minimal_decision_sets_backache_train4: Unit = {
        solve(task.copy(problemName = "minimal-decision-sets", modelName = "sparse_mds", instanceName = "backache_train4", maybeOptimum = Some(34)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def mqueens_n11: Unit = {
        solve(task.copy(problemName = "mqueens", modelName = "mqueens2", instanceName = "n11", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasElementConstraint]))
    def mrcpsp_j30_15_5: Unit = {
        solve(task.copy(problemName = "mrcpsp", instanceName = "mm_j30/j30_15_5", maybeOptimum = Some(24)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def mspsp_easy_1: Unit = {
        solve(task.copy(problemName = "mspsp", instanceName = "easy_01", maybeOptimum = Some(26)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def multi_knapsack_1_sat: Unit = {
        solve(task.copy(problemName = "multi-knapsack", modelName = "mknapsack", instanceName = "mknap1-6"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasKnapsackConstraint]))
    def multi_knapsack_2_1: Unit = {
        solve(task.copy(problemName = "multi-knapsack", modelName = "mknapsack_global", instanceName = "mknap2-1", maybeOptimum = Some(7772)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def neighbours_1: Unit = {
        solve(task.copy(problemName = "neighbours", modelName = "neighbours-rect", instanceName = "neighbours1", maybeOptimum = Some(63)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasNetworkFlowCostConstraint]))
    def nfc_12_2_10: Unit = {
        solve(task.copy(problemName = "nfc", instanceName = "12_2_10", maybeOptimum = Some(848)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def nmseq_83: Unit = {
        solve(task.copy(problemName = "nmseq", instanceName = "83"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def nonogram_dom_06: Unit = {
        solve(task.copy(problemName = "nonogram", modelName = "non", instanceName = "dom_06"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCumulativeConstraint], classOf[HasGlobalCardinalityConstraint]))
    def nside_EASY_200_50: Unit = {
        solve(task.copy(problemName = "nside", modelName = "full", instanceName = "EASY_200_50", maybeOptimum = Some(2916), maybeMaximumNumberOfThreads = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def on_call_rostering_2s_200d: Unit = {
        solve(task.copy(problemName = "on-call-rostering", modelName = "oc-roster", instanceName = "2s-200d", maybeOptimum = Some(63)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasIncreasingConstraint]))
    def oocsp_racks_030_e6_cc: Unit = {
        solve(task.copy(problemName = "oocsp_racks", instanceName = "oocsp_racks_030_e6_cc"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def opd_flener_et_al_15_350_100: Unit = {
        solve(task.copy(problemName = "opd", instanceName = "flener_et_al_15_350_100", maybeOptimum = Some(24)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def open_stacks_wbo_10_20_1: Unit = {
        solve(task.copy(problemName = "open_stacks", modelName = "open_stacks_01", instanceName = "wbo_10_20_1", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def openshop_gp10_4: Unit = {
        solve(task.copy(problemName = "openshop", instanceName = "gp10-4", maybeOptimum = Some(1077)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def opt_cryptanalysis_r5: Unit = {
        solve(task.copy(problemName = "opt-cryptanalysis", modelName = "mznc2017_aes_opt", instanceName = "r5", maybeOptimum = Some(20)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCircuitConstraint], classOf[HasInverseConstraint]))
    def p1f_10: Unit = {
        solve(task.copy(problemName = "p1f", instanceName = "10", maybeOptimum = Some(300)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def parity_learning_44_22_5_3: Unit = {
        solve(task.copy(problemName = "parity-learning", instanceName = "44_22_5.3", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def pattern_set_mining_anneal_k1: Unit = {
        solve(task.copy(problemName = "pattern-set-mining", modelName = "pattern_set_mining_k1", instanceName = "anneal", maybeOptimum = Some(494)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def pattern_set_mining_k2_audiology: Unit = {
        solve(task.copy(problemName = "pattern-set-mining", modelName = "pattern_set_mining_k2", instanceName = "audiology", maybeOptimum = Some(54)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def pentominoes_int_02: Unit = {
        solve(task.copy(problemName = "pentominoes", modelName = "pentominoes-int", instanceName = "02"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint]))
    def pillars_and_planks: Unit = {
        solve(task.copy(problemName = "pillars-and-planks", modelName = "pillars-planks-solution", instanceName = "p-d12", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def prize_collecting_28_4_7_1: Unit = {
        solve(task.copy(problemName = "prize-collecting", modelName = "pc", instanceName = "28-4-7-1"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def project_planning_12_7: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "project-planning", instanceName = "ProjectPlannertest_12_7", maybeOptimum = Some(17)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def proteindesign12_1PGB_11p_9aa_usingEref_self_x: Unit = {
        solve(task.copy(problemName = "proteindesign12", modelName = "wcsp", instanceName = "1PGB.11p.9aa.usingEref_self_x", maybeOptimum = Some(4151)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def ptv_pax_period_20_2b: Unit = {
        solve(task.copy(problemName = "ptv", modelName = "pax_model", instanceName = "pax_period_20_2b", maybeOptimum = Some(3077)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def racp_j30_13_6_1_25: Unit = {
        solve(task.copy(problemName = "racp", instanceName = "j30_13_6_1.25", maybeOptimum = Some(401)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def radiation_07_07_20: Unit = {
        solve(task.copy(problemName = "radiation", instanceName = "m07_07_20", maybeOptimum = Some(856)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def radiation_i7_9: Unit = {
        solve(task.copy(problemName = "radiation", instanceName = "i7-9", maybeOptimum = Some(1007)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def rcpsp_12: Unit = {
        solve(task.copy(problemName = "rcpsp", instanceName = "12", maybeOptimum = Some(38)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCumulativeConstraint]))
    def rcpsp_j30_27_5_wet_diverse_3: Unit = {
        solve(task.copy(problemName = "rcpsp-wet-diverse", instanceName = "j30_27_5-wet-diverse-3", maybeOptimum = Some(6708)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def rcpsp_wet_j30_1_3: Unit = {
        solve(task.copy(problemName = "rcpsp-wet", instanceName = "j30_1_3-wet", maybeOptimum = Some(93)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def rectangle_packing_18_true: Unit = {
        solve(task.copy(problemName = "rectangle-packing", modelName = "rect_packing_mznc2014", instanceName = "data_mznc2014/rpp18_true"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def rel2onto_3_9: Unit = {
        solve(task.copy(problemName = "rel2onto", instanceName = "3_9", maybeOptimum = Some(70802)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def road_cons_17: Unit = {
        solve(task.copy(problemName = "road-cons", modelName = "road_naive", instanceName = "road_17", maybeMaximumNumberOfThreads = Some(1), maybeOptimum = Some(13560)))
    }

    // The original problem definition was buggy and hence trivial,
    // see https://github.com/MiniZinc/minizinc-benchmarks/pull/1.
    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAtLeastConstraint], classOf[HasAtMostConstraint], classOf[HasExactlyConstraint]))
    def roster_chicroster_dataset_2: Unit = {
        solve(task.copy(problemName = "roster", modelName = "roster_model", instanceName = "chicroster_dataset_2", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasRegularConstraint]))
    def rotating_workforce_Example103: Unit = {
        solve(task.copy(problemName = "rotating-workforce", instanceName = "Example103"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def routing_flexible_GCM_0001: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "routing-flexible", instanceName = "routing_GCM_0001", maybeOptimum = Some(115729)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def rubik_4_cube: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "rubik", instanceName = "4-cube"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def sdn_chain_d10n780_1: Unit = {
        solve(task.copy(problemName = "sdn-chain", modelName = "model", instanceName = "d10n780-1", maybeOptimum = Some(10)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentExcept0Constraint]))
    def seat_moving_10_20_05: Unit = {
        solve(task.copy(problemName = "seat-moving", instanceName = "sm-10-20-05", maybeOptimum = Some(90)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def ship_schedule_3Ships: Unit = {
        solve(task.copy(problemName = "ship-schedule", modelName = "ship-schedule.cp", instanceName = "3Ships", maybeOptimum = Some(265650)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def skill_allocation_1m_1: Unit = {
        solve(task.copy(problemName = "skill-allocation", modelName = "skill_allocation_only", instanceName = "skill_allocation_mzn_1m_1", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def smelt_2: Unit = {
        solve(task.copy(problemName = "smelt", instanceName = "smelt_2", maybeOptimum = Some(69)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def soccer_computational_xIGData_22_12_22_5: Unit = {
        solve(task.copy(problemName = "soccer-computational", modelName = "ecp", instanceName = "xIGData_22_12_22_5"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def solbat_12_12_5_1: Unit = {
        solve(task.copy(problemName = "solbat", modelName = "sb", instanceName = "sb_12_12_5_1"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def spot5_54: Unit = {
        solve(task.copy(problemName = "spot5", instanceName = "54", maybeOptimum = Some(37)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def stable_goods_s_d6: Unit = {
        solve(task.copy(problemName = "stable-goods", modelName = "stable-goods-solution", instanceName = "s-d6", maybeOptimum = Some(6264)))
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
    @Category(Array(classOf[MinimizationProblem]))
    def steiner_tree_es10fst03_stp: Unit = {
        solve(task.copy(problemName = "steiner-tree", instanceName = "es10fst03.stp", maybeOptimum = Some(26003678)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def still_life_wastage_09: Unit = {
        solve(task.copy(problemName = "still-life-wastage", modelName = "still-life", instanceName = "09", maybeOptimum = Some(43)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def stochastic_fjsp_a1_s4_fjsp_t8_j2_m3_a1_det: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "stochastic-fjsp", instanceName = "fjsp-a1-s4_fjsp-t8-j2-m3-a1_det", maybeOptimum = Some(242)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint]))
    def stochastic_vrp_s2_v2_c7: Unit = {
        solve(task.copy(problemName = "stochastic-vrp", instanceName = "s2-v2-c7", maybeOptimum = Some(160)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def tc_graph_color_k10_31: Unit = {
        solve(task.copy(problemName = "tc-graph-color", modelName = "tcgc2", instanceName = "k10_31", maybeOptimum = Some(427)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasInverseConstraint]))
    def tdtsp_10_34_00: Unit = {
        solve(task.copy(problemName = "tdtsp", instanceName = "inst_10_34_00", maybeOptimum = Some(6662)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint], classOf[HasBinPackingConstraint]))
    def team_assignment_data2_5_6: Unit = {
        solve(task.copy(problemName = "team-assignment", modelName = "model", instanceName = "data2_5_6", maybeOptimum = Some(974)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDisjunctiveConstraint]))
    def test_scheduling_t30m10r10_5: Unit = {
        solve(task.copy(problemName = "test-scheduling", instanceName = "t30m10r10-5", maybeOptimum = Some(2785)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def tower_challenge_070_070_15_070_04: Unit = {
        solve(task.copy(problemName = "tower-challenge", modelName = "tower", instanceName = "tower_070_070_15_070-04", maybeOptimum = Some(61)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasSubcircuitConstraint]))
    def tpp_5_3_30_1: Unit = {
        solve(task.copy(problemName = "tpp", instanceName = "tpp_5_3_30_1", maybeOptimum = Some(173)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def train_16: Unit = {
        solve(task.copy(problemName = "train", instanceName = "instance.16", maybeOptimum = Some(93925)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasRegularConstraint]))
    def traveling_tppv_circ8bbal: Unit = {
        solve(task.copy(problemName = "traveling-tppv", modelName = "ttppv", instanceName = "circ8bbal", maybeOptimum = Some(80)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def triangular_n17: Unit = {
        solve(task.copy(problemName = "triangular", instanceName = "n17", maybeHighScore = Some(40)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasCumulativeConstraint]))
    def vrplc_9_5_10_s1: Unit = {
        solve(task.copy(problemName = "vrplc", modelName = "vrplc_service", instanceName = "vrplc9_5_10_s1", maybeOptimum = Some(351)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def whirlpool_d8: Unit = {
        solve(task.copy(problemName = "whirlpool", modelName = "whirlpool-x", instanceName = "d8"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def zephyrus_12_6_8_3: Unit = {
        solve(task.copy(problemName = "zephyrus", instanceName = "12__6__8__3", maybeOptimum = Some(1300)))
    }

}
