package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.*
import org.junit.experimental.categories.Categories.*

import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*
import yuck.flatzinc.test.util.VerificationFrequency.*

/**
 * A collection of problems from the MiniZinc challenges 2012 - 2023
 *
 * Goals of this test suite:
 * <ul>
 *   <li>Test toolchain e2e including verification of solutions</li>
 *   <li>Measure performance</li>
 * </ul>
 *
 * Rules for choosing instances for a given problem:
 * <ul>
 *   <li>From the solvable instances, choose a long-running and big instance.</li>
 *   <li>If all solvable instances are very easy to solve, add a solvable and an unsolvable instance.</li>
 *   <li>If there is no solvable instance, choose an easy one - easy according to the official challenge results.</li>
 * </ul>
 *
 * @author Michael Marte
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenges extends ZincBasedTest {

    private val task =
        ZincTestTask(
            directoryLayout = StandardMiniZincBenchmarksLayout,
            suitePath = "resources/mzn/tests/minizinc-benchmarks",
            suiteName = "minizinc-challenges",
            maybeRuntimeLimitInSeconds = Some(150))

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint]))
    def accap_instance9(): Unit = {
        solve(task.copy(problemName = "accap", instanceName = "accap_instance9", maybeHighScore = Some(525)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def aircraft_B737NG_600_01_anon(): Unit = {
        solve(task.copy(problemName = "aircraft-disassembly", modelName = "aircraft", instanceName = "B737NG-600-01-Anon.json", maybeOptimum = Some(3297500)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def amaze_2012_03_19(): Unit = {
        solve(task.copy(problemName = "amaze", instanceName = "2012-03-19", maybeOptimum = Some(447)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def amaze2_2012_06_28(): Unit = {
        solve(task.copy(problemName = "amaze", modelName = "amaze2", instanceName = "2012-06-28"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def amaze3_2012_03_09(): Unit = {
        solve(task.copy(problemName = "amaze", modelName = "amaze3", instanceName = "2012-03-09"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentExceptConstraint], classOf[HasCountConstraint]))
    def arithmetic_target_6872_with_1_2_3_3_4_4_5_6_7_9_10(): Unit = {
        solve(task.copy(problemName = "arithmetic-target", modelName = "model", instanceName = "6872_with_1_2_3_3_4_4_5_6_7_9_10", maybeOptimum = Some(7)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def atsp_12_6_8_3(): Unit = {
        solve(task.copy(problemName = "atsp", instanceName = "instance10_0p25", maybeOptimum = Some(3711320)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint], classOf[HasTableConstraint]))
    def black_hole_20(): Unit = {
        solve(task.copy(problemName = "black-hole", instanceName = "20"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasIncreasingConstraint]))
    def blocks_world_16_4_5(): Unit = {
        solve(task.copy(problemName = "blocks-world", modelName = "blocks", instanceName = "16-4-5", maybeOptimum = Some(25)))
    }


    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def bnn_planner_navigation_5x5_10s(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "bnn-planner", instanceName = "navigation_5x5_10s", maybeOptimum = Some(8)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def cable_tree_wiring_R046(): Unit = {
        solve(task.copy(problemName = "cable-tree-wiring", modelName = "ctw", instanceName = "R046", maybeOptimum = Some(143534)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def cargo_01_0s_1913(): Unit = {
        solve(task.copy(problemName = "cargo", modelName = "cargo_coarsePiles", instanceName = "challenge01_0s_1913", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def carpet_cutting_02(): Unit = {
        solve(task.copy(problemName = "carpet-cutting", modelName = "cc_base", instanceName = "mzn_rnd_test.02", maybeOptimum = Some(919)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def celar_6_sub0(): Unit = {
        solve(task.copy(problemName = "celar", instanceName = "CELAR6-SUB0", maybeOptimum = Some(159)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def chessboard_4(): Unit = {
        solve(task.copy(problemName = "chessboard", instanceName = "chessboard4", maybeOptimum = Some(25)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def city_position_5_06(): Unit = {
        solve(task.copy(problemName = "city-position", instanceName = "city-5-06", maybeOptimum = Some(3493)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint], classOf[HasMinimumConstraint], classOf[HasTableConstraint]))
    def code_generator_mips_gcc_cfgbuild_control_flow_insn_p(): Unit = {
        solve(task.copy(problemName = "code-generator", modelName = "unison", instanceName = "mips_gcc.cfgbuild.control_flow_insn_p", maybeMaximumNumberOfThreads = Some(1), maybeOptimum = Some(274663947)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def collaborative_construction_37(): Unit = {
        solve(task.copy(problemName = "collaborative-construction", modelName = "macc", instanceName = "37", maybeMaximumNumberOfThreads = Some(1), maybeOptimum = Some(9)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasValuePrecedeConstraint]))
    def community_detection_dolphin_s62_k3(): Unit = {
        solve(task.copy(problemName = "community-detection", instanceName = "dolphin.s62.k3", maybeHighScore = Some(71)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasGlobalCardinalityConstraint]))
    def community_detection_rnd_n100_e50_s50_d30_c9_p30(): Unit = {
        solve(task.copy(problemName = "community-detection-rnd", modelName = "community-detection", instanceName = "rnd_n100_e50_s50_d30_c9_p30", maybeOptimum = Some(2963)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasGlobalCardinalityConstraint]))
    def compression_bin_8(): Unit = {
        solve(task.copy(problemName = "compression", instanceName = "bin_8", maybeOptimum = Some(26)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def concert_hall_cap_06(): Unit = {
        solve(task.copy(problemName = "concert-hall-cap", instanceName = "concert-cap.mznc2018.06", maybeOptimum = Some(31985)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def connect_0018(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "connect", instanceName = "connect__0018", maybeOptimum = Some(52490)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def costas_array_16(): Unit = {
        solve(task.copy(problemName = "costas-array", modelName = "CostasArray", instanceName = "16", maybeRuntimeLimitInSeconds = Some(360)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def crosswords_grid_05_04_dict_80(): Unit = {
        solve(task.copy(problemName = "crosswords", modelName = "crossword_opt", instanceName = "grid-05.04_dict-80", maybeOptimum = Some(88)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def cryptanalysis_kb128_n5_obj16(): Unit = {
        solve(task.copy(problemName = "cryptanalysis", modelName = "step1_aes", instanceName = "kb128_n5_obj16"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def cvrp_A_n45_k6(): Unit = {
        solve(task.copy(problemName = "cvrp", modelName = "cvrp_yuck", instanceName = "Augerat/A/A-n45-k6", dataAssignments = Map(("MaxKToMinKRatio", "1")), maybeOptimum = Some(944)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def cyclic_rcpsp_medium_2(): Unit = {
        solve(task.copy(problemName = "cyclic-rcpsp", modelName = "rcmsp", instanceName = "medium_2", maybeOptimum = Some(516)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def depot_placement_st70_6(): Unit = {
        solve(task.copy(problemName = "depot-placement", modelName = "depot_placement", instanceName = "st70_6", maybeOptimum = Some(206)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def diameterc_mst_s_v20_a50_d4(): Unit = {
        solve(task.copy(problemName = "diameterc-mst", modelName = "dcmst", instanceName = "s_v20_a50_d4", maybeOptimum = Some(442)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCumulativeConstraint], classOf[HasInverseConstraint], classOf[HasMemberConstraint], classOf[HasRegularConstraint]))
    def elitserien_handball1(): Unit = {
        solve(task.copy(problemName = "elitserien", modelName = "handball", instanceName = "handball1", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasGlobalCardinalityConstraint]))
    def evm_super_compilation_isolated_block_6_0_input(): Unit = {
        solve(task.copy(problemName = "evm-super-compilation", modelName = "evmopt-generic", instanceName = "isolated_block_6_0_input", maybeOptimum = Some(13)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def fast_food_61(): Unit = {
        solve(task.copy(problemName = "fast-food", modelName = "fastfood", instanceName = "ff61", maybeOptimum = Some(152)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def fillomino_07(): Unit = {
        solve(task.copy(problemName = "fillomino", instanceName = "07"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint]))
    def filter_fir_1_1(): Unit = {
        solve(task.copy(problemName = "filters", modelName = "filter", instanceName = "fir_1_1", maybeOptimum = Some(18)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def fjsp_med04(): Unit = {
        solve(task.copy(problemName = "flexible-jobshop", modelName = "fjsp", instanceName = "med04", maybeHighScore = Some(2642)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def flowshop_workers_5stat_ex3(): Unit = {
        solve(task.copy(problemName = "flowshop-workers", instanceName = "5stat_ex3", maybeOptimum = Some(2751)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def fox_geese_corn_06_07_08_00(): Unit = {
        solve(task.copy(problemName = "fox-geese-corn", modelName = "foxgeesecorn", instanceName = "fgc_06_07_08_00", maybeOptimum = Some(148)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def freepizza_27(): Unit = {
        solve(task.copy(problemName = "freepizza", instanceName = "pizza27", maybeHighScore = Some(703530)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentExceptConstraint], classOf[HasIncreasingConstraint]))
    def gametes_nl7_m10_134(): Unit = {
        solve(task.copy(problemName = "gametes", instanceName = "gamete_nl7_m10_134", maybeOptimum = Some(3)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasGlobalCardinalityConstraint]))
    def gbac_UD2(): Unit = {
        solve(task.copy(problemName = "gbac", instanceName = "UD2-gbac", maybeOptimum = Some(146)))
    }

    // The objective variable is dangling.
    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasNValueConstraint], classOf[HasAtMostConstraint]))
    def gfd_schedule_n30f3d30m7k4(): Unit = {
        solve(task.copy(problemName = "gfd-schedule", instanceName = "n30f3d30m7k4", maybeOptimum = Some(1)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasRegularConstraint]))
    def generalized_peacable_queens_n25_q4(): Unit = {
        solve(task.copy(problemName = "generalized-peacable-queens", modelName = "peaceable_queens", instanceName = "n25_q4", maybeHighScore = Some(29)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def ghoulomb_4_9_10(): Unit = {
        solve(task.copy(problemName = "ghoulomb", instanceName = "4-9-10", maybeOptimum = Some(33)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def graph_clear_planar_n20_seed2022_14(): Unit = {
        solve(task.copy(problemName = "graph-clear", modelName = "graph_clear_cp", instanceName = "planar_n20_seed2022_14", maybeOptimum = Some(54)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def grid_colouring_10_5(): Unit = {
        solve(task.copy(problemName = "grid-colouring", modelName = "GridColoring", instanceName = "10_5", maybeOptimum = Some(3)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCountConstraint], classOf[HasTableConstraint]))
    def groupsplitter_u5g1pref0(): Unit = {
        solve(task.copy(problemName = "groupsplitter", modelName = "group", instanceName = "u5g1pref0", maybeOptimum = Some(90)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDecreasingConstraint]))
    def harmony_brother(): Unit = {
        solve(task.copy(problemName = "harmony", instanceName = "brother", maybeOptimum = Some(36)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def hoist_scheduling_PU_3_2_1(): Unit = {
        solve(task.copy(problemName = "hoist-scheduling", instanceName = "PU_3_2_1", maybeHighScore = Some(720)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def hrc_exp2_1_5180(): Unit = {
        solve(task.copy(problemName = "hrc", instanceName = "exp2-1-5180", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasTableConstraint]))
    def is_jZ9pQqRxJ2(): Unit = {
        solve(task.copy(problemName = "is", modelName = "model", instanceName = "jZ9pQqRxJ2", maybeOptimum = Some(210944), maybeRuntimeLimitInSeconds = Some(420)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasElementConstraint]))
    def java_auto_gen_routing_7(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "java-auto-gen", instanceName = "routing_7", maybeOptimum = Some(53)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasElementConstraint]))
    def java_routing_trip_6_3(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "java-routing", instanceName = "trip_6_3", maybeOptimum = Some(67)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def jp_encoding_data100(): Unit = {
        solve(task.copy(problemName = "jp-encoding", instanceName = "data100", maybeOptimum = Some(5338)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint], classOf[HasBinPackingConstraint]))
    def kidney_exchange_3_25_0_20_3(): Unit = {
        solve(task.copy(problemName = "kidney-exchange", modelName = "ccmcp", instanceName = "3_25_0.20_3", maybeOptimum = Some(1448)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def largescheduling_0100_1(): Unit = {
        solve(task.copy(problemName = "largescheduling", modelName = "largecumulative", instanceName = "instance-0100-1", maybeHighScore = Some(230502)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def league_105_18_14(): Unit = {
        solve(task.copy(problemName = "league", instanceName = "model105-18-14", maybeHighScore = Some(349909)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCountConstraint]))
    def linear_to_program_l2p1(): Unit = {
        solve(task.copy(problemName = "linear-to-program", instanceName = "l2p1", maybeOptimum = Some(6)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasAlldifferentExceptConstraint]))
    def liner_sf_repositioning_tp7_0(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "liner-sf-repositioning", instanceName = "tp7_0", maybeOptimum = Some(125988)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasAtLeastConstraint], classOf[HasAtMostConstraint], classOf[HasGlobalCardinalityConstraint]))
    def lot_sizing_pigment15a_psp(): Unit = {
        solve(task.copy(problemName = "lot-sizing", modelName = "lot_sizing_cp", instanceName = "pigment15a.psp", maybeOptimum = Some(1195)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def ma_path_finding_ins_g16_p10_a10(): Unit = {
        solve(task.copy(problemName = "ma-path-finding", modelName = "mapf", instanceName = "ins_g16_p10_a10", maybeOptimum = Some(112), maybeMaximumNumberOfThreads = Some(1)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasCountConstraint], classOf[HasMaximumConstraint], classOf[HasNetworkFlowCostConstraint]))
    def mapping_full2x2(): Unit = {
        solve(task.copy(problemName = "mapping", instanceName = "full2x2", maybeOptimum = Some(793)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasSubcircuitConstraint]))
    def mario_easy_2(): Unit = {
        solve(task.copy(problemName = "mario", instanceName = "mario_easy_2", maybeOptimum = Some(628)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def maximum_dag_31_02(): Unit = {
        solve(task.copy(problemName = "maximum-dag", instanceName = "31_02", maybeOptimum = Some(89)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def median_string_p1_15_20_1(): Unit = {
        solve(task.copy(problemName = "median-string", modelName = "median_string_dp", instanceName = "p1_15_20-1", maybeHighScore = Some(100)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def minimal_decision_sets_backache_train4(): Unit = {
        solve(task.copy(problemName = "minimal-decision-sets", modelName = "sparse_mds", instanceName = "backache_train4", maybeOptimum = Some(34)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def monitor_placement_1id_hop_counting_based_zoo_Forthnet(): Unit = {
        solve(task.copy(problemName = "monitor-placement-1id", modelName = "monitor_1id", instanceName = "hop_counting_based_zoo_Forthnet", maybeOptimum = Some(52)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def monomatch_data_n_6_percentage_0_5(): Unit = {
        solve(task.copy(problemName = "monomatch", instanceName = "data_n_6_percentage_0.5"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def mqueens_n11(): Unit = {
        solve(task.copy(problemName = "mqueens", modelName = "mqueens2", instanceName = "n11", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasElementConstraint]))
    def mrcpsp_j30_15_5(): Unit = {
        solve(task.copy(problemName = "mrcpsp", instanceName = "mm_j30/j30_15_5", maybeOptimum = Some(24)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def mspsp_hard_08(): Unit = {
        solve(task.copy(problemName = "mspsp", instanceName = "hard_08", maybeOptimum = Some(31)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def multi_agent_graph_coverage_rnd_n10_p1500_c15_s1(): Unit = {
        solve(task.copy(problemName = "multi-agent-graph-coverage", modelName = "graph-scan-revised", instanceName = "rnd_n10_p1500_c15_s1", maybeOptimum = Some(147)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def multi_knapsack_1_6(): Unit = {
        solve(task.copy(problemName = "multi-knapsack", modelName = "mknapsack", instanceName = "mknap1-6"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasKnapsackConstraint]))
    def multi_knapsack_2_1(): Unit = {
        solve(task.copy(problemName = "multi-knapsack", modelName = "mknapsack_global", instanceName = "mknap2-1", maybeOptimum = Some(7772)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def neighbours_5(): Unit = {
        solve(task.copy(problemName = "neighbours", modelName = "neighbours-rect", instanceName = "neighbours5", maybeHighScore = Some(149)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def network_50_cstr_MODEL1507180015(): Unit = {
        solve(task.copy(problemName = "network_50_cstr", modelName = "efm_cstr", instanceName = "MODEL1507180015", maybeOptimum = Some(3)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasNetworkFlowCostConstraint]))
    def nfc_24_4_10(): Unit = {
        solve(task.copy(problemName = "nfc", instanceName = "24_4_10", maybeOptimum = Some(1912)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def nmseq_83(): Unit = {
        solve(task.copy(problemName = "nmseq", instanceName = "83"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def nonogram_dom_06(): Unit = {
        solve(task.copy(problemName = "nonogram", modelName = "non", instanceName = "dom_06"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def nonogram_non_med_4(): Unit = {
        solve(task.copy(problemName = "nonogram", modelName = "non", instanceName = "non_med_4"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCumulativeConstraint], classOf[HasGlobalCardinalityConstraint]))
    def nside_EASY_200_50(): Unit = {
        solve(task.copy(problemName = "nside", modelName = "full", instanceName = "EASY_200_50", maybeOptimum = Some(2916), maybeMaximumNumberOfThreads = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def on_call_rostering_30s_400d_A(): Unit = {
        solve(task.copy(problemName = "on-call-rostering", modelName = "oc-roster", instanceName = "30s-400d-A", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasIncreasingConstraint]))
    def oocsp_racks_030_ea4_cc(): Unit = {
        solve(task.copy(problemName = "oocsp_racks", instanceName = "oocsp_racks_030_ea4_cc"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def opd_flener_et_al_15_350_100(): Unit = {
        solve(task.copy(problemName = "opd", instanceName = "flener_et_al_15_350_100", maybeOptimum = Some(24)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def open_stacks_wbop_15_30_1(): Unit = {
        solve(task.copy(problemName = "open_stacks", modelName = "open_stacks_01", instanceName = "wbop_15_30_1", maybeOptimum = Some(6)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def openshop_gp10_4(): Unit = {
        solve(task.copy(problemName = "openshop", instanceName = "gp10-4", maybeOptimum = Some(1077)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def opt_cryptanalysis_r11(): Unit = {
        solve(task.copy(problemName = "opt-cryptanalysis", modelName = "mznc2017_aes_opt", instanceName = "r11", maybeOptimum = Some(46)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCircuitConstraint], classOf[HasInverseConstraint]))
    def p1f_10(): Unit = {
        solve(task.copy(problemName = "p1f", instanceName = "10", maybeOptimum = Some(300)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def parity_learning_44_22_5_3(): Unit = {
        solve(task.copy(problemName = "parity-learning", instanceName = "44_22_5.3", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def pattern_set_mining_k1_vehicle(): Unit = {
        solve(task.copy(problemName = "pattern-set-mining", modelName = "pattern_set_mining_k1", instanceName = "vehicle", maybeHighScore = Some(148)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def pattern_set_mining_k2_audiology(): Unit = {
        solve(task.copy(problemName = "pattern-set-mining", modelName = "pattern_set_mining_k2", instanceName = "audiology", maybeOptimum = Some(54)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasRegularConstraint]))
    def peaceable_queens_40(): Unit = {
        solve(task.copy(problemName = "peaceable_queens", instanceName = "40", maybeHighScore = Some(222)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def pennies_opt_n5(): Unit = {
        // The solution cannot be verified because the model uses opt variables.
        solve(task.copy(problemName = "pennies-opt", modelName = "pennies", instanceName = "n5", maybeOptimum = Some(5), verificationFrequency = NoVerification))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def pentominoes_int_02(): Unit = {
        solve(task.copy(problemName = "pentominoes", modelName = "pentominoes-int", instanceName = "02"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def pentominoes_zayenz_size_5_tiles_20_seed_17_strategy_close(): Unit = {
        solve(task.copy(problemName = "pentominoes-zayenz", modelName = "pentominoes", instanceName = "size_5_tiles_20_seed_17_strategy_close"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def pentominoes_zayenz_size_10_tiles_10_seed_17_strategy_target(): Unit = {
        solve(task.copy(problemName = "pentominoes-zayenz", modelName = "pentominoes", instanceName = "size_10_tiles_10_seed_17_strategy_target"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def perfect_square_57(): Unit = {
        solve(task.copy(problemName = "perfect_square", instanceName = "57"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasNValueConstraint]))
    def physician_scheduling_instance03_0_34(): Unit = {
        solve(task.copy(problemName = "physician-scheduling", instanceName = "instance03_0.34", maybeOptimum = Some(1505)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint]))
    def pillars_and_planks(): Unit = {
        solve(task.copy(problemName = "pillars-and-planks", modelName = "pillars-planks-solution", instanceName = "p-d12", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def prize_collecting_28_4_7_1(): Unit = {
        solve(task.copy(problemName = "prize-collecting", modelName = "pc", instanceName = "28-4-7-1"))
    }


    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def project_planning_12_7(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "project-planning", instanceName = "ProjectPlannertest_12_7", maybeOptimum = Some(17)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def portal_random_10_9_10(): Unit = {
        solve(task.copy(problemName = "portal", instanceName = "random_10_9_10", maybeOptimum = Some(9)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def proteindesign12_1PGB_11p_9aa_usingEref_self_x(): Unit = {
        solve(task.copy(problemName = "proteindesign12", modelName = "wcsp", instanceName = "1PGB.11p.9aa.usingEref_self_x", maybeOptimum = Some(4151)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def ptv_pax_period_20_5a(): Unit = {
        solve(task.copy(problemName = "ptv", modelName = "pax_model", instanceName = "pax_period_20_5a", maybeOptimum = Some(50)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def racp_j30_13_6_1_25(): Unit = {
        solve(task.copy(problemName = "racp", instanceName = "j30_13_6_1.25", maybeOptimum = Some(401)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def radiation_i7_9(): Unit = {
        solve(task.copy(problemName = "radiation", instanceName = "i7-9", maybeOptimum = Some(1007)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def rcpsp_la34_x2(): Unit = {
        solve(task.copy(problemName = "rcpsp", instanceName = "data_la_x/la34_x2", maybeHighScore = Some(1811)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCumulativeConstraint]))
    def rcpsp_j30_27_5_wet_diverse_3(): Unit = {
        solve(task.copy(problemName = "rcpsp-wet-diverse", instanceName = "j30_27_5-wet-diverse-3", maybeOptimum = Some(6708)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def rcpsp_wet_j30_43_10(): Unit = {
        solve(task.copy(problemName = "rcpsp-wet", instanceName = "j30_43_10-wet", maybeOptimum = Some(121)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def rectangle_packing_18_true(): Unit = {
        solve(task.copy(problemName = "rectangle-packing", modelName = "rect_packing_mznc2014", instanceName = "data_mznc2014/rpp18_true"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def rel2onto_3_9(): Unit = {
        solve(task.copy(problemName = "rel2onto", instanceName = "3_9", maybeOptimum = Some(70802)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def road_cons_17(): Unit = {
        solve(task.copy(problemName = "road-cons", modelName = "road_naive", instanceName = "road_17", maybeMaximumNumberOfThreads = Some(1), maybeOptimum = Some(13560)))
    }

    // The original problem definition was buggy and hence trivial,
    // see https://github.com/MiniZinc/minizinc-benchmarks/pull/1.
    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAtLeastConstraint], classOf[HasAtMostConstraint], classOf[HasExactlyConstraint]))
    def roster_chicroster_dataset_large_18(): Unit = {
        solve(task.copy(problemName = "roster", modelName = "roster_model", instanceName = "chicroster_dataset_large_18", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def roster_shifts_bool_s50_e50_c3(): Unit = {
        solve(task.copy(problemName = "roster-shifts-bool", modelName = "bool-model", instanceName = "s50_e50_c3", maybeOptimum = Some(5312)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def roster_sickness_large_4(): Unit = {
        solve(task.copy(problemName = "roster-sickness", modelName = "bool-model-sickness", instanceName = "large-4", maybeOptimum = Some(217464)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasRegularConstraint]))
    def rotating_workforce_Example103(): Unit = {
        solve(task.copy(problemName = "rotating-workforce", instanceName = "Example103"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasRegularConstraint]))
    def rotating_workforce_Example789(): Unit = {
        solve(task.copy(problemName = "rotating-workforce", instanceName = "Example789"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasRegularConstraint], classOf[HasSlidingSumConstraint]))
    def rotating_workforce_scheduling_rws_instance_e_100_s_2(): Unit = {
        solve(task.copy(problemName = "rotating-workforce-scheduling", instanceName = "rws-instance-e-100-s-2"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def routing_flexible_GCM_0001(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "routing-flexible", instanceName = "routing_GCM_0001", maybeOptimum = Some(115729)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def rubik_4_cube(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "rubik", instanceName = "4-cube"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def sdn_chain_d24n600_3(): Unit = {
        solve(task.copy(problemName = "sdn-chain", modelName = "model", instanceName = "d24n600-3", maybeOptimum = Some(12)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasAlldifferentExceptConstraint]))
    def seat_moving_10_20_05(): Unit = {
        solve(task.copy(problemName = "seat-moving", instanceName = "sm-10-20-05", maybeOptimum = Some(90)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def ship_schedule_3Ships(): Unit = {
        solve(task.copy(problemName = "ship-schedule", modelName = "ship-schedule.cp", instanceName = "3Ships", maybeOptimum = Some(265650)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def skill_allocation_1m_1(): Unit = {
        solve(task.copy(problemName = "skill-allocation", modelName = "skill_allocation_only", instanceName = "skill_allocation_mzn_1m_1", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def smelt_2(): Unit = {
        solve(task.copy(problemName = "smelt", instanceName = "smelt_2", maybeOptimum = Some(69)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def soccer_computational_xIGData_28_22_7_4(): Unit = {
        solve(task.copy(problemName = "soccer-computational", modelName = "ecp", instanceName = "xIGData_28_22_7_4"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def solbat_12_12_5_1(): Unit = {
        solve(task.copy(problemName = "solbat", modelName = "sb", instanceName = "sb_12_12_5_1"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def speck_optimisation_easy_1(): Unit = {
        solve(task.copy(problemName = "speck-optimisation", modelName = "SPECK-Optimisation", instanceName = "easy_1", maybeOptimum = Some(1)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def spot5_5(): Unit = {
        solve(task.copy(problemName = "spot5", instanceName = "5", maybeOptimum = Some(261)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def stable_goods_s_d6(): Unit = {
        solve(task.copy(problemName = "stable-goods", modelName = "stable-goods-solution", instanceName = "s-d6", maybeOptimum = Some(6264)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def stack_cuttingstock_d5(): Unit = {
        solve(task.copy(problemName = "stack-cuttingstock", modelName = "stack-cutstock-cumu", instanceName = "d5", maybeOptimum = Some(19)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint]))
    def steelmillslab_bench_15_11(): Unit = {
        solve(task.copy(problemName = "steelmillslab", instanceName = "bench_15_11", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def steiner_systems_t3_k3_N11(): Unit = {
        solve(task.copy(problemName = "steiner-systems", instanceName = "steiner_t3_k3_N11"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def steiner_tree_es10fst03_stp(): Unit = {
        solve(task.copy(problemName = "steiner-tree", instanceName = "es10fst03.stp", maybeOptimum = Some(26003678)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def still_life_wastage_09(): Unit = {
        solve(task.copy(problemName = "still-life-wastage", modelName = "still-life", instanceName = "09", maybeOptimum = Some(43)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def stochastic_fjsp_a1_s5_fjsp_t8_j2_m3_a1_det(): Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "stochastic-fjsp", instanceName = "fjsp-a1-s5_fjsp-t8-j2-m3-a1_det", maybeOptimum = Some(328)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint]))
    def stochastic_vrp_s2_v2_c7(): Unit = {
        solve(task.copy(problemName = "stochastic-vrp", instanceName = "vrp-s2-v2-c7_vrp-v2-c7_det", maybeOptimum = Some(160)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasDiffnConstraint], classOf[HasDisjunctiveConstraint]))
    def stripboard_common_emitter_simple (): Unit = {
        solve(task.copy(problemName = "stripboard", instanceName = "common-emitter-simple", maybeOptimum = Some(40)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def sudoku_fixed_p48(): Unit = {
        solve(task.copy(problemName = "sudoku_fixed", instanceName = "sudoku_p48"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasDiffnConstraint], classOf[HasDisjunctiveConstraint]))
    def sudoku_opt_p20(): Unit = {
        solve(task.copy(problemName = "sudoku_opt", instanceName = "sudoku_p20", maybeOptimum = Some(-3)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def table_layout_p1000_m3_r100_c10(): Unit = {
        solve(task.copy(problemName = "table-layout", modelName = "TableLayout", instanceName = "p1000_m3_r100_c10", maybeOptimum = Some(8137)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def tc_graph_color_k10_34(): Unit = {
        solve(task.copy(problemName = "tc-graph-color", modelName = "tcgc2", instanceName = "k10_34", maybeOptimum = Some(433)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def tiny_cvrp_easy_instance_04(): Unit = {
        solve(task.copy(problemName = "tiny-cvrp", modelName = "TinyCVRP_ExactlyOneSolution", instanceName = "easy_instance_04", maybeOptimum = Some(454)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasInverseConstraint]))
    def tdtsp_10_34_00(): Unit = {
        solve(task.copy(problemName = "tdtsp", instanceName = "inst_10_34_00", maybeOptimum = Some(6662)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint], classOf[HasBinPackingConstraint]))
    def team_assignment_data2_6_12(): Unit = {
        solve(task.copy(problemName = "team-assignment", modelName = "model", instanceName = "data2_6_12", maybeOptimum = Some(17946)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDisjunctiveConstraint]))
    def test_scheduling_t30m10r10_5(): Unit = {
        solve(task.copy(problemName = "test-scheduling", instanceName = "t30m10r10-5", maybeOptimum = Some(2785)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def tower_challenge_070_070_15_070_04(): Unit = {
        solve(task.copy(problemName = "tower", instanceName = "tower_070_070_15_070-04", maybeOptimum = Some(61)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasSubcircuitConstraint]))
    def tpp_5_3_30_1(): Unit = {
        solve(task.copy(problemName = "tpp", instanceName = "tpp_5_3_30_1", maybeOptimum = Some(173)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def train_16(): Unit = {
        solve(task.copy(problemName = "train", instanceName = "instance.16", maybeOptimum = Some(93925)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def train_scheduling_trains15(): Unit = {
        solve(task.copy(problemName = "train-scheduling", modelName = "trains", instanceName = "trains15", maybeOptimum = Some(10)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasRegularConstraint]))
    def traveling_tppv_circ8bbal(): Unit = {
        solve(task.copy(problemName = "traveling-tppv", modelName = "ttppv", instanceName = "circ8bbal", maybeOptimum = Some(80)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def travelling_thief_n10_k15_c5000_l10000_u11000_r49(): Unit = {
        solve(task.copy(problemName = "travelling-thief", modelName = "ttp", instanceName = "data/n10_k15_c5000_l10000_u11000_r49", maybeOptimum = Some(5134100)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def triangular_n17(): Unit = {
        solve(task.copy(problemName = "triangular", instanceName = "n17", maybeHighScore = Some(40)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def unit_commitment_L7_T12G5L5(): Unit = {
        solve(task.copy(problemName = "unit-commitment", modelName = "Unit-Commitment", instanceName = "L7-T12G5L5", maybeOptimum = Some(130950)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasGlobalCardinalityConstraint]))
    def vaccine_v946(): Unit = {
        solve(task.copy(problemName = "vaccine", instanceName = "v946", maybeHighScore = Some(85)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def valve_network_horizon6(): Unit = {
        solve(task.copy(problemName = "valve-network", instanceName = "horizon6", maybeOptimum = Some(34)))
    }


    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint]))
    def vrp_submission_A_4v_22l(): Unit = {
        solve(task.copy(problemName = "vrp-submission", modelName = "cvrptw_w_reload", instanceName = "A_4v_22l", maybeHighScore = Some(481)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasCumulativeConstraint]))
    def vrplc_9_5_10_s1(): Unit = {
        solve(task.copy(problemName = "vrplc", modelName = "vrplc_service", instanceName = "vrplc9_5_10_s1", maybeOptimum = Some(351)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def whirlpool_d8(): Unit = {
        solve(task.copy(problemName = "whirlpool", modelName = "whirlpool-x", instanceName = "d8"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def wmsmc_int_batch_0_case_115_instance_0_small_subset_elements_3_sumreqs_1295_candidates_41(): Unit = {
        solve(task.copy(problemName = "wmsmc-int", modelName = "multisetcover", instanceName = "batch_0_case_115_instance_0_small_subset_elements_3_sumreqs_1295_candidates_41", maybeOptimum = Some(509704)))
    }

    // increasing and value_precede constraints might be redundant.
    @Test
    @Ignore("Does not compile since MiniZinc 2.7.3")
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCircuitConstraint], classOf[HasIncreasingConstraint], classOf[HasTableConstraint], classOf[HasValuePrecedeConstraint]))
    def yumi_dynamic_p_4_GG_GG_yumi_grid_setup_3_4_zones() : Unit = {
        solve(task.copy(problemName = "yumi-dynamic", instanceName = "p_4_GG_GG_yumi_grid_setup_3_4_zones", maybeOptimum = Some(462)))
    }

    // increasing and value_precede constraints might be redundant.
    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCircuitConstraint], classOf[HasIncreasingConstraint], classOf[HasTableConstraint], classOf[HasValuePrecedeConstraint]))
    def yumi_static_p_4_GG_GG_yumqi_grid_setup_3_3(): Unit = {
        solve(task.copy(problemName = "yumi-static", instanceName = "p_4_GG_GG_yumi_grid_setup_3_3", maybeOptimum = Some(626)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
    def word_equations_01_track_140_int(): Unit = {
        solve(task.copy(problemName = "word-equations", directoryLayout = NonStandardMiniZincBenchmarksLayout, modelName = "word_equations_01_track_140-int", maybeOptimum = Some(3)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def wordpress_7_Offers500(): Unit = {
        solve(task.copy(problemName = "wordpress", instanceName = "Wordpress7_Offers500", maybeOptimum = Some(2022)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def zephyrus_12_6_8_3(): Unit = {
        solve(task.copy(problemName = "zephyrus", instanceName = "12__6__8__3", maybeOptimum = Some(1300)))
    }

}
