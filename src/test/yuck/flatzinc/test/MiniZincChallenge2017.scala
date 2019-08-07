package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * The smallest problems from the MiniZinc challenge 2017
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenge2017 extends MiniZincBasedTest {

    private val task = MiniZincTestTask(directoryLayout = StandardMiniZincBenchmarksLayout, suitePath = "resources/mzn/benchmarks", suiteName = "mznc17")

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def cargo_01_0s_1913: Unit = {
        solve(task.copy(problemName = "cargo", modelName = "cargo_coarsePiles", instanceName = "challenge01_0s_1913", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def city_position_5_06: Unit = {
        solve(task.copy(problemName = "city-position", instanceName = "city-5-06", maybeOptimum = Some(3493)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasValuePrecedeConstraint]))
    def community_detection_Sampson_s10_k3: Unit = {
        solve(task.copy(problemName = "community-detection", instanceName = "Sampson.s10.k3", maybeOptimum = Some(10)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def crosswords_grid_05_02_dict_55: Unit = {
        solve(task.copy(problemName = "crosswords", modelName = "crossword_opt", instanceName = "grid-05.02_dict-55", maybeOptimum = Some(62)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasGlobalCardinalityConstraint]))
    def gbac_UD2: Unit = {
        solve(task.copy(problemName = "gbac", instanceName = "UD2-gbac", maybeOptimum = Some(146)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasCountConstraint], classOf[HasTableConstraint]))
    def groupsplitter_u5g1pref0: Unit = {
        solve(task.copy(problemName = "groupsplitter", modelName = "group", instanceName = "u5g1pref0", maybeOptimum = Some(90)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def hrc_exp1_1_5670: Unit = {
        solve(task.copy(problemName = "hrc", instanceName = "exp1-1-5670", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def jp_encoding_data200: Unit = {
        solve(task.copy(problemName = "jp-encoding", instanceName = "data200", maybeOptimum = Some(7544)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def ma_path_finding_ins_g16_p10_a10: Unit = {
        solve(task.copy(problemName = "ma-path-finding", modelName = "mapf", instanceName = "ins_g16_p10_a10", maybeOptimum = Some(112), maybeMaximumNumberOfThreads = Some(1)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasSubcircuitConstraint]))
    def mario_medium_1: Unit = {
        solve(task.copy(problemName = "mario", instanceName = "mario_medium_1", maybeOptimum = Some(1481)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def opd_flener_et_al_15_350_100: Unit = {
        solve(task.copy(problemName = "opd", instanceName = "flener_et_al_15_350_100", maybeOptimum = Some(24)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def opt_cryptanalysis_r5: Unit = {
        solve(task.copy(problemName = "opt-cryptanalysis", modelName = "mznc2017_aes_opt", instanceName = "r5", maybeOptimum = Some(20)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def rcpsp_wet_j30_1_3: Unit = {
        solve(task.copy(problemName = "rcpsp-wet", instanceName = "j30_1_3-wet", maybeOptimum = Some(93)))
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

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def routing_flexible_GCM_0001: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "routing-flexible", instanceName = "routing_GCM_0001", maybeOptimum = Some(115729)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint]))
    def steelmillslab_bench_14_1: Unit = {
        solve(task.copy(problemName = "steelmillslab", instanceName = "bench_14_1", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def tc_graph_color_k10_31: Unit = {
        solve(task.copy(problemName = "tc-graph-color", modelName = "tcgc2", instanceName = "k10_31", maybeOptimum = Some(427)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasInverseConstraint]))
    def tdtsp_10_42_00: Unit = {
        solve(task.copy(problemName = "tdtsp", instanceName = "inst_10_42_00", maybeOptimum = Some(8421)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasRegularConstraint]))
    def traveling_tppv_circ8dbal: Unit = {
        solve(task.copy(problemName = "traveling-tppv", modelName = "ttppv", instanceName = "circ8dbal", maybeOptimum = Some(80)))
    }

}
