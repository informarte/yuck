package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * The smallest problems from the MiniZinc challenge 2013
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class MiniZincChallenge2013 extends MiniZincTestSuite {

    val task = MiniZincTestTask(directoryLayout = StandardMiniZincBenchmarksLayout, suitePath = "resources/mzn/benchmarks", suiteName = "mznc13")

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint], classOf[HasTableConstraint]))
    def black_hole_4 {
        solve(task.copy(problemName = "black-hole", instanceName = "4"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def cargo_04_1s_626 {
        solve(task.copy(problemName = "cargo", modelName = "cargo_coarsePiles", instanceName = "challenge04_1s_626", maybeOptimum = Some(714)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def celar_6_sub2 {
        solve(task.copy(problemName = "celar", instanceName = "CELAR6-SUB2"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint]))
    def filters_dfq_1_1 {
        solve(task.copy(problemName = "filters", modelName = "filter", instanceName = "dfq_1_1", maybeOptimum = Some(4)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def fjsp_easy01 {
        solve(task.copy(problemName = "flexible-jobshop", modelName = "fjsp", instanceName = "easy01", maybeOptimum = Some(253)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def ghoulomb_4_9_10 {
        solve(task.copy(problemName = "ghoulomb", instanceName = "4-9-10", maybeOptimum = Some(33)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasElementConstraint]))
    def java_routing_trip_6_3 {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "java-routing", instanceName = "trip_6_3", maybeOptimum = Some(67)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCountConstraint]))
    def linear_to_program_l2p1 {
        solve(task.copy(problemName = "linear-to-program", instanceName = "l2p1", maybeOptimum = Some(6)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def league_15_4_3 {
        solve(task.copy(problemName = "league", modelName = "league_mznc2013", instanceName = "model15-4-3", maybeOptimum = Some(290)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasSubcircuitConstraint]))
    def mario_easy_2 {
        solve(task.copy(problemName = "mario", instanceName = "mario_easy_2", maybeOptimum = Some(628)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def nmseq_99 {
        solve(task.copy(problemName = "nmseq", instanceName = "99"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def nonogram_dom_06 {
        solve(task.copy(problemName = "nonogram", modelName = "non", instanceName = "dom_06"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def on_call_rostering_10s_150d {
        solve(task.copy(problemName = "on-call-rostering", modelName = "oc-roster", instanceName = "10s-150d", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def pattern_set_mining_anneal_k1 {
        solve(task.copy(problemName = "pattern-set-mining", modelName = "pattern_set_mining_k1", instanceName = "anneal", maybeOptimum = Some(494)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def pentominoes_int_02 {
        solve(task.copy(problemName = "pentominoes", modelName = "pentominoes-int", instanceName = "02"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def proteindesign12_2TRX_11p_8aa_usingEref_self {
        solve(task.copy(problemName = "proteindesign12", modelName = "wcsp", instanceName = "2TRX.11p.8aa.usingEref_self"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def radiation_i6_7 {
        solve(task.copy(problemName = "radiation", instanceName = "i6-7", maybeOptimum = Some(635)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def rcpsp_12 {
        solve(task.copy(problemName = "rcpsp", instanceName = "12", maybeOptimum = Some(38)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def rubik_4_cube {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "rubik", instanceName = "4-cube"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def vrp_P_n16_k8 {
        solve(task.copy(problemName = "vrp", instanceName = "P-n16-k8.vrp", maybeOptimum = Some(450)))
    }

}
