package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.*
import org.junit.experimental.categories.Categories.*
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

import scala.language.implicitConversions

import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.VariableWithInfiniteDomainException
import yuck.flatzinc.test.util.*

/**
 * Test cases taken from the MiniZinc 1.6 distribution
 *
 * @author Michael Marte
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincExamples extends ZincBasedTest {

    override protected val logToConsole = false

    private val task =
        ZincTestTask(
            directoryLayout = MiniZincExamplesLayout,
            suitePath = "resources/mzn/tests/minizinc-examples",
            maybeRuntimeLimitInSeconds = Some(10),
            throwWhenUnsolved = true,
            reusePreviousTestResult = false)

    private implicit def createTask(problemName: String): ZincTestTask = task.copy(problemName = problemName)

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def test2DPacking(): Unit = {
        solve(task.copy(problemName = "2DPacking", maybeOptimum = Some(1)))
    }

    // Tough puzzle with tight all_different constraint, seemingly has only one solution!
    // (http://www.mathematik.uni-bielefeld.de/~sillke/PUZZLES/ALPHAMETIC/alphacipher)
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlpha(): Unit = {
        solve("alpha")
    }

    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem]))
    def testBattleships(): Unit = {
        solve("battleships_1")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testBlocksworld1(): Unit = {
        solve(task.copy(problemName = "blocksworld_instance_1"))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testBlocksworld2(): Unit = {
        solve(task.copy(problemName = "blocksworld_instance_2"))
    }

    // Search variables item[] have infinite domains but pruning saves the day.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testCutstock(): Unit = {
        solve(task.copy(problemName = "cutstock", maybeOptimum = Some(4)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testEq20(): Unit = {
        solve("eq20")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testFactoryPlanning(): Unit = {
        solve("factory_planning_instance")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def testGolomb(): Unit = {
        solve(task.copy(problemName = "golomb", maybeOptimum = Some(6)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testJobshop(): Unit = {
        solve(task.copy(problemName = "jobshop2x2", maybeOptimum = Some(11)))
    }

    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testKnights(): Unit = {
        solve(task.copy(problemName = "knights"))
    }

    // Uses redundant constraints in the form of a dual model that implies the need for
    // a lot of channeling constraints.
    // Hence this model is bad for local search and we test langford2 instead.
    @Test
    @Category(Array(classOf[UnsuitableProblem], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testLangford(): Unit = {
        solve("langford")
    }

    // Runtime depends strongly on algorithmic details like choice of seed and random generator.
    // Removing the symmetry breaker makes the problem even harder to solve.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testLangford2(): Unit = {
        solve("langford2")
    }

    // Runtime depends strongly on algorithmic details like choice of seed and random generator.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testLatinSquares(): Unit = {
        solve("latin_squares_fd")
    }

    // Magic sequence instances have tight all_different constraints.
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    @Test
    def testMagicSequence3(): Unit = {
        solve("magicsq_3")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testMagicSequence4(): Unit = {
        solve("magicsq_4")
    }

    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    @Test
    def testMagicSequence5(): Unit = {
        solve("magicsq_5")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MaximizationProblem]))
    def testMultiDimKnapsack(): Unit = {
        solve(task.copy(problemName = "multidimknapsack_simple", maybeOptimum = Some(17)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testOss(): Unit = {
        solve(task.copy(problemName = "oss", maybeOptimum = Some(1168), maybeTargetObjectiveValue = Some(1250)))
    }

    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem]))
    def testPacking(): Unit = {
        solve(task.copy(problemName = "packing"))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testPartition(): Unit = {
        solve("partition")
    }

    // The original formulation contains symmetry breaking and the optimization part is based on that.
    // The improved model uses alldifferent_except_0 and count :-)
    // This formulation maximizes the number of squares in the sum.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def testPerfectSquares(): Unit = {
        solve(task.copy(problemName = "perfsq_ls", maybeOptimum = Some(5)))
    }

    // Another formulation for the perfect square problem that maximizes the sum of squares.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MaximizationProblem]))
    def testPerfectSquares2(): Unit = {
        solve(task.copy(problemName = "perfsq2", maybeOptimum = Some(337561)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def testPhoto(): Unit = {
        solve(task.copy(problemName = "photo", maybeOptimum = Some(8)))
    }

    // Big domains!
    // Solver finds good solutions very fast but cannot find optimum.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testProductFd(): Unit = {
        solve(task.copy(problemName = "product_fd", maybeOptimum = Some(37200), maybeTargetObjectiveValue = Some(37400)))
    }

    // Variables inside and outside do not have infinite domains and pruning cannot make them finite.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testProductLp(): Unit = {
        assertEx(
            solve(task.copy(problemName = "product_lp", maybeOptimum = Some(37200))),
            classOf[VariableWithInfiniteDomainException])
    }

    // Solved, but not to optimality.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testRadiation(): Unit = {
        solve(task.copy(problemName = "radiation", maybeOptimum = Some(553), maybeTargetObjectiveValue = Some(2000)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testQuasigroup(): Unit = {
        solve("quasigroup_qg5")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testQueensCp(): Unit = {
        solve("queen_cp2")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testQueensIp(): Unit = {
        solve("queen_ip")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testSimpleSat(): Unit = {
        solve("simple_sat")
    }

    // Uses set decision variables and symmetry breaking on set variables.
    // Symmetry breaking translates to set_le constraints.
    // set_le is defined in terms of subset relation and symmetrical difference:
    // a \subset b or min(a \symdiff b) \in a
    // Gecode implements set_le via SRT_LQ (see gecode/set/dom.cpp and gecode/set/rel/rel-lq.hpp)
    // in an unclear way.
    // Do not implement set_le and set_lt as they have no obvious business value!
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testSteinerTriples(): Unit = {
        solve(task.copy(problemName = "steiner-triples"))
    }

    // Runtime depends strongly on algorithmic details like choice of seed and random generator.
    // Has a lot of constants in all_different constraints that are used to prune domains up-front!
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testSudoku(): Unit = {
        solve("sudoku")
    }

    // A lot of (allegedly) redundant constraints in the original definition.
    // Annotating them leads to a solution better than the optimum proven by CP (439 vs 442).
    // So it seems that the redundant constraints are not that redundant ...
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testTemplateDesign(): Unit = {
        solve(task.copy(problemName = "template_design", maybeOptimum = Some(442)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki1(): Unit = {
        solve("tenpenki_1")
    }

    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki2(): Unit = {
        solve("tenpenki_2")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki3(): Unit = {
        solve("tenpenki_3")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki4(): Unit = {
        solve("tenpenki_4")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki5(): Unit = {
        solve("tenpenki_5")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki6(): Unit = {
        solve("tenpenki_6")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testTimetabling(): Unit = {
        solve("timetabling")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testTrucking(): Unit = {
        solve(task.copy(problemName = "trucking", maybeOptimum = Some(224)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testWarehouses(): Unit = {
        solve(task.copy(problemName = "warehouses", maybeOptimum = Some(383)))
    }

    // Runtime depends strongly on algorithmic details like choice of seed and random generator.
    // Has six variables declared as parameters by bool_eq constraints.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testWolfGoatCabbage(): Unit = {
        solve("wolf_goat_cabbage")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testZebra(): Unit = {
        solve("zebra")
    }

}

/**
 * Test cases for running after every change
 *
 * @author Michael Marte
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[EasyInstance]))
@SuiteClasses(Array(classOf[MiniZincExamples]))
class EasyMiniZincExamples

/**
 * Test cases for running after every change
 *
 * @author Michael Marte
 */
@Ignore("Avoid pointless NoTestsRemainException")
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[MediumInstance]))
@SuiteClasses(Array(classOf[MiniZincExamples]))
final class MediumMiniZincExamples

/**
 * Hard and unsolved problems
 *
 * @author Michael Marte
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HardInstance]))
@SuiteClasses(Array(classOf[MiniZincExamples]))
final class HardMiniZincExamples

/**
 * @author Michael Marte
 *
 * Test cases for running after every change
 */
@RunWith(classOf[runners.Suite])
@SuiteClasses(
    Array(
        classOf[EasyMiniZincExamples],
        classOf[MediumMiniZincExamples]))
class TractableMiniZincExamples

/**
 * @author Michael Marte
 *
 * Tractable satisfiability examples
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[SatisfiabilityProblem]))
@SuiteClasses(Array(classOf[TractableMiniZincExamples]))
class TractableSatisfiabilityExamples

/**
 * Tractable minimization examples
 *
 * @author Michael Marte
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[MinimizationProblem]))
@SuiteClasses(Array(classOf[TractableMiniZincExamples]))
class TractableMinimizationExamples

/**
 * Tractable maximization examples
 *
 * @author Michael Marte
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[MaximizationProblem]))
@SuiteClasses(Array(classOf[TractableMiniZincExamples]))
class TractableMaximizationExamples

/**
 * Tractable examples with alldifferent_int constraints
 *
 * @author Michael Marte
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasAlldifferentConstraint]))
@SuiteClasses(Array(classOf[TractableMiniZincExamples]))
class TractableAlldistinctExamples
