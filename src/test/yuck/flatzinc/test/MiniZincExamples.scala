package yuck.flatzinc.test

import scala.language.implicitConversions

import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}
import org.junit.jupiter.api.{Disabled, MethodOrderer, Tag, Test, TestMethodOrder}
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource
import org.junit.platform.suite.api.{ExcludeTags, IncludeTags, SelectClasses, Suite}

import yuck.SolvingMethod
import yuck.flatzinc.compiler.VariableWithInfiniteDomainException
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.HasGlobalConstraint.*
import yuck.flatzinc.test.util.InstanceDifficulty.*
import yuck.flatzinc.test.util.ProblemType.*
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*

/**
 * Test cases taken from the MiniZinc 1.6 distribution
 */
@TestMethodOrder(classOf[MethodOrderer.MethodName])
@ParameterizedClass
@MethodSource(Array("parameters"))
@Execution(ExecutionMode.CONCURRENT)
final class MiniZincExamples(maybePreferredSolvingMethod: Option[SolvingMethod]) extends ZincBasedTest {

    private val task =
        ZincTestTask(
            directoryLayout = MiniZincExamplesLayout,
            suitePath = "resources/mzn/tests/minizinc-examples",
            solverConfiguration =
                ZincTestTask().solverConfiguration.copy(
                    name = maybePreferredSolvingMethod.map(_.toString.toLowerCase).getOrElse("hybrid"),
                    maybePreferredSolvingMethod = maybePreferredSolvingMethod,
                    maybeRuntimeLimitInSeconds = Some(5)),
            throwWhenUnsolved = true)

    private implicit def createTask(problemName: String): ZincTestTask = task.copy(problemName = problemName)

    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def test2DPacking(): Unit = {
        solve(task.copy(problemName = "2DPacking", maybeOptimum = Some(1)))
    }

    // Tough puzzle with tight all_different constraint, seemingly has only one solution!
    // (http://www.mathematik.uni-bielefeld.de/~sillke/PUZZLES/ALPHAMETIC/alphacipher)
    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testAlpha(): Unit = {
        val runtimeLimitInSeconds = maybePreferredSolvingMethod match {
            case Some(SolvingMethod.FeasibilityJump) => 15
            case _ => 5
        }
        solve(task.copy(problemName = "alpha", maybeRuntimeLimitInSeconds = Some(runtimeLimitInSeconds)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HardInstance)
    def testBattleships(): Unit = {
        solve("battleships_1")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testBlocksworld1(): Unit = {
        solve("blocksworld_instance_1")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testBlocksworld2(): Unit = {
        solve("blocksworld_instance_2")
    }

    // Search variables item[] have infinite domains but pruning saves the day.
    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testCutstock(): Unit = {
        solve(task.copy(problemName = "cutstock", maybeOptimum = Some(4)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testEq20(): Unit = {
        solve("eq20")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testFactoryPlanning(): Unit = {
        solve("factory_planning_instance")
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testGolomb(): Unit = {
        solve(task.copy(problemName = "golomb", maybeOptimum = Some(6)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testJobshop(): Unit = {
        solve(task.copy(problemName = "jobshop2x2", maybeOptimum = Some(11)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HardInstance)
    @Tag(HasAllDifferentConstraint)
    def testKnights(): Unit = {
        solve("knights")
    }

    // Uses redundant constraints in the form of a dual model that implies the need for
    // a lot of channeling constraints.
    // Hence this model is bad for local search and we test langford2 instead.
    @Test
    @Disabled
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentConstraint)
    def testLangford(): Unit = {
        solve("langford")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testLangford2(): Unit = {
        solve("langford2")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testLatinSquares(): Unit = {
        solve("latin_squares_fd")
    }

    // Magic sequence instances have tight all_different constraints.
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Test
    def testMagicSequence3(): Unit = {
        solve("magicsq_3")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testMagicSequence4(): Unit = {
        solve("magicsq_4")
    }

    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Test
    def testMagicSequence5(): Unit = {
        val runtimeLimitInSeconds = maybePreferredSolvingMethod match {
            case Some(SolvingMethod.FeasibilityJump) => 10
            case _ => 5
        }
        solve(task.copy(problemName = "magicsq_5", maybeRuntimeLimitInSeconds = Some(runtimeLimitInSeconds)))
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(EasyInstance)
    def testMultiDimKnapsack(): Unit = {
        solve(task.copy(problemName = "multidimknapsack_simple", maybeOptimum = Some(17)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testOss(): Unit = {
        solve(task.copy(problemName = "oss", maybeOptimum = Some(1168), maybeTargetObjectiveValue = Some(1250)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HardInstance)
    def testPacking(): Unit = {
        solve("packing")
    }

    // This one is hard for FJ because FJ does not support implicit constraints.
    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HardInstance)
    @Tag(HasAllDifferentConstraint)
    def testPartition(): Unit = {
        solve("partition")
    }

    // The original formulation contains symmetry breaking and the optimization part is based on that.
    // The improved model uses all_different_except_0 and count :-)
    // This formulation maximizes the number of squares in the sum.
    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentExceptConstraint)
    def testPerfectSquares(): Unit = {
        solve(task.copy(problemName = "perfsq_ls", maybeOptimum = Some(5)))
    }

    // Another formulation for the perfect square problem that maximizes the sum of squares.
    // FJ finds good solutions very fast but cannot find the optimum.
    @Test
    @Tag(MaximizationProblem)
    @Tag(EasyInstance)
    def testPerfectSquares2(): Unit = {
        solve(task.copy(problemName = "perfsq2", maybeOptimum = Some(337561), maybeTargetObjectiveValue = Some(300000)))
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testPhoto(): Unit = {
        solve(task.copy(problemName = "photo", maybeOptimum = Some(8)))
    }

    // Big domains!
    // The solver finds good solutions fast but takes too long to find the optimum.
    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testProductFd(): Unit = {
        solve(task.copy(problemName = "product_fd", maybeOptimum = Some(37200), maybeTargetObjectiveValue = Some(37400)))
    }

    // Variables inside and outside do not have infinite domains and pruning cannot make them finite.
    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testProductLp(): Unit = {
        assertThrows(
            solve(task.copy(problemName = "product_lp", maybeOptimum = Some(37200))),
            classOf[VariableWithInfiniteDomainException])
    }

    // Solved, but not to optimality.
    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testRadiation(): Unit = {
        solve(task.copy(problemName = "radiation", maybeOptimum = Some(553), maybeTargetObjectiveValue = Some(2000)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testQuasigroup(): Unit = {
        solve("quasigroup_qg5")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testQueensCp(): Unit = {
        solve("queen_cp2")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testQueensIp(): Unit = {
        solve("queen_ip")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testSimpleSat(): Unit = {
        solve("simple_sat")
    }

    // Uses set decision variables.
    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testSteinerTriples(): Unit = {
        solve(task.copy(problemName = "steiner-triples"))
    }

    // Has a lot of constants in all_different constraints that are used to prune domains up-front.
    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testSudoku(): Unit = {
        solve("sudoku")
    }

    // A lot of (allegedly) redundant constraints in the original definition.
    // Annotating them leads to a solution better than the optimum proven by CP (439 vs 442).
    // So it seems that the redundant constraints are not that redundant ...
    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testTemplateDesign(): Unit = {
        solve(task.copy(problemName = "template_design", maybeOptimum = Some(442)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testTenpenki1(): Unit = {
        solve("tenpenki_1")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HardInstance)
    def testTenpenki2(): Unit = {
        solve("tenpenki_2")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testTenpenki3(): Unit = {
        solve("tenpenki_3")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testTenpenki4(): Unit = {
        val result = solveWithResult(task.copy(problemName = "tenpenki_4"))
        assertEq(result.space.searchVariables, Set())
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testTenpenki5(): Unit = {
        solve("tenpenki_5")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testTenpenki6(): Unit = {
        solve("tenpenki_6")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testTimetabling(): Unit = {
        solve("timetabling")
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testTrucking(): Unit = {
        solve(task.copy(problemName = "trucking", maybeOptimum = Some(224)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(EasyInstance)
    def testWarehouses(): Unit = {
        solve(task.copy(problemName = "warehouses", maybeOptimum = Some(383)))
    }

    // Has six variables declared as parameters by bool_eq constraints.
    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    def testWolfGoatCabbage(): Unit = {
        solve("wolf_goat_cabbage")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(EasyInstance)
    @Tag(HasAllDifferentConstraint)
    def testZebra(): Unit = {
        solve("zebra")
    }

}

object MiniZincExamples {

    def parameters = Array(None, Some(SolvingMethod.SimulatedAnnealing), Some(SolvingMethod.FeasibilityJump))

}

/**
 * Test cases for running after every change
 */
@Suite
@SelectClasses(Array(classOf[MiniZincExamples]))
@IncludeTags(Array(EasyInstance))
class EasyMiniZincExamples

/**
 * Test cases for running after every change
 */
@Suite
@SelectClasses(Array(classOf[MiniZincExamples]))
@IncludeTags(Array(MediumInstance))
final class MediumMiniZincExamples

/**
 * Hard and unsolved problems
 */
@Suite
@SelectClasses(Array(classOf[MiniZincExamples]))
@IncludeTags(Array(HardInstance))
final class HardMiniZincExamples

/**
 * Test cases for running after every change
 */
@Suite
@SelectClasses(Array(classOf[MiniZincExamples]))
@ExcludeTags(Array(HardInstance))
class TractableMiniZincExamples

/**
 * Tractable satisfiability examples
 */
@Suite
@SelectClasses(Array(classOf[MiniZincExamples]))
@IncludeTags(Array(SatisfiabilityProblem))
@ExcludeTags(Array(HardInstance))
class TractableSatisfiabilityExamples

/**
 * Tractable minimization examples
 */
@Suite
@SelectClasses(Array(classOf[MiniZincExamples]))
@IncludeTags(Array(MinimizationProblem))
@ExcludeTags(Array(HardInstance))
class TractableMinimizationExamples

/**
 * Tractable maximization examples
 */
@Suite
@SelectClasses(Array(classOf[MiniZincExamples]))
@IncludeTags(Array(MaximizationProblem))
@ExcludeTags(Array(HardInstance))
class TractableMaximizationExamples

/**
 * Tractable examples with integer all_different constraints
 */
@Suite
@SelectClasses(Array(classOf[MiniZincExamples]))
@IncludeTags(Array(HasAllDifferentConstraint))
@ExcludeTags(Array(HardInstance))
class TractableAllDifferentExamples
