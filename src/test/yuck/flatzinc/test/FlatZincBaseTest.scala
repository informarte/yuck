package yuck.flatzinc.test

import scala.language.implicitConversions
import scala.reflect.ClassTag

import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}
import org.junit.jupiter.api.{Tag, Test}

import yuck.constraints.*
import yuck.core.*
import yuck.flatzinc.compiler.{Bool2Int1, LevelWeightMaintainer, VariableWithInfiniteDomainException}
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.HasGlobalConstraint.*
import yuck.flatzinc.test.util.ProblemType.*
import yuck.flatzinc.test.util.SourceFormat.*
import yuck.flatzinc.test.util.VerificationFrequency.*

/**
 * Tests that cover edge cases and rarely used features of the FlatZinc language
 */
@Execution(ExecutionMode.CONCURRENT)
final class FlatZincBaseTest extends FrontEndTest {

    @Test
    @Tag(SatisfiabilityProblem)
    def testParametersInArrays(): Unit = {
        solve(task.copy(sourceFormat = FlatZinc, problemName = "parameters_in_arrays_test"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testArrayBoolAndWithDuplicateVariable(): Unit = {
        val result = solveWithResult(task.copy(sourceFormat = FlatZinc, problemName = "array_bool_and_with_duplicate_variable_test", solverConfiguration = task.solverConfiguration.copy(runPresolver = false)))
        assertEq(result.space.searchVariables.map(_.name), Set("x", "y"))
        assert(result.space.problemParameters.isEmpty)
        assertEq(result.space.channelVariables.size, 2)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 3)
        assertEq(result.space.numberOfConstraints[And], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testArrayBoolOrWithDuplicateVariable(): Unit = {
        val result = solveWithResult(task.copy(sourceFormat = FlatZinc, problemName = "array_bool_or_with_duplicate_variable_test", solverConfiguration = task.solverConfiguration.copy(runPresolver = false)))
        assertEq(result.space.searchVariables.map(_.name), Set("x", "y"))
        assert(result.space.problemParameters.isEmpty)
        assertEq(result.space.channelVariables.size, 2)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 3)
        assertEq(result.space.numberOfConstraints[Or], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testVarArrayAccessWithoutOptimization(): Unit = {
        val result = solveWithResult(
            task.copy(
                problemName = "var_array_access_test",
                solverConfiguration = task.solverConfiguration.copy(
                    name = "without-optimization",
                    optimizeArrayAccess = false)))
        assertEq(result.space.numberOfConstraints[ElementVar[?, ?, ?]], 10)
        assertEq(result.space.searchVariables.size, 40)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testVarArrayAccessWithOptimization(): Unit = {
        val result = solveWithResult(
            task.copy(
                problemName = "var_array_access_test",
                solverConfiguration = task.solverConfiguration.copy(name = "with-optimization")))
        assertEq(result.space.numberOfConstraints[ElementsVar[?, ?, ?]], 1)
        assertEq(result.space.searchVariables.size, 25)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testConstArrayAccess(): Unit = {
        val result = solveWithResult(task.copy(problemName = "const_array_access_test"))
        assertEq(result.space.numberOfConstraints[ElementConst[?, ?, ?]], 3)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testArrayAccessWhereResultMustEqualIndex(): Unit = {
        val result = solveWithResult(task.copy(problemName = "array_access_where_result_must_equal_index_test"))
        assertEq(result.space.numberOfConstraints[ElementVar[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testArrayAccessWhereIndexIsChannelVariable(): Unit = {
        solve(task.copy(problemName = "array_access_with_index_channel_test"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testArrayAccessWithConstrainedIndexVariables(): Unit = {
        val result = solveWithResult(task.copy(problemName = "array_access_with_constrained_index_variables_test"))
        assertEq(result.space.searchVariables.size, 12)
        assert(! result.space.searchVariables.exists(_.name == "x[5]"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseBool(): Unit = {
        testIfThenElse("if_then_else_bool_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseBoolWithConstCondition(): Unit = {
        testIfThenElseWithConstCondition("if_then_else_bool_with_const_condition_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseBoolWithEqualAlternatives(): Unit = {
        testIfThenElseWithEqualAlternatives("if_then_else_bool_with_equal_alternatives_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseVarBool(): Unit = {
        testIfThenElseVar("if_then_else_var_bool_test")(using BooleanTypeTraits)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseVarBoolWithConstCondition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_var_bool_with_const_condition_test"))
        assertEq(result.space.searchVariables.map(_.name), Set("u"))
        assertEq(result.space.channelVariables.size, 2)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 1)
        assertEq(result.space.numberOfConstraints, 2)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Not], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseInt(): Unit = {
        testIfThenElse("if_then_else_int_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseIntToBool2Int(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_int_to_bool2int_test"))
        assertEq(result.space.searchVariables.map(_.name), Set("c", "y"))
        assertEq(result.space.channelVariables.size, 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 4)
        assertEq(result.space.numberOfConstraints[Bool2Int1], 1)
        assertEq(result.space.numberOfConstraints[LinearConstraint[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseIntWithConstCondition(): Unit = {
        testIfThenElseWithConstCondition("if_then_else_int_with_const_condition_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseIntWithEqualAlternatives(): Unit = {
        testIfThenElseWithEqualAlternatives("if_then_else_int_with_equal_alternatives_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseVarInt(): Unit = {
        testIfThenElseVar("if_then_else_var_int_test")(using IntegerTypeTraits)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseVarIntWithConstCondition(): Unit = {
        testIfThenElseVarWithConstCondition("if_then_else_var_int_with_const_condition_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseSet(): Unit = {
        testIfThenElse("if_then_else_set_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseSetWithConstCondition(): Unit = {
        testIfThenElseWithConstCondition("if_then_else_set_with_const_condition_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseSetWithEqualAlternatives(): Unit = {
        testIfThenElseWithEqualAlternatives("if_then_else_set_with_equal_alternatives_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseVarSet(): Unit = {
        testIfThenElseVar("if_then_else_var_set_test")(using IntegerSetTypeTraits)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testIfThenElseVarSetWithConstCondition(): Unit = {
        testIfThenElseVarWithConstCondition("if_then_else_var_set_with_const_condition_test")
    }

    private def testIfThenElse(problemName: String): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.searchVariables.map(_.name), Set("c"))
        assertEq(result.space.channelVariables.size, 5)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("x", "y"))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[IfThenElse[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Ne[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Not], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    private def testIfThenElseWithConstCondition(problemName: String): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.searchVariables, Set())
        assertEq(result.space.channelVariables.size, 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 1)
        assertEq(result.space.numberOfConstraints, 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
    }

    private def testIfThenElseWithEqualAlternatives(problemName: String): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.searchVariables, Set())
        assertEq(result.space.channelVariables.size, 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 1)
        assertEq(result.space.numberOfConstraints, 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
    }

    private def testIfThenElseVar
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
        (problemName: String)
        (using typeTraits: TypeTraits[A, D, X]):
        Unit =
    {
        val booleanCase = typeTraits == BooleanTypeTraits
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.searchVariables.map(_.name), Set("c", "u", "v"))
        assertEq(result.space.channelVariables.size, if booleanCase then 5 else 6)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("x", "y"))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), if booleanCase then 2 else 3)
        assertEq(result.space.numberOfConstraints, if booleanCase then 6 else 7)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[IfThenElse[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Ne[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Not], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        typeTraits match {
            case BooleanTypeTraits =>
            case IntegerTypeTraits =>
                assertEq(result.space.numberOfConstraints[Contains], 1)
            case IntegerSetTypeTraits =>
                assertEq(result.space.numberOfConstraints[Subset], 1)
        }
    }

    private def testIfThenElseVarWithConstCondition(problemName: String): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.searchVariables.map(_.name), Set("u", "v"))
        assertEq(result.space.channelVariables.size, 2)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 3)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Ne[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(MinimizationProblem)
    def testSetIntersection(): Unit = {
        testSetOperation[SetIntersection]("set_intersect_test")
    }

    @Test
    @Tag(MinimizationProblem)
    def testSetUnion(): Unit = {
        testSetOperation[SetUnion]("set_union_test")
    }

    @Test
    @Tag(MinimizationProblem)
    def testSetDiff(): Unit = {
        testSetOperation[SetDifference]("set_diff_test")
    }

    @Test
    @Tag(MinimizationProblem)
    def testSetSymdiff(): Unit = {
        testSetOperation[SymmetricalSetDifference]("set_symdiff_test")
    }

    private def testSetOperation[T <: Constraint](problemName: String)(using classTag: ClassTag[T]): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName, verificationFrequency = VerifyEverySolution))
        assertEq(result.space.searchVariables.filterNot(wasIntroducedByYuck).map(_.name), Set("u", "v", "w"))
        assertEq(result.space.channelVariables.size, 8)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("uv", "vw"))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 3)
        assertEq(result.space.numberOfConstraints, 10)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[LevelWeightMaintainer], 1)
        assertEq(result.space.numberOfConstraints[Plus[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        assertEq(result.space.numberOfConstraints[SetCardinality], 2)
        assertEq(
            result.space.numberOfConstraints(
                constraint => constraint.isInstanceOf[Subset] && constraint.inVariables.count(_.name == "vw") == 1),
            1)
        assertEq(result.space.numberOfConstraints(classTag.runtimeClass.isInstance), 2)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasTableConstraint)
    def testInconsistentProblem(): Unit = {
        assertThrows(
            solve(task.copy(problemName = "empty_table_int_test")),
            classOf[InconsistentProblemException])
    }

    @Test
    @Tag(MinimizationProblem)
    def testMinimizationOfSumWithNegativeAddends(): Unit = {
        val result = solveWithResult(task.copy(problemName = "minimization_of_sum_with_negative_addends_test"))
        assertEq(result.quality, Zero)
    }

    @Test
    @Tag(MinimizationProblem)
    def testMinimizationProblemWithBoundedDanglingObjectiveVariable(): Unit = {
        val result = solveWithResult(task.copy(problemName = "minimization_with_bounded_dangling_objective_variable_test"))
        val x = result.objective.objectiveVariables(1).asInstanceOf[IntegerVariable]
        assert(result.space.isDanglingVariable(x))
        assertEq(result.assignment.value(x), IntegerValue(-1000))
    }

    @Test
    @Tag(MinimizationProblem)
    def testMinimizationProblemWithUnboundedDanglingObjectiveVariable(): Unit = {
        // We cannot verify the solution because Gecode does not support 64 bit integers.
        val result = solveWithResult(task.copy(problemName = "minimization_with_unbounded_dangling_objective_variable_test", verificationFrequency = NoVerification))
        val x = result.objective.objectiveVariables(1).asInstanceOf[IntegerVariable]
        assert(result.space.isDanglingVariable(x))
        assertEq(result.assignment.value(x), IntegerTypeTraits.minValue)
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasAllDifferentConstraint)
    def testMinimizationProblemWithImplicitlyConstrainedObjectiveVariable(): Unit = {
        val result = solveWithResult(task.copy(problemName = "minimization_with_implicitly_constrained_objective_variable_test"))
        val x = result.objective.objectiveVariables(1).asInstanceOf[IntegerVariable]
        assert(result.space.isImplicitlyConstrainedSearchVariable(x))
        assertEq(result.assignment.value(x), One)
    }

    @Test
    @Tag(MaximizationProblem)
    def testMaximizationProblemWithBoundedDanglingObjectiveVariable(): Unit = {
        val result = solveWithResult(task.copy(problemName = "maximization_with_bounded_dangling_objective_variable_test"))
        val x = result.objective.objectiveVariables(1).asInstanceOf[IntegerVariable]
        assert(result.space.isDanglingVariable(x))
        assertEq(result.assignment.value(x), IntegerValue(1000))
    }

    @Test
    @Tag(MaximizationProblem)
    def testMaximizationProblemWithUnboundedDanglingObjectiveVariable(): Unit = {
        // We cannot verify the solution because Gecode does not support 64 bit integers.
        val result = solveWithResult(task.copy(problemName = "maximization_with_unbounded_dangling_objective_variable_test", verificationFrequency = NoVerification))
        val x = result.objective.objectiveVariables(1).asInstanceOf[IntegerVariable]
        assert(result.space.isDanglingVariable(x))
        assertEq(result.assignment.value(x), IntegerTypeTraits.maxValue)
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(HasAllDifferentConstraint)
    def testMaximizationProblemWithImplicitlyConstrainedObjectiveVariable(): Unit = {
        val result = solveWithResult(task.copy(problemName = "maximization_with_implicitly_constrained_objective_variable_test"))
        val x = result.objective.objectiveVariables(1).asInstanceOf[IntegerVariable]
        assert(result.space.isImplicitlyConstrainedSearchVariable(x))
        assertEq(result.assignment.value(x), IntegerValue(512))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testProblemWithBoundedDanglingVariable(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bounded_dangling_variable_test"))
        val x = result.outputArray("x")
        assertEq(x.size, 4)
        assertEq(result.space.searchVariables, Set(x(0), x(1)))
        assertEq(result.space.channelVariables.filter(isUserDefined), Set(x(2)))
        assert(x(3).domain.isFinite)
        assertEq(result.space.numberOfConstraints, 4)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 1)
        assertEq(result.space.numberOfConstraints[Plus[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testProblemWithUnboundedDanglingVariable(): Unit = {
        assertThrows(
            solve(task.copy(problemName = "unbounded_dangling_variable_test")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testProblemWithBoundedIrrelevantSearchVariable(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bounded_irrelevant_search_variable_test"))
        assertEq(result.space.searchVariables.map(_.name), Set("x"))
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("r"))
        assertEq(result.space.numberOfConstraints, 2)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testProblemWithUnboundedIrrelevantSearchVariable(): Unit = {
        assertThrows(
            solve(task.copy(problemName = "unbounded_irrelevant_search_variable_test")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testProblemWithUnboundedRelevantSearchVariable(): Unit = {
        assertThrows(
            solve(task.copy(problemName = "unbounded_relevant_search_variable_test")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testBitSetCompilation(): Unit = {
        val result = solveWithResult(task.copy(sourceFormat = SourceFormat.FlatZinc, problemName = "bitset_compilation_test"))
        val l = result.outputArray("l")
        assertEq(l.size, 4)
        assertEq(l.view.map(x => result.assignment.value(x)).toSet.size, 4)
        assert(l(0).domain.isInstanceOf[SingletonIntegerSetDomain])
        assertEq(l(0).domain.asInstanceOf[SingletonIntegerSetDomain].base.getClass, classOf[SixtyFourBitSet])
        assertEq(l(0).domain.asInstanceOf[SingletonIntegerSetDomain].base, IntegerRange(1, 62))
        assert(l(1).domain.isInstanceOf[IntegerPowerSetDomain])
        assertEq(l(1).domain.asInstanceOf[IntegerPowerSetDomain].base.getClass, classOf[SixtyFourBitSet])
        assertEq(l(1).domain.asInstanceOf[IntegerPowerSetDomain].base, IntegerRange(0, 63))
        assert(l(2).domain.isInstanceOf[IntegerPowerSetDomain])
        assertEq(l(2).domain.asInstanceOf[IntegerPowerSetDomain].base.getClass, classOf[SixtyFourBitSet])
        assertEq(l(2).domain.asInstanceOf[IntegerPowerSetDomain].base, IntegerDomain(10, 11, 12, 27, 28, 29))
        assert(l(3).domain.isInstanceOf[SingletonIntegerSetDomain])
        assertEq(l(3).domain.asInstanceOf[SingletonIntegerSetDomain].base.getClass, classOf[SixtyFourBitSet])
        assertEq(l(3).domain.asInstanceOf[SingletonIntegerSetDomain].base, IntegerDomain(1, 2, 3, 7, 8, 9))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testBitSetConversion(): Unit = {
        val result = solveWithResult(task.copy(sourceFormat = SourceFormat.FlatZinc, problemName = "bitset_conversion_test"))
        val l = result.outputArray("l")
        assertEq(l.size, 4)
        assertEq(l.view.map(x => result.assignment.value(x)).toSet.size, 4)
        assert(l(0).domain.isInstanceOf[SingletonIntegerSetDomain])
        assertEq(l(0).domain.asInstanceOf[SingletonIntegerSetDomain].base.getClass, classOf[IntegerRange])
        assertEq(l(0).domain.asInstanceOf[SingletonIntegerSetDomain].base, IntegerRange(1, 62))
        assert(l(1).domain.isInstanceOf[IntegerPowerSetDomain])
        assertEq(l(1).domain.asInstanceOf[IntegerPowerSetDomain].base.getClass, classOf[IntegerRange])
        assertEq(l(1).domain.asInstanceOf[IntegerPowerSetDomain].base, IntegerRange(0, 64))
        assert(l(2).domain.isInstanceOf[IntegerPowerSetDomain])
        assertEq(l(2).domain.asInstanceOf[IntegerPowerSetDomain].base.getClass, classOf[IntegerRangeList])
        assertEq(l(2).domain.asInstanceOf[IntegerPowerSetDomain].base, IntegerDomain(10, 11, 12, 27, 28, 29))
        assert(l(3).domain.isInstanceOf[SingletonIntegerSetDomain])
        assertEq(l(3).domain.asInstanceOf[SingletonIntegerSetDomain].base.getClass, classOf[IntegerRangeList])
        assertEq(l(3).domain.asInstanceOf[SingletonIntegerSetDomain].base, IntegerDomain(1, 2, 3, 7, 8, 9))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testProblemWithDuplicateCostVariable(): Unit = {
        val result = solveWithResult(task.copy(sourceFormat = FlatZinc, problemName = "duplicate_cost_variable_test"))
        assertEq(result.space.searchVariables, Set())
        assertEq(result.space.problemParameters.size, 1)
        assertEq(result.space.channelVariables.size, 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 1)
        assertEq(result.space.numberOfConstraints, 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
    }

    // Checks that a neighbourhood is created from the redundant all_different constraint.
    @Test
    @Tag(SatisfiabilityProblem)
    def testRedundantAllDifferent(): Unit = {
        val result = solveWithResult(task.copy(problemName = "redundant_all_different_test"))
        assertEq(result.space.searchVariables.map(_.name), Set("x[1]", "x[2]", "x[3]"))
        assertEq(result.space.channelVariables.size, 5)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Ne[?, ?, ?]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        assertEq(result.space.numberOfPropagations, 4)
        assertEq(result.space.numberOfRetractions, 0)
        assert(result.neighbourhood.isInstanceOf[AllDifferentNeighbourhood[?, ?, ?]])
    }

    // Checks that the reified redundant all_different constraints are ignored.
    @Test
    @Tag(SatisfiabilityProblem)
    def testReifiedRedundantAllDifferent(): Unit = {
        val result = solveWithResult(task.copy(problemName = "reified_redundant_all_different_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.searchVariables.filter(isUserDefined).map(_.name), Set("x[1]", "x[2]", "x[3]", "y[1]", "y[2]", "y[3]"))
        assertEq(result.space.channelVariables.size, 10)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 8)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 11)
        assertEq(result.space.numberOfConstraints[Conjunction], 3)
        assertEq(result.space.numberOfConstraints[Ne[?, ?, ?]], 6)
        assertEq(result.space.numberOfConstraints[Or], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        assertEq(result.space.numberOfPropagations, 9)
        assertEq(result.space.numberOfRetractions, 0)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    // Checks that the redundant bin_packing constraint is removed after propagation.
    @Test
    @Tag(SatisfiabilityProblem)
    def testRedundantBinPacking(): Unit = {
        val result = solveWithResult(task.copy(problemName = "redundant_bin_packing_test"))
        assertEq(result.space.searchVariables.map(_.name), Set("bin[1]", "bin[2]", "bin[3]", "bin[4]", "bin[5]", "bin[6]"))
        assertEq(result.space.channelVariables.size, 40)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 36)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 41)
        assertEq(result.space.numberOfConstraints[Bool2Int1], 18)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 18)
        assertEq(result.space.numberOfConstraints[LinearConstraint[?, ?, ?]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        assertEq(result.space.numberOfPropagations, 52)
        assertEq(result.space.numberOfRetractions, 5)
    }

    // Checks that redundant constraints with free variables compile.
    // Checks that the redundant implications do not introduce additional search variables
    // via half-reified clauses.
    // Checks that the redundant implications are removed after propagation.
    // Requires --no-half-reifications to succeed.
    @Test
    @Tag(SatisfiabilityProblem)
    def testRedundantImplicationsWithFreeVariables(): Unit = {
        val result = solveWithResult(task.copy(problemName = "redundant_implications_with_free_variables_test"))
        assertEq(result.space.searchVariables.map(_.name), Set("x[1]", "x[2]", "x[3]"))
        assertEq(result.space.channelVariables.size, 2)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 3)
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        assertEq(result.space.numberOfPropagations, 21)
        assertEq(result.space.numberOfRetractions, 13)
    }

    // Checks that the inner implication does not introduce an additional search variable
    // via a half-reified clause.
    // Requires --no-half-reifications to succeed.
    @Test
    @Tag(SatisfiabilityProblem)
    def testNestedImplications(): Unit = {
        val result = solveWithResult(task.copy(problemName = "nested_implications_test"))
        assertEq(result.space.searchVariables.map(_.name), Set("c", "d", "x"))
        assertEq(result.space.channelVariables.size, 6)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 3)
        assertEq(result.space.numberOfConstraints, 7)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Le[?, ?, ?]], 5)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

}
