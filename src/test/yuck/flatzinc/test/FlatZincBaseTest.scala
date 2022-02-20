package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.*

import scala.language.implicitConversions

import yuck.constraints.{Alldistinct, ElementConst, ElementVar, IfThenElse}
import yuck.core.*
import yuck.flatzinc.compiler.{Bool2Int1, VariableWithInfiniteDomainException}
import yuck.flatzinc.test.util.*
import yuck.test.util.ParallelTestRunner

/**
 * Tests that cover edge cases and rarely used features of the FlatZinc language
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[ParallelTestRunner])
final class FlatZincBaseTest extends FrontEndTest {

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testVarArrayAccess(): Unit = {
        val result = solveWithResult(task.copy(problemName = "var_array_access"))
        assertEq(numberOfConstraints[ElementVar[_]](result), 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testConstArrayAccess(): Unit = {
        val result = solveWithResult(task.copy(problemName = "const_array_access"))
        assertEq(numberOfConstraints[ElementConst[_]](result), 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testArrayAccessWhereResultMustEqualIndex(): Unit = {
        solve(task.copy(problemName = "array_access_where_result_must_equal_index"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testArrayAccessWhereIndexIsChannelVariable(): Unit = {
        solve(task.copy(problemName = "array_access_where_index_is_channel_variable"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testArrayAccessWithConstrainedIndexVariables(): Unit = {
        val result = solveWithResult(task.copy(problemName = "array_access_with_constrained_index_variables"))
        assertEq(searchVariables(result).size, 12)
        assert(! searchVariables(result).exists(_.name == "x[5]"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_bool_test"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 2)
        assertEq(searchVariables(result).map(_.name), Set("c"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseBoolWithConstCondition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_bool_test_with_const_condition"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result), Set())
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseBoolWithEqualAlternatives(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_bool_test_with_equal_alternatives"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result), Set())
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseVarBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_var_bool_test"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 2)
        assertEq(searchVariables(result).map(_.name), Set("c", "d", "e"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseVarBoolWithConstCondition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_var_bool_test_with_const_condition"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result).map(_.name), Set("e"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_int_test"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 2)
        assertEq(searchVariables(result).map(_.name), Set("c"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseIntInsteadBool2Int(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_int_instead_bool2int_test"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(numberOfConstraints[Bool2Int1](result), 1)
        assertEq(searchVariables(result).map(_.name), Set("c", "y"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseIntWithConstCondition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_int_test_with_const_condition"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result), Set())
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseIntWithEqualAlternatives(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_int_test_with_equal_alternatives"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result), Set())
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseVarInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_var_int_test"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 2)
        assertEq(searchVariables(result).map(_.name), Set("c", "u", "v"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseVarIntWithConstCondition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_var_int_test_with_const_condition"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result).map(_.name), Set("u", "v"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_set_test"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 2)
        assertEq(searchVariables(result).map(_.name), Set("c"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseSetWithConstCondition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_set_test_with_const_condition"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result), Set())
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseSetWithEqualAlternatives(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_set_test_with_equal_alternatives"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result), Set())
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseVarSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_var_set_test"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 2)
        assertEq(searchVariables(result).map(_.name), Set("c", "u", "v"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testIfThenElseVarSetWithConstCondition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "if_then_else_var_set_test_with_const_condition"))
        assertEq(numberOfConstraints[IfThenElse[_]](result), 0)
        assertEq(searchVariables(result).map(_.name), Set("u", "v"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testInconsistentProblem(): Unit = {
        assertEx(
            solve(task.copy(problemName = "empty_table_int_test")),
            classOf[InconsistentProblemException])
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationOfSumWithNegativeAddends(): Unit = {
        solve(task.copy(problemName = "minimization_of_sum_with_negative_addends"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationProblemWithBoundedDanglingObjectiveVariable(): Unit = {
        solve(task.copy(problemName = "minimization_problem_with_bounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationProblemWithUnboundedDanglingObjectiveVariable(): Unit = {
        solve(task.copy(problemName = "minimization_problem_with_unbounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def testMinimizationProblemWithImplicitlyConstrainedObjectiveVariable(): Unit = {
        val result = solveWithResult(task.copy(problemName = "minimization_problem_with_implicitly_constrained_objective_variable"))
        assertEq(numberOfConstraints[Alldistinct[_]](result), 1)
        assert(result.space.isImplicitlyConstrainedSearchVariable(result.objective.objectiveVariables(1)))
        assertEq(quality(result), One)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testMaximizationProblemWithBoundedDanglingObjectiveVariable(): Unit = {
        solve(task.copy(problemName = "maximization_problem_with_bounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testMaximizationProblemWithUnboundedDanglingObjectiveVariable(): Unit = {
        solve(task.copy(problemName = "maximization_problem_with_unbounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def testMaximizationProblemWithImplicitlyConstrainedObjectiveVariable(): Unit = {
        val result = solveWithResult(task.copy(problemName = "maximization_problem_with_implicitly_constrained_objective_variable"))
        assertEq(numberOfConstraints[Alldistinct[_]](result), 1)
        assert(result.space.isImplicitlyConstrainedSearchVariable(result.objective.objectiveVariables(1)))
        assertEq(quality(result), IntegerValue(512))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithBoundedDanglingVariable(): Unit = {
        solve(task.copy(problemName = "problem_with_bounded_dangling_variable"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedDanglingVariable(): Unit = {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_dangling_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithBoundedIrrelevantSearchVariable(): Unit = {
        solve(task.copy(problemName = "problem_with_bounded_irrelevant_search_variable"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedIrrelevantSearchVariable(): Unit = {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_irrelevant_search_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedRelevantSearchVariable(): Unit = {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_relevant_search_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

}
