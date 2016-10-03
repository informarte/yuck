package yuck.flatzinc

/**
 * @author Michael Marte
 *
 */
package object runner {

    val FLATZINC_SOLUTION_SEPARATOR = "----------"
    // The FlatZinc 1.6 specification requires to print the following indicator when the
    // whole search space was explored.
    // Assuming a branch & bound search, the MiniZinc challenge scripts rely on this line
    // to detect solutions that were proven optimal.
    // As local search does not explore systematically, we print the indicator only in
    // the latter case.
    val FLATZINC_BEST_SOLUTION_FOUND_INDICATOR = "=========="
    val FLATZINC_INCONSISTENT_PROBLEM_INDICATOR = "=====UNSATISFIABLE====="
    val FLATZINC_NO_SOLUTION_FOUND_INDICATOR = "=====UNKNOWN====="

}
