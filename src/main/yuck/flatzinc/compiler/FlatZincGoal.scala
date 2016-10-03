package yuck.flatzinc.compiler

import yuck.core.Goal
import yuck.flatzinc.ast.Constraint

/**
 * @author Michael Marte
 *
 */
final class FlatZincGoal(val constraint: Constraint) extends Goal(constraint.toString) {
}
