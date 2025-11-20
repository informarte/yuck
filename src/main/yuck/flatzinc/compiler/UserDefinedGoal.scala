package yuck.flatzinc.compiler

import yuck.core.Goal
import yuck.flatzinc.ast.Constraint

/**
 * @author Michael Marte
 *
 */
final case class UserDefinedGoal(name: String) extends Goal {
    override def toString = name
}
