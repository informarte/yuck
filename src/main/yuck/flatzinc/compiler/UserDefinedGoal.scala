package yuck.flatzinc.compiler

import yuck.core.Goal
import yuck.flatzinc.ast.Constraint

/**
 * @author Michael Marte
 *
 */
case class UserDefinedGoal(name: String) extends Goal {
    override def toString = name
}
