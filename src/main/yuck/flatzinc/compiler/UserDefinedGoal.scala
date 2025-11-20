package yuck.flatzinc.compiler

import yuck.core.Goal
import yuck.flatzinc.ast.Constraint

final case class UserDefinedGoal(name: String) extends Goal {
    override def toString = name
}
