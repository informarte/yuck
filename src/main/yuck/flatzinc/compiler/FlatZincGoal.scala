package yuck.flatzinc.compiler

import yuck.core.Goal
import yuck.flatzinc.ast.Constraint

final case class FlatZincGoal(constraint: Constraint) extends Goal {
    override def toString = constraint.toString
}
