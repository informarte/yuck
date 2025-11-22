package yuck.flatzinc.compiler

import yuck.core.Goal

final case class UserDefinedGoal(name: String) extends Goal {
    override def toString = name
}
