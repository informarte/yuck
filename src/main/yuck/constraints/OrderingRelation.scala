package yuck.constraints

/**
 * @author Michael Marte
 *
 */
trait OrderingRelation

case object EqRelation extends OrderingRelation {
    override def toString = "=="
}

case object NeRelation extends OrderingRelation {
    override def toString = "!="
}

case object LtRelation extends OrderingRelation {
    override def toString = "<"
}

case object LeRelation extends OrderingRelation {
    override def toString = "=<"
}
