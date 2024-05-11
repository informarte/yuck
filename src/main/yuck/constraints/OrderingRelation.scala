package yuck.constraints

/**
 * @author Michael Marte
 *
 */
enum OrderingRelation(val name: String) {

    case EqRelation extends OrderingRelation("==")
    case NeRelation extends OrderingRelation("!=")
    case LtRelation extends OrderingRelation("<")
    case LeRelation extends OrderingRelation("=<")

    override def toString = name

}
