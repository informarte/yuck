package yuck.core

import scala.collection._

/**
 * Data structure for aggregating effects into a move.
 *
 * @author Michael Marte
 */
final class BulkMove(id: Id[Move]) extends Move(id) {

    private var map = new mutable.AnyRefMap[AnyVariable, AnyEffect]

    /** Adds the given effect. */
    def +=(effect: AnyEffect): BulkMove = {
        require(map.put(effect.anyVariable, effect).isEmpty, "%s is re-assignment".format(effect))
        this
    }

    /** Adds the given effects. */
    def ++=(effects: TraversableOnce[AnyEffect]): BulkMove = {
        effects.foreach(this += _)
        this
    }

    override def isEmpty = map.isEmpty
    override def effects = map.valuesIterator
    override def size = map.size
    override def involvedVariables = map.keysIterator
    override def involves(x: AnyVariable) = map.get(x).isDefined
    override def maybeAnyValue(x: AnyVariable) = map.get(x).map(_.anyValue)

}
