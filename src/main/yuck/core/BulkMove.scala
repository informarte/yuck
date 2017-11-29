package yuck.core

import scala.collection._

/**
 * Data structure for aggregating effects into a move.
 *
 * @author Michael Marte
 */
final class BulkMove(id: Id[Move]) extends Move(id) {

    private var effectDir = new mutable.AnyRefMap[AnyVariable, AnyEffect]

    /** Adds the given effect. */
    def +=(effect: AnyEffect): BulkMove = {
        require(effectDir.put(effect.anyVariable, effect).isEmpty, "%s is re-assignment".format(effect))
        this
    }

    /** Adds the given effects. */
    def ++=(effects: TraversableOnce[AnyEffect]): BulkMove = {
        effects.foreach(this += _)
        this
    }

    override def isEmpty = effectDir.isEmpty
    override def effects = effectDir.valuesIterator
    override def size = effectDir.size
    override def involvedVariables = effectDir.keysIterator
    override def involves(x: AnyVariable) = effectDir.contains(x)
    override def anyValue(x: AnyVariable) = effectDir(x).anyValue
    override def maybeAnyValue(x: AnyVariable) = effectDir.get(x).map(_.anyValue)

}
