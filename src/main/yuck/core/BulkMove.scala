package yuck.core

import scala.collection._

/**
 * Data structure for aggregating effects into a move.
 *
 * @author Michael Marte
 */
final class BulkMove(id: Id[Move]) extends Move(id) {

    private var effectDir = new mutable.AnyRefMap[AnyVariable, AnyMoveEffect]

    /** Adds the given effect. */
    def +=(effect: AnyMoveEffect): BulkMove = {
        require(effectDir.put(effect.anyVariable, effect).isEmpty, "%s is re-assignment".format(effect))
        this
    }

    /** Adds the given effects. */
    def ++=(effects: Iterable[AnyMoveEffect]): BulkMove = {
        effects.foreach(this += _)
        this
    }

    /** Adds the given effects. */
    def ++=(effects: Iterator[AnyMoveEffect]): BulkMove = {
        effects.foreach(this += _)
        this
    }

    override def isEmpty = effectDir.isEmpty
    override def effects = effectDir.view.values
    override def effectsIterator = effectDir.valuesIterator
    override def foreach[U](f: AnyVariable => U) = effectDir.foreachKey(f)
    override def size = effectDir.size
    override def involvedVariables = effectDir.view.keys
    override def involvedVariablesIterator = effectDir.keysIterator
    override def involves(x: AnyVariable) = effectDir.contains(x)
    override def anyValue(x: AnyVariable) = effectDir(x).anyValue
    override def maybeAnyValue(x: AnyVariable) = effectDir.get(x).map(_.anyValue)

}
