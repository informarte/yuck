package yuck.core

import scala.collection.*

/**
 * Data structure for aggregating effects into a move.
 *
 * @author Michael Marte
 */
final class BulkMove(id: Id[Move]) extends Move(id) {

    private val effectDir = new mutable.AnyRefMap[AnyVariable, AnyMoveEffect]

    /** Adds the given effect. */
    @inline def +=(effect: AnyMoveEffect): BulkMove = {
        require(effectDir.put(effect.x, effect).isEmpty, "%s is re-assignment".format(effect))
        this
    }

    /** Adds the given effects. */
    @inline def ++=(effects: Iterable[AnyMoveEffect]): BulkMove = {
        effects.foreach(this += _)
        this
    }

    /** Adds the given effects. */
    @inline def ++=(effects: Iterator[AnyMoveEffect]): BulkMove = {
        effects.foreach(this += _)
        this
    }

    @inline override def isEmpty = effectDir.isEmpty
    @inline override def effects = effectDir.view.values
    @inline override def effectsIterator = effectDir.valuesIterator
    @inline override def foreach[U](f: AnyVariable => U) = effectDir.foreachKey(f)
    @inline override def size = effectDir.size
    @inline override def involvedVariables = effectDir.view.keys
    @inline override def involvedVariablesIterator = effectDir.keysIterator
    @inline override def involves(x: AnyVariable) = effectDir.contains(x)
    @inline override def value(x: AnyVariable) = effectDir(x).a
    @inline override def maybeValue(x: AnyVariable) = effectDir.get(x).map(_.a)

}
