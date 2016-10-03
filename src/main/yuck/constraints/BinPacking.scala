package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class BinPackingItem
    [Weight <: NumericalValue[Weight]]
    (val bin: Variable[IntegerValue], val weight: Weight) {
    override def toString = "(%s, %s)".format(bin, weight)
}

/**
 * Basis for implementing MiniZinc's ''bin_packing_load'' constraint.
 *
 * Maintains the loads for a given set of bins.
 *
 * @author Michael Marte
 *
 */
final class BinPacking
    [Load <: NumericalValue[Load]]
    (id: Id[Constraint], goal: Goal,
     items: immutable.Seq[BinPackingItem[Load]],
     loads: immutable.Map[Int, Variable[Load]]) // bin -> load
    (implicit valueTraits: NumericalValueTraits[Load])
    extends Constraint(id, goal)
{

    require(loads.valuesIterator.toSet.size == loads.size)

    override def toString =
        "bin_packing([%s], [%s])".format(items.mkString(", "), loads.valuesIterator.mkString(", "))
    override def inVariables = items.toIterator.map(_.bin)
    override def outVariables = loads.valuesIterator

    private val x2Item =
        (for (item <- items) yield item.bin -> item).toMap[AnyVariable, BinPackingItem[Load]]
    private lazy val currentLoads = new mutable.HashMap[Int, Load] // bin -> load
    private val loadDeltas = new mutable.HashMap[Int, Load] // bin -> load delta
    private val effects = // bin -> effect
        (for ((i, load) <- loads) yield i -> new ReusableEffectWithFixedVariable[Load](load)).toMap

    override def initialize(now: SearchState) = {
        for (i <- loads.keysIterator) {
            currentLoads(i) = valueTraits.zero
        }
        for (item <- items) {
            currentLoads(now.value(item.bin).value) += item.weight
        }
        for (i <- loads.keysIterator) {
            val effect = effects(i)
            effect.a = currentLoads(i)
        }
        effects.valuesIterator
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
       loadDeltas.clear
       for (x <- move.involvedVariables) {
           val item = x2Item(x)
           val j = before.value(item.bin).value
           val k = after.value(item.bin).value
           if (effects.contains(j)) {
               loadDeltas += j -> (loadDeltas.get(j).getOrElse(valueTraits.zero) - item.weight)
           }
           if (effects.contains(k)) {
               loadDeltas += k -> (loadDeltas.get(k).getOrElse(valueTraits.zero) + item.weight)
           }
       }
       for ((j, loadDelta) <- loadDeltas) {
           effects(j).a = currentLoads(j) + loadDelta
       }
       loadDeltas.keysIterator.map(effects(_))
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
       for ((j, loadDelta) <- loadDeltas) {
           currentLoads(j) += loadDelta
       }
       loadDeltas.keysIterator.map(effects(_))
    }

}
