package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class BinPackingItem
    [Weight <: NumericalValue[Weight]]
    (val bin: IntegerVariable, val weight: Weight)
{
    override def toString = "(%s, %s)".format(bin, weight)
}

/**
 * Basis for implementing MiniZinc's ''bin_packing_load'' constraint.
 *
 * Maintains the loads for a given set of bins.
 *
 * Ignores tasks assigned to bins other than the given bins.
 *
 * @author Michael Marte
 */
final class BinPacking
    [Load <: NumericalValue[Load]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     items: immutable.Seq[BinPackingItem[Load]],
     loads: immutable.Map[Int, Variable[Load]]) // bin -> load
    (using valueTraits: NumericalValueTraits[Load])
    extends Constraint(id)
{

    require(items.forall(_.weight >= valueTraits.zero))
    require(loads.valuesIterator.toSet.size == loads.size)
    require(items.iterator.map(_.bin).toSet.size == items.size)

    override def toString =
        "bin_packing([%s], [%s])".format(
            items.mkString(", "),
            loads.iterator.map(item => "(%s, %s)".format(item._1, item._2)).mkString(", "))

    override def inVariables = items.view.filter(_.weight > valueTraits.zero).map(_.bin)
    override def outVariables = loads.view.values

    private val x2Item = items.view.map(item => (item.bin: AnyVariable) -> item).to(immutable.HashMap)
    private val currentLoads = new mutable.HashMap[Int, Load] // bin -> load
    private val loadDeltas = new mutable.HashMap[Int, Load] // bin -> load delta
    private val effects = loads.view.map((i, load) => i -> load.reuseableEffect).to(immutable.HashMap) // bin -> effect

    override def initialize(now: SearchState) = {
        currentLoads.clear()
        for (i <- loads.keysIterator) {
            currentLoads(i) = valueTraits.zero
        }
        for (item <- items) {
            val i = now.value(item.bin).toInt
            if (currentLoads.contains(i)) {
                currentLoads(i) += item.weight
            }
        }
        for (i <- loads.keysIterator) {
            val effect = effects(i)
            effect.a = currentLoads(i)
        }
        effects.view.values
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
       loadDeltas.clear()
       for (x <- move) {
           val item = x2Item(x)
           val j = before.value(item.bin).toInt
           val k = after.value(item.bin).toInt
           if (effects.contains(j)) {
               loadDeltas += j -> (loadDeltas.getOrElse(j, valueTraits.zero) - item.weight)
           }
           if (effects.contains(k)) {
               loadDeltas += k -> (loadDeltas.getOrElse(k, valueTraits.zero) + item.weight)
           }
       }
       for ((j, loadDelta) <- loadDeltas) {
           effects(j).a = currentLoads(j) + loadDelta
       }
       loadDeltas.view.keys.map(effects(_))
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
       for ((j, loadDelta) <- loadDeltas) {
           currentLoads(j) += loadDelta
       }
       loadDeltas.view.keys.map(effects(_))
    }

}
