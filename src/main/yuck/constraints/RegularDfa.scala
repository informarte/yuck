package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * This data structure defines the DFA underlying a ''regular'' constraint.
 *
 * @param xs Sequence of input variables (may contain channel and duplicate variables)
 * @param Q Number of states
 * @param S Number of inputs
 * @param delta State transition function 1..Q x 1..S -> 0..Q (0 is failed state)
 * @param q0 Start state in 1..Q
 * @param F Accepting states (subset of 1..Q)
 *
 * @author Michael Marte
 */
final class RegularDfa
    (val xs: immutable.IndexedSeq[IntegerVariable],
     val Q: Int,
     val S: Int,
     val delta: immutable.IndexedSeq[immutable.IndexedSeq[Int]],
     val q0: Int,
     val F: IntegerDomain)
{

    require(Q > 0)
    require(S > 0)
    require(delta.size == Q)
    require(delta.forall(_.size == S))
    require(delta.forall(_.forall(q => q >= 0 && q <= Q)))
    require(q0 >= 1 && q0 <= Q)
    require(F.isSubsetOf(IntegerRange(1, Q)))

}
