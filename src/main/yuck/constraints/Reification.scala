package yuck.constraints

import yuck.core._

/**
 * Implements reification for any type of cost.
 *
 * Let r be a Boolean variable and let x and y be numerical variables.
 * (Consider x as the cost variable of the reified constraint.)
 * Then, roughly speaking, Reification(x, r, y) computes y from x and r to as follows:
 *
 * {{{
 * x = 0     r         y
 * true      true      0
 * true      false     1
 * false     true      x
 * false     false     0
 * }}}
 *
 * Rationale for passing on the costs of the reified constraint in case the latter should
 * hold (according to r): This approach smoothes the cost landscape and helps to guide the
 * search towards a solution.
 *
 * @author Michael Marte
 */
final class Reification
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], r: Variable[BooleanValue], y: Variable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends Constraint(id, goal)
{

    override def toString = "reification(%s, %s, %s)".format(x, r, y)
    override def inVariables = List(x, r)
    override def outVariables = List(y)

    private val effects = List(new ReusableEffectWithFixedVariable[Value](y))
    private val effect = effects.head

    override def initialize(now: SearchState) = {
        effect.a = computeCosts(now)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)

    override def commit(before: SearchState, after: SearchState, move: Move) =
        effects

    private def computeCosts(searchState: SearchState): Value = {
        val a = searchState.value(x)
        val zero = valueTraits.zero
        val one = valueTraits.one
        if (searchState.value(r).value) a else if (a == zero) one else zero
    }

}
