package yuck.core

/**
 * @author Michael Marte
 *
 */
final class DummyConstraint
    (id: Id[Constraint],
     override val inVariables: Traversable[AnyVariable],
     override val outVariables: Traversable[AnyVariable])
    extends Constraint(id, null)
{
    override def initialize(now: SearchState) = Nil
    override def consult(before: SearchState, after: SearchState, move: Move) = Nil
}
