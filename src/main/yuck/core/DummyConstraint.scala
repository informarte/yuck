package yuck.core

/**
 * @author Michael Marte
 *
 */
final class DummyConstraint
    (id: Id[Constraint],
     override val inVariables: Iterable[AnyVariable],
     override val outVariables: Iterable[AnyVariable])
    extends Constraint(id, null)
{
    override def initialize(now: SearchState) = Nil
    override def consult(before: SearchState, after: SearchState, move: Move) = Nil
}
