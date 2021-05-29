package yuck.constraints.test

import org.junit.Assert

import scala.language.implicitConversions

import yuck.core._

/**
 * A small DSL for testing assignment propagation
 *
 * @author Michael Marte
 */
trait AssignmentPropagationTestTooling extends ConstraintTestTooling {

    private def checkAssignments(searchState: SearchState, effects: List[AnyMoveEffect]): Unit = {
        for (effect <- effects) {
            val a = searchState.value(effect.x)
            if (a != effect.a) {
                Assert.fail("Value of %s is %s, but expected %s".format(effect.x, a, effect.a))
            }
        }
    }

    protected case class Initialize
        (comment: String, assignment: List[AnyMoveEffect], effects: List[AnyMoveEffect])
        extends TestStep
    {
        override def run(space: Space) = {
            logger.log("Initialize: %s".format(comment))
            for (effect <- assignment) {
                effect.affect(space)
            }
            space.initialize()
            checkAssignments(space.searchState, effects)
        }
    }
    protected object Initialize {
        def apply(comment: String, effects: AnyMoveEffect*): Initialize =
            Initialize(comment, effects.take(effects.size - 1).toList, List(effects.last))
    }

    protected case class Consult
        (comment: String, move: List[AnyMoveEffect], effects: List[AnyMoveEffect])
        extends TestStep
    {
        override def run(space: Space) = {
            logger.log("Consult: %s".format(comment))
            val move = new ChangeAnyValues(space.nextMoveId, this.move)
            val after = space.consult(move)
            checkAssignments(after, effects)
        }
    }
    protected object Consult {
        def apply(comment: String, effects: AnyMoveEffect*): Consult =
            Consult(comment, effects.take(effects.size - 1).toList, List(effects.last))
    }

    protected case class ConsultAndCommit
        (comment: String, move: List[AnyMoveEffect], effects: List[AnyMoveEffect])
        extends TestStep
    {
        override def run(space: Space) = {
            logger.log("ConsultAndCommit: %s".format(comment))
            val move = new ChangeAnyValues(space.nextMoveId, this.move)
            val after = space.consult(move)
            space.commit(move)
            checkAssignments(after, effects)
        }
    }
    protected object ConsultAndCommit {
        def apply(comment: String, effects: AnyMoveEffect*): ConsultAndCommit =
            ConsultAndCommit(comment, effects.take(effects.size - 1).toList, List(effects.last))
    }

    protected implicit def createMoveEffect[Value <: AnyValue](assignment: (Variable[Value], Value)) =
        new ImmutableMoveEffect[Value](assignment._1, assignment._2)

}
