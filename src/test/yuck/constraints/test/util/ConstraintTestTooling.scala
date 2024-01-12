package yuck.constraints.test.util

import org.junit.Assert

import yuck.core.{given, *}
import yuck.test.util.YuckAssert
import yuck.util.logging.LazyLogger

/**
 * A small DSL for testing constraints
 *
 * Useful explanation of the interaction between overload resolution and implicit conversions:
 * https://users.scala-lang.org/t/how-does-overload-resolution-interact-with-implicit-conversions/7836/5
 *
 * @author Michael Marte
 */
trait ConstraintTestTooling extends YuckAssert {

    protected val logger: LazyLogger

    protected abstract class TestStep {
        def run(space: Space): Unit
    }

    protected final case class TestScenario(space: Space, steps: TestStep*)

    protected def runScenario(scenario: TestScenario): Unit = {
        scenario.space.searchVariables.foreach(scenario.space.registerObjectiveVariable)
        scenario.space.channelVariables.foreach(scenario.space.registerObjectiveVariable)
        for (step <- scenario.steps) {
            step.run(scenario.space)
        }
    }

    protected abstract class AnyDomainReduction {
        val x: AnyVariable
        val dx: AnyDomain
        final override def toString = "(%s, %s)".format(x, dx)
        def perform(): Unit
        def check(): Unit
    }

    protected final class DomainReduction
        [V <: Value[V]]
        (override val x: Variable[V], override val dx: Domain[V])
        extends AnyDomainReduction
    {
        override def perform() = x.pruneDomain(dx)
        override def check() = {
            if (x.domain != dx) {
                Assert.fail("Domain of %s is %s, but expected %s".format(x, x.domain, dx))
            }
        }
    }

    private def propagate(space: Space, preconditions: Seq[AnyDomainReduction], postconditions: Seq[AnyDomainReduction]): Unit = {
        logger.log("   Given: %s".format(preconditions.mkString(", ")))
        logger.log("Expected: %s".format(postconditions.mkString(", ")))
        preconditions.foreach(_.perform())
        val checkedVariables = postconditions.view.map(_.x).toSet
        val otherVariables = space.searchVariables.union(space.channelVariables).diff(checkedVariables)
        val initialDomains = otherVariables.view.map(x => (x, x.domain)).toMap
        try {
            space.propagate()
        } catch {
            case _: DomainWipeOutException =>
        }
        logger.log("  Result: %s".format(postconditions.map(r => "(%s, %s)".format(r.x, r.x.domain)).mkString(", ")))
        postconditions.foreach(_.check())
        for (x <- otherVariables) {
            if (x.domain != initialDomains(x)) {
                Assert.fail("Domain of %s is %s, but expected %s".format(x, x.domain, initialDomains(x)))
            }
        }
    }

    /**
     * Performs the given domain reductions, calls space.propagate, and checks whether the expected reductions
     * have happened.
     *
     * Also checks that only the expected reductions have happened.
     */
    protected final case class Propagate
        (comment: String, preconditions: Seq[AnyDomainReduction], postconditions: Seq[AnyDomainReduction])
        extends TestStep
    {
        override def run(space: Space) = {
            logger.withLogScope("Propagate: %s".format(comment)) {
                propagate(space, preconditions, postconditions)
            }
        }
    }

    /**
     * Performs the given domain reductions, calls space.propagate, checks whether the expected reductions
     * have happened, and rolls back all changes.
     *
     * Also checks that only the expected reductions have happened.
     */
    protected final case class PropagateAndRollback
        (comment: String, preconditions: Seq[AnyDomainReduction], postconditions: Seq[AnyDomainReduction])
        extends TestStep
    {
        override def run(space: Space) = {
            logger.withLogScope("PropagateAndRollback: %s".format(comment)) {
                val checkpoint = space.searchVariables.union(space.channelVariables).map(_.createDomainRestorer)
                propagate(space, preconditions, postconditions)
                checkpoint.foreach(_.apply())
            }
        }
    }

    private def checkAssignments
        (preconditions: Seq[AnyMoveEffect], postconditions: Seq[AnyMoveEffect],
         before: SearchState, after: SearchState):
        Unit =
    {
        logger.log("   Given: %s".format(preconditions.mkString(", ")))
        logger.log("Expected: %s".format(postconditions.mkString(", ")))
        logger.log("  Result: %s".format(postconditions.map(effect => "(%s, %s)".format(effect.x, after.value(effect.x))).mkString(", ")))
        val effects = preconditions.concat(postconditions)
        for (effect <- effects) {
            val a = after.value(effect.x)
            if (a != effect.a) {
                Assert.fail("Value of %s is %s, but expected %s".format(effect.x, a, effect.a))
            }
        }
        val checkedVariables = effects.view.map(_.x).toSet
        val otherVariables = before.mappedVariables.diff(checkedVariables)
        for (x <- otherVariables) {
            val a = after.value(x)
            val b = before.value(x)
            if (a != b) {
                Assert.fail("Value of %s is %s, but expected %s".format(x, a, b))
            }
        }
    }

    /**
     * Performs the given variable assignments, calls space.initialize, and checks whether the expected
     * assignments have happened.
     *
     * Also checks that only the expected assignments have happened.
     */
    protected final case class Initialize
        (comment: String, preconditions: Seq[AnyMoveEffect], postconditions: Seq[AnyMoveEffect])
        extends TestStep
    {
        override def run(space: Space) = {
            logger.withLogScope("Initialize: %s".format(comment)) {
                preconditions.foreach(_.affect(space))
                checkAssignments(preconditions, postconditions, space.searchState.clone(), space.initialize().searchState)
            }
        }
    }

    protected object Initialize {
        def apply(comment: String, effects: AnyMoveEffect*): Initialize =
            Initialize(comment, effects.take(effects.size - 1), List(effects.last))
    }

    /**
     * Calls space.consult on the given move and checks the result for the expected effects.
     *
     * Also checks that there are no other effects than the expected ones.
     */
    protected final case class Consult
        (comment: String, preconditions: Seq[AnyMoveEffect], postconditions: Seq[AnyMoveEffect])
        extends TestStep
    {
        override def run(space: Space) = {
            logger.withLogScope("Consult: %s".format(comment)) {
                val move = new ChangeAnyValues(space.nextMoveId(), preconditions)
                checkAssignments(preconditions, postconditions, space.searchState.clone(), space.consult(move))
            }
        }
    }

    protected object Consult {
        def apply(comment: String, effects: AnyMoveEffect*): Consult =
            Consult(comment, effects.take(effects.size - 1), List(effects.last))
    }

    /**
     * Calls space.consultAndCommit on the given move and checks whether the expected
     * assignments have happened.
     *
     * Also checks that only the expected assignments have happened.
     */
    protected final case class ConsultAndCommit
        (comment: String, preconditions: Seq[AnyMoveEffect], postconditions: Seq[AnyMoveEffect])
        extends TestStep
    {
        override def run(space: Space) = {
            logger.withLogScope("ConsultAndCommit: %s".format(comment)) {
                val move = new ChangeAnyValues(space.nextMoveId(), preconditions)
                val before = space.searchState.clone()
                logger.withLogScope("Consult:") {
                    checkAssignments(preconditions, postconditions, before, space.consult(move))
                }
                logger.withLogScope("Commit:") {
                    checkAssignments(preconditions, postconditions, before, space.commit(move).searchState)
                }
            }
        }
    }

    protected object ConsultAndCommit {
        def apply(comment: String, effects: AnyMoveEffect*): ConsultAndCommit =
            ConsultAndCommit(comment, effects.take(effects.size - 1), List(effects.last))
    }

    extension [V <: Value[V]](x: Variable[V]) {
        def <<(dx: Domain[V]): DomainReduction[V] = DomainReduction(x, dx)
        def <<(values: Iterable[V])(using valueTraits: ValueTraits[V]): DomainReduction[V] =
            DomainReduction(x, valueTraits.createDomain(values.toSet))
        def <<(a: V): MoveEffect[V] = ImmutableMoveEffect(x, a)
    }

    extension (x: IntegerVariable) {
        def <<(range: (Int, Int)): DomainReduction[IntegerValue] = DomainReduction(x, IntegerRange(range._1, range._2))
        def <<(values: Iterable[Int]): DomainReduction[IntegerValue] = DomainReduction(x, IntegerDomain(values))
        def <<(a: Int): MoveEffect[IntegerValue] = ImmutableMoveEffect(x, IntegerValue(a))
    }

}
