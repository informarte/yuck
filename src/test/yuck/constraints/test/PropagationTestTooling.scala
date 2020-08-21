package yuck.constraints.test

import scala.collection._
import scala.language.implicitConversions

import yuck.core._
import yuck.util.logging.LazyLogger
import yuck.util.testing.YuckAssert

/**
 * @author Michael Marte
 *
 */
trait PropagationTestTooling extends YuckAssert {

    protected val logger: LazyLogger

    protected abstract class AnyDomainReduction {
        def perform: Unit
        def check: Unit
    }
    protected final class DomainReduction[Value <: AnyValue](x: Variable[Value], dx: Domain[Value]) extends AnyDomainReduction {
        override def perform = x.pruneDomain(dx)
        override def check = assertEq(x.domain, dx)
    }

    // Helper class to facilitate implicit conversions
    protected case class Setup(setup: () => Unit) {
        def apply(): Unit = setup()
    }

    // Helper class to facilitate implicit conversions
    protected case class Check(check: () => Unit) {
        def apply(): Unit = check()
    }

    protected abstract class PropagationTestStep
    protected case class Propagate(comment: String, setup: Setup, check: Check) extends PropagationTestStep
    protected case class PropagateAndRollback(comment: String, setup: Setup, check: Check) extends PropagationTestStep

    protected case class PropagationTestScenario(space: Space, steps: PropagationTestStep*)

    protected implicit def createDomainReduction[Value <: AnyValue](reduction: (Variable[Value], Domain[Value])) =
        new DomainReduction[Value](reduction._1, reduction._2)

    protected implicit def createSetup[Value <: AnyValue](reduction: (Variable[Value], Domain[Value])) =
        Setup(() => new DomainReduction[Value](reduction._1, reduction._2).perform)
    protected implicit def createSetup(reductions: List[AnyDomainReduction]) =
        Setup(() => reductions.foreach(_.perform))
    protected implicit def createSetup(setup: () => Unit) =
        Setup(setup)

    protected implicit def createCheck[Value <: AnyValue](reduction: (Variable[Value], Domain[Value])) =
        Check(() => new DomainReduction[Value](reduction._1, reduction._2).check)
    protected implicit def createCheck(reductions: List[AnyDomainReduction]) =
        Check(() => reductions.foreach(_.check))
    protected implicit def createCheck(check: () => Unit) =
        Check(check)

    protected def runScenario(scenario: PropagationTestScenario): Unit = {
        val space = scenario.space
        val steps = scenario.steps
        for (step <- steps) step match {
            case Propagate(comment, setup, check) =>
                logger.log(comment)
                setup()
                try {
                    space.propagate
                } catch {
                    case _: DomainWipeOutException =>
                }
                check()
            case PropagateAndRollback(comment, setup, check) =>
                logger.log(comment)
                val checkpoint = new mutable.ArrayBuffer[() => Unit]
                for (x <- space.searchVariables) {
                    checkpoint += x.createDomainRestorer
                }
                for (x <- space.channelVariables) {
                    checkpoint += x.createDomainRestorer
                }
                setup()
                try {
                    space.propagate
                } catch {
                    case _: DomainWipeOutException =>
                }
                check()
                for (action <- checkpoint) {
                    action.apply()
                }
        }
    }

}
