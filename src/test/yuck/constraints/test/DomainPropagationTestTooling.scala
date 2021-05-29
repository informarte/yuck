package yuck.constraints.test

import org.junit.Assert

import scala.collection.mutable
import scala.language.implicitConversions

import yuck.core._

/**
 * A small DSL for testing domain propagation
 *
 * @author Michael Marte
 */
trait DomainPropagationTestTooling extends ConstraintTestTooling {

    protected final class Setup(setup: () => Unit) {
        def apply(): Unit = setup()
    }

    protected final class Check(check: () => Unit) {
        def apply(): Unit = check()
    }

    protected case class Propagate(comment: String, setup: Setup, check: Check) extends TestStep {
        override def run(space: Space) = {
            logger.log("Propagate: %s".format(comment))
            setup()
            try {
                space.propagate()
            } catch {
                case _: DomainWipeOutException =>
            }
            check()
        }
    }

    protected case class PropagateAndRollback(comment: String, setup: Setup, check: Check) extends TestStep {
        override def run(space: Space) = {
            logger.log("PropagateAndRollback: %s".format(comment))
            val checkpoint = new mutable.ArrayBuffer[() => Unit]
            for (x <- space.searchVariables) {
                checkpoint += x.createDomainRestorer
            }
            for (x <- space.channelVariables) {
                checkpoint += x.createDomainRestorer
            }
            setup()
            try {
                space.propagate()
            } catch {
                case _: DomainWipeOutException =>
            }
            check()
            for (action <- checkpoint) {
                action.apply()
            }
        }
    }

    protected abstract class AnyDomainReduction {
        def perform: Unit
        def check: Unit
    }
    protected final class DomainReduction
        [Value <: AnyValue]
        (x: Variable[Value], dx: Domain[Value])
        extends AnyDomainReduction
    {
        override def perform = x.pruneDomain(dx)
        override def check = {
            if (x.domain != dx) {
                Assert.fail("Domain of %s is %s, but expected %s".format(x, x.domain, dx))
            }
        }
    }

    protected implicit def createDomainReduction[Value <: AnyValue](reduction: (Variable[Value], Domain[Value])) =
        new DomainReduction[Value](reduction._1, reduction._2)

    protected implicit def createSetup(setup: () => Unit): Setup =
        new Setup(setup)
    protected implicit def createSetup1(reductions: List[AnyDomainReduction]): Setup =
        createSetup(() => reductions.foreach(_.perform))
    protected implicit def createSetup2[Value <: AnyValue](reduction: (Variable[Value], Domain[Value])): Setup =
        createSetup1(List(new DomainReduction[Value](reduction._1, reduction._2)))
    protected implicit def createSetup3[Value <: AnyValue](reductions: List[(Variable[Value], Domain[Value])]): Setup =
        createSetup1(for (reduction <- reductions) yield new DomainReduction[Value](reduction._1, reduction._2))
    protected implicit def createSetup4(nil: scala.collection.immutable.Nil.type) =
        createSetup(() => {})

    protected implicit def createCheck(check: () => Unit): Check =
        new Check(check)
    protected implicit def createCheck1(reductions: List[AnyDomainReduction]): Check =
        createCheck(() => reductions.foreach(_.check))
    protected implicit def createCheck2[Value <: AnyValue](reduction: (Variable[Value], Domain[Value])): Check =
        createCheck1(List(new DomainReduction[Value](reduction._1, reduction._2)))
    protected implicit def createCheck3[Value <: AnyValue](reductions: List[(Variable[Value], Domain[Value])]): Check =
        createCheck1(for (reduction <- reductions) yield new DomainReduction[Value](reduction._1, reduction._2))

}
