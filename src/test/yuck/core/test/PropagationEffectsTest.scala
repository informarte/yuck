package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
  * @author Michael Marte
  *
  */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class PropagationEffectsTest extends UnitTest {

    @Test
    def testPruning1 {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val effects1 = NoPropagationOccurred.pruneDomain(x, x.domain)
        assert(effects1.affectedVariables.isEmpty)
        assert(! effects1.rescheduleStep)
        val effects2 = NoPropagationOccurred.pruneDomain(x, NonNegativeIntegerRange)
        assertEq(effects2.affectedVariables, Set(x))
        assert(effects2.rescheduleStep)
        assertEq(NonNegativeIntegerRange, x.domain)
        assertEx(NoPropagationOccurred.pruneDomain(x, NegativeIntegerRange), classOf[DomainWipeOutException])
    }

    @Test
    def testPruning2 {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val effects1 = NoPropagationOccurred.pruneDomains(x, x.domain, y, y.domain)
        assert(effects1.affectedVariables.isEmpty)
        assert(! effects1.rescheduleStep)
        val effects2 = NoPropagationOccurred.pruneDomains(x, NegativeIntegerRange, y, NonNegativeIntegerRange)
        assertEq(effects2.affectedVariables, Set(x, y))
        assert(effects2.rescheduleStep)
        assertEq(x.domain, NegativeIntegerRange)
        assertEq(y.domain, NonNegativeIntegerRange)
    }

    @Test
    def testPruning3 {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val z = space.createVariable("z", CompleteIntegerRange)
        val effects1 = NoPropagationOccurred.pruneDomains(x, x.domain, y, y.domain, z, z.domain)
        assert(effects1.affectedVariables.isEmpty)
        assert(! effects1.rescheduleStep)
        val effects2 = NoPropagationOccurred.pruneDomains(x, NegativeIntegerRange, y, y.domain, z, PositiveIntegerRange)
        assertEq(effects2.affectedVariables, Set(x, z))
        assert(effects2.rescheduleStep)
        assertEq(x.domain, NegativeIntegerRange)
        assertEq(y.domain, CompleteIntegerRange)
        assertEq(z.domain, PositiveIntegerRange)
    }

    @Test
    def testPruningN {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val z = space.createVariable("z", CompleteIntegerRange)
        val effects1 = NoPropagationOccurred.pruneDomains(List((x, x.domain), (y, y.domain), (z, z.domain)))
        assert(effects1.affectedVariables.isEmpty)
        assert(! effects1.rescheduleStep)
        val effects2 = NoPropagationOccurred.pruneDomains(List((x, NegativeIntegerRange), (y, y.domain), (z, PositiveIntegerRange)))
        assertEq(effects2.affectedVariables, Set(x, z))
        assert(effects2.rescheduleStep)
        assertEq(x.domain, NegativeIntegerRange)
        assertEq(y.domain, CompleteIntegerRange)
        assertEq(z.domain, PositiveIntegerRange)
    }

}
