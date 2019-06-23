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
final class VariableTest extends UnitTest {

    @Test
    def testEquality {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        assertEq(s, s)
        assertEq(t, t)
        assertNe(s, t)
    }

    @Test
    def testPruning1 {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        assertEq(false, x.pruneDomain(x.domain))
        assertEq(true, x.pruneDomain(NonNegativeIntegerRange))
        assertEq(x.domain, NonNegativeIntegerRange)
        assertEx(x.pruneDomain(NegativeIntegerRange), classOf[DomainWipeOutException])
    }

    @Test
    def testPruning2 {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        assertEq(false, Variable.pruneDomains(x, x.domain, y, y.domain))
        assertEq(true, Variable.pruneDomains(x, NegativeIntegerRange, y, NonNegativeIntegerRange))
        assertEq(NegativeIntegerRange, x.domain)
        assertEq(NonNegativeIntegerRange, y.domain)
    }

    @Test
    def testPruning3 {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val z = space.createVariable("z", CompleteIntegerRange)
        assertEq(false, Variable.pruneDomains(x, x.domain, y, y.domain, z, z.domain))
        assertEq(true, Variable.pruneDomains(x, NegativeIntegerRange, y, y.domain, z, PositiveIntegerRange))
        assertEq(NegativeIntegerRange, x.domain)
        assertEq(CompleteIntegerRange, y.domain)
        assertEq(PositiveIntegerRange, z.domain)
    }

    @Test
    def testPruningN {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val z = space.createVariable("z", CompleteIntegerRange)
        assertEq(false, Variable.pruneDomains(List((x, x.domain), (y, y.domain), (z, z.domain))))
        assertEq(true, Variable.pruneDomains(List((x, NegativeIntegerRange), (y, y.domain), (z, PositiveIntegerRange))))
        assertEq(NegativeIntegerRange, x.domain)
        assertEq(CompleteIntegerRange, y.domain)
        assertEq(PositiveIntegerRange, z.domain)
    }

}
