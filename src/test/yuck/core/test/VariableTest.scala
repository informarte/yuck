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
    def testPruning {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        assert(! x.pruneDomain(x.domain))
        assert(x.pruneDomain(NonNegativeIntegerRange))
        assertEq(x.domain, NonNegativeIntegerRange)
        assertEx(x.pruneDomain(NegativeIntegerRange), classOf[DomainWipeOutException])
    }

}
