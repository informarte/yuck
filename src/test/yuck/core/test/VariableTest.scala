package yuck.core.test

import org.junit.*

import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class VariableTest extends UnitTest {

    @Test
    def testEquality(): Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(0, 9)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        assertEq(s, s)
        assertEq(t, t)
        assertNe(s, t)
    }

    @Test
    def testPruning(): Unit = {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", NonNegativeIntegerRange)
        assert(! x.pruneDomain(x.domain))
        assert(! x.pruneDomain(CompleteIntegerRange))
        assert(x.pruneDomain(PositiveIntegerRange))
        assertEq(x.domain, PositiveIntegerRange)
        assertEx(x.pruneDomain(NegativeIntegerRange), classOf[DomainWipeOutException])
    }

    @Test
    def testDomainRestoration(): Unit = {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val backup1 = x.createDomainRestorer
        x.pruneDomain(NonNegativeIntegerRange)
        val backup2 = x.createDomainRestorer
        x.pruneDomain(PositiveIntegerRange)
        assertEq(x.domain, PositiveIntegerRange)
        backup2.apply()
        assertEq(x.domain, NonNegativeIntegerRange)
        backup1.apply()
        assertEq(x.domain, CompleteIntegerRange)
    }

}
