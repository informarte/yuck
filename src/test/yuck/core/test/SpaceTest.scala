package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class SpaceTest extends UnitTest {

    @Test
    def testBasicNetworkManagement {

        /*
         * s, t -> c -> u    \
         *                    ->  e -> y
         * v    -> d -> w, x /
         *
         * with variables s, ..., z and constraint c, d, and e.
         */
        val space = new Space(logger)
        def domain(name: Char) = if (name == 'v') ZeroIntegerDomain else UnboundedIntegerDomain
        val vars @ IndexedSeq(s, t, u, v, w, x, y, z): IndexedSeq[AnyVariable] =
            for (name <- 's' to 'z') yield space.createVariable(name.toString, domain(name))
        val c = new DummyConstraint(space.constraintIdFactory.nextId, List(s, t), List(u))
        val d = new DummyConstraint(space.constraintIdFactory.nextId, List(s, v), List(w, x))
        val e = new DummyConstraint(space.constraintIdFactory.nextId, List(u, w, x), List(y))
        space.post(c).post(d).post(e)

        val problemParams = space.problemParameters
        assertEq(problemParams, Set(v))
        val channelVars = space.channelVariables
        assertEq(channelVars, Set(u, w, x, y))
        val searchVars = space.searchVariables
        assertEq(searchVars, Set(s, t))
        for (x <- vars) {
            assertEq(space.isProblemParameter(x), problemParams.contains(x))
            assertEq(space.isChannelVariable(x), channelVars.contains(x))
            assertEq(space.isSearchVariable(x), searchVars.contains(x))
            assertEq(
                space.isDanglingVariable(x),
                ! problemParams.contains(x) && ! channelVars.contains(x) && ! searchVars.contains(x))
        }

        assertEq(space.directlyAffectedConstraints(s), Set(c, d))
        assertEq(space.directlyAffectedConstraints(t), Set(c))
        assertEq(space.directlyAffectedConstraints(u), Set(e))
        assertEq(space.directlyAffectedConstraints(v), Set())
        assertEq(space.directlyAffectedConstraints(w), Set(e))
        assertEq(space.directlyAffectedConstraints(x), Set(e))
        assertEq(space.directlyAffectedConstraints(y), Set())

        for (x <- problemParams) {
            assert(space.involvedSearchVariables(x).isEmpty)
        }
        for (x <- searchVars) {
            assert(space.involvedSearchVariables(x).isEmpty)
        }
        assertEq(space.involvedSearchVariables(u), Set(s, t))
        assertEq(space.involvedSearchVariables(w), Set(s))
        assertEq(space.involvedSearchVariables(x), Set(s))
        assertEq(space.involvedSearchVariables(y), Set(s, t))
        assertEq(space.involvedSearchVariables(z), Set())

        assertEq(space.involvedSearchVariables(c), Set(s, t))
        assertEq(space.involvedSearchVariables(d), Set(s))
        assertEq(space.involvedSearchVariables(e), Set(s, t))

        for (x <- problemParams) {
            assert(space.definingConstraint(x).isEmpty)
        }
        for (x <- searchVars) {
            assert(space.definingConstraint(x).isEmpty)
        }
        assertEq(space.definingConstraint(u), Some(c))
        assertEq(space.definingConstraint(w), Some(d))
        assertEq(space.definingConstraint(x), Some(d))
        assertEq(space.definingConstraint(y), Some(e))
        assertEq(space.definingConstraint(z), None)

        for (x <- problemParams) {
            assert(space.involvedConstraints(x).isEmpty)
        }
        for (x <- searchVars) {
            assert(space.involvedConstraints(x).isEmpty)
        }
        assertEq(space.involvedConstraints(u), Set(c))
        assertEq(space.involvedConstraints(w), Set(d))
        assertEq(space.involvedConstraints(x), Set(d))
        assertEq(space.involvedConstraints(y), Set(c, d, e))

    }

    @Test
    def testCycleDetection1 {
        val space = new Space(logger)
        val x = space.createVariable("x", UnboundedIntegerDomain)
        val c = new DummyConstraint(space.constraintIdFactory.nextId, List(x, x), List(x))
        assert(space.wouldIntroduceCycle(c))
        assertEx(space.post(c))
    }

    @Test
    def testCycleDetection2 {
        val space = new Space(logger)
        val x = space.createVariable("x", UnboundedIntegerDomain)
        val y = space.createVariable("y", UnboundedIntegerDomain)
        val z = space.createVariable("z", UnboundedIntegerDomain)
        val c = new DummyConstraint(space.constraintIdFactory.nextId, List(x, y), List(z))
        assert(! space.wouldIntroduceCycle(c))
        assert(space.findHypotheticalCycle(c).isEmpty)
        val d = new DummyConstraint(space.constraintIdFactory.nextId, List(x, z), List(y))
        assert(! space.wouldIntroduceCycle(d))
        assert(space.findHypotheticalCycle(d).isEmpty)
        space.post(c)
        assert(space.wouldIntroduceCycle(d))
        assertEq(space.findHypotheticalCycle(d), Some(List(d, c)))
        assertEx(space.post(d))
    }

    @Test
    def testManagementOfImplicitConstraints {
        val space = new Space(logger)
        val x = space.createVariable("x", UnboundedIntegerDomain)
        val y = space.createVariable("y", UnboundedIntegerDomain)
        val z = space.createVariable("z", UnboundedIntegerDomain)
        val c = new DummyConstraint(space.constraintIdFactory.nextId, List(x), List(y))
        val d = new DummyConstraint(space.constraintIdFactory.nextId, List(x), List(z))
        space.post(c).post(d)
        assertEq(space.numberOfImplicitConstraints, 0)
        assert(! space.isImplicit(c))
        assert(! space.isImplicit(d))
        space.markAsImplicit(d)
        assertEq(space.numberOfImplicitConstraints, 1)
        assert(! space.isImplicit(c))
        assert(space.isImplicit(d))
        space.setValue(x, Zero).setValue(y, Zero).initialize
        assertEq(space.numberOfInitializations, 1)
        val move = new ChangeValue(space.moveIdFactory.nextId, x, One)
        space.consult(move)
        assertEq(space.numberOfConsultations, 1)
        space.commit(move)
        assertEq(space.numberOfCommitments, 1)
    }

    // See Queens, SendMoreMoney, and SendMostMoney for tests of initialize, consult, and commit.

}
