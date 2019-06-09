package yuck.core.test

import org.junit._

import scala.collection._

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
         * with variables s, ..., z and constraints c, d, and e.
         */
        val space = new Space(logger, sigint)
        def domain(name: Char) = if (name == 'v') ZeroToZeroIntegerRange else CompleteIntegerRange
        val vars @ IndexedSeq(s, t, u, v, w, x, y, z): IndexedSeq[AnyVariable] =
            for (name <- 's' to 'z') yield space.createVariable(name.toString, domain(name))
        val c = new DummyConstraint(space.nextConstraintId, List(s, t), List(u))
        val d = new DummyConstraint(space.nextConstraintId, List(s, v), List(w, x))
        val e = new DummyConstraint(space.nextConstraintId, List(u, w, x), List(y))
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
        assertEq(space.directlyAffectedConstraints(v), Set(d))
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
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val c = new DummyConstraint(space.nextConstraintId, List(x, x), List(x))
        assert(space.wouldIntroduceCycle(c))
        assertEx(space.post(c), classOf[CyclicConstraintNetworkException])
    }

    @Test
    def testCycleDetection2 {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val z = space.createVariable("z", CompleteIntegerRange)
        val c = new DummyConstraint(space.nextConstraintId, List(x, y), List(z))
        assert(! space.wouldIntroduceCycle(c))
        assert(space.findHypotheticalCycle(c).isEmpty)
        val d = new DummyConstraint(space.nextConstraintId, List(x, z), List(y))
        assert(! space.wouldIntroduceCycle(d))
        assert(space.findHypotheticalCycle(d).isEmpty)
        space.post(c)
        assert(space.wouldIntroduceCycle(d))
        assertEq(space.findHypotheticalCycle(d), Some(List(d, c)))
        assertEx(space.post(d), classOf[CyclicConstraintNetworkException])
        val e = new DummyConstraint(space.nextConstraintId, List(x, y), List(x, z))
        assert(space.wouldIntroduceCycle(e))
        assertEq(space.findHypotheticalCycle(e), Some(List(e)))
        assertEx(space.post(e))
    }

    @Test
    def testManagementOfImplicitConstraints {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val z = space.createVariable("z", CompleteIntegerRange)
        val c = new DummyConstraint(space.nextConstraintId, List(x), List(y))
        val d = new DummyConstraint(space.nextConstraintId, List(x), List(z))
        space.post(c).post(d)
        assertEq(space.numberOfImplicitConstraints, 0)
        assert(! space.isImplicitConstraint(c))
        assert(! space.isImplicitConstraint(d))
        space.markAsImplicit(d)
        assertEq(space.numberOfImplicitConstraints, 1)
        assert(! space.isImplicitConstraint(c))
        assert(space.isImplicitConstraint(d))
        space.setValue(x, Zero).setValue(y, Zero).initialize
        assertEq(space.numberOfInitializations, 1)
        val move = new ChangeValue(space.nextMoveId, x, One)
        space.consult(move)
        assertEq(space.numberOfConsultations, 1)
        space.commit(move)
        assertEq(space.numberOfCommitments, 1)
    }

    // A spy constraint maintains the sum of its input variables and,
    // on each call to consult and commit, checks that Space calls these methods
    // according to their contracts.
    private final class Spy
        (id: Id[Constraint], xs: Set[IntegerVariable], sum: IntegerVariable)
        extends Constraint(id, null)
    {
        require(! xs.isEmpty)

        override def toString = "spy([%s], %s)".format(xs.mkString(", "), sum)
        override def inVariables = xs
        override def outVariables = List(sum)

        private val effects = Vector(new ReusableMoveEffectWithFixedVariable[IntegerValue](sum))

        var numberOfPropagations = 0
        var numberOfInitializations = 0
        var numberOfConsultations = 0
        var numberOfCommitments = 0

        override def propagate = {
            numberOfPropagations += 1
            val lhs0 = new Iterable[(IntegerValue, IntegerDomain)] {
                override def iterator = xs.toIterator.map(x => (One, x.domain))
            }
            val rhs0 = sum.domain
            val (lhs1, rhs1) = IntegerValueTraits.domainPruner.linEq(lhs0, rhs0)
            NoPropagationOccurred.pruneDomains(xs.toIterator.zip(lhs1.toIterator)).pruneDomain(sum, rhs1)
        }

        override def initialize(now: SearchState) = {
            numberOfInitializations += 1
            effects(0).a = IntegerValue.get(xs.toIterator.map(now.value(_).value).sum)
            effects
        }

        override def consult(before: SearchState, after: SearchState, move: Move) = {
            numberOfConsultations += 1
            checkContract(before, after, move)
            initialize(after)
        }

        override def commit(before: SearchState, after: SearchState, move: Move) = {
            numberOfCommitments += 1
            checkContract(before, after, move)
            initialize(after)
        }

        private def checkContract(before: SearchState, after: SearchState, move: Move) {
            for (x <- move.involvedVariables) {
                assert(xs.contains(IntegerValueTraits.safeDowncast(x)))
                assertNe(before.anyValue(x), after.anyValue(x))
                assertEq(after.anyValue(x), move.anyValue(x))
            }
        }

    }

    // To test constraint processing, we generate n random multi-layer networks of
    // spy constraints and, for each network, we exercise the machinery on m random moves.
    //
    // To generate a random network, we start out from a set X_0 of k variables and then,
    // for each layer 1 <= i <= l,
    //   1. we choose a random subset P from X_{i - 1} with |P| = k
    //   2. we generate a spy constraint over each non-empty subset Q of P resulting in a set Y
    //      of new output variables
    //   3. we set X_{i} = X_{i - 1} union Y.
    private def createRandomSpyNetwork(randomGenerator: RandomGenerator): (Space, Seq[IntegerVariable], Seq[Spy]) = {

        val k = 4 // number of variables per layer
        val l = 8 // number of layers
        val dx = new IntegerRange(One, IntegerValue.get(k * l))

        val space = new Space(logger, sigint)
        val spies = new mutable.ArrayBuffer[Spy]

        // build constraint network (see above for how we do it)
        val xs = new mutable.ArrayBuffer[IntegerVariable]
        for (i <- 1 to k) {
            xs += new IntegerVariable(space.nextVariableId, "x(0, %d)".format(i), dx)
        }
        for (i <- 1 to l) {
            val ps = new mutable.HashSet[IntegerVariable]
            while (ps.size < k) {
                ps += xs(randomGenerator.nextInt(xs.size))
            }
            var j = 1
            for (qs <- ps.subsets if ! qs.isEmpty) {
                val sum = new IntegerVariable(space.nextVariableId, "sum(%d, %d)".format(i, j), NonNegativeIntegerRange)
                xs += sum
                val spy = new Spy(space.nextConstraintId, qs, sum)
                space.post(spy)
                spies += spy
                j += 1
            }
        }

        (space, xs, spies)
    }

    @Test
    def testPropagation {

        val n = 8 // number of networks
        val randomGenerator = new JavaRandomGenerator

        // generate and test n networks
        for (i <- 1 to n) {

            val (space, xs, spies) = createRandomSpyNetwork(randomGenerator)

            // check forward propagation
            space.propagate
            for (spy <- spies) {
                assertGe(spy.numberOfPropagations, 2)
                assertEq(spy.propagate, NoPropagationOccurred)
                spy.numberOfPropagations = 0
            }

            // check criss-cross propagation departing from random variable until domain wipe-out
            try {
                var m = 0
                while (true) {
                    val x = xs(randomGenerator.nextInt(xs.size))
                    x.pruneDomain(x.domain.randomSubrange(randomGenerator))
                    space.propagate(x)
                    for (spy <- spies) {
                        assertEq(spy.propagate, NoPropagationOccurred)
                        spy.numberOfPropagations = 0
                    }
                    m += 1
                }
                assertGt(m, 1)
            }
            catch {
                case _: DomainWipeOutException =>
            }

            // check that neither initialize, nor consult, nor commit were called during propagation
            for (spy <- spies) {
                assertEq(spy.numberOfInitializations, 0)
                assertEq(spy.numberOfConsultations, 0)
                assertEq(spy.numberOfCommitments, 0)
            }

        }

    }

    @Test
    def testConsultAndCommit {

        val n = 8 // number of networks
        val m = 64 // number of moves per network
        val moveSizeDistribution = DistributionFactory.createDistribution(1, List(60, 30, 10))
        val randomGenerator = new JavaRandomGenerator

        // generate and test n networks
        for (i <- 1 to n) {

            val (space, _, spies) = createRandomSpyNetwork(randomGenerator)

            def checkResults(searchState: SearchState) {
                for (spy <- spies) {
                    val sum = spy.inVariables.toIterator.map(searchState.value(_).value).sum
                    assertEq(searchState.value(spy.outVariables(0)).value, sum)
                }
            }

            def checkSearchStateEquivalence(lhs: SearchState, rhs: SearchState) {
                assertEq(lhs.mappedVariables, rhs.mappedVariables)
                for (x <- lhs.mappedVariables) {
                    assertEq(lhs.anyValue(x), rhs.anyValue(x))
                }
            }

            // initialize variables
            new RandomInitializer(space, randomGenerator).run
            // initialize spies
            space.initialize
            // check that each spy was initialized exactly once
            for (spy <- spies) {
                assertEq(spy.numberOfInitializations, 1)
            }
            // check that results are correct
            checkResults(space.searchState)

            // generate and perform m moves
            val neighbourhood =
                new RandomReassignmentGenerator(
                    space, space.searchVariables.toIndexedSeq, randomGenerator, moveSizeDistribution, None, None)
            for (i <- 1 to m) {
                // generate move and consult space
                val move = neighbourhood.nextMove
                val beforeConsult = space.searchState.clone
                val afterConsult = space.consult(move).clone
                // check that each spy was consulted at most once
                for (spy <- spies) {
                    assertLe(spy.numberOfConsultations, 1)
                }
                // check that consult considered the move
                for (x <- move.involvedVariables) {
                    assertEq(afterConsult.anyValue(x), move.anyValue(x))
                }
                // check that results are correct
                checkResults(afterConsult)
                // check that consult did not change the search state
                checkSearchStateEquivalence(space.searchState, beforeConsult)
                // check that consult and commit computed equivalent search states
                val afterCommit = space.commit(move).searchState
                checkSearchStateEquivalence(afterConsult, afterCommit)
                // check that each spy was told at most once to commit
                for (spy <- spies) {
                    assertLe(spy.numberOfCommitments, 1)
                }
                // check that no propagation happened during consult and commit
                for (spy <- spies) {
                    assertEq(spy.numberOfPropagations, 0)
                }
                // prepare for next round
                for (spy <- spies) {
                    spy.numberOfCommitments = 0
                    spy.numberOfConsultations = 0
                }
            }

        }
    }

    // See Queens, SendMoreMoney, and SendMostMoney for more tests of propagate, initialize, consult, and commit.

}
