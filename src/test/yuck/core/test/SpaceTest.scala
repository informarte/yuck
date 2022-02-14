package yuck.core.test

import org.junit.*

import scala.collection.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class SpaceTest extends UnitTest {

    @Test
    def testBasicNetworkManagement: Unit = {

        /*
         * s, t -> c -> u    \
         *                    ->  e -> y
         * v    -> d -> w, x /
         *
         * with variables s, ..., z and constraints c, d, and e.
         */
        val space = new Space(logger, sigint)
        def domain(name: Char) =
            if (List('u', 'v').contains(name)) ZeroToZeroIntegerRange else CompleteIntegerRange
        val vars @ IndexedSeq(s, t, u, v, w, x, y, z) =
            for (name <- 's' to 'z') yield space.createVariable(name.toString, domain(name))
        val c = new DummyConstraint(space.nextConstraintId, List(s, t), List(u))
        val d = new DummyConstraint(space.nextConstraintId, List(s, v), List(w, x))
        val e = new DummyConstraint(space.nextConstraintId, List(u, w, x), List(y))
        val f = new DummyConstraint(space.nextConstraintId, List(s, t), List(x, y))
        space.post(c).post(d).post(e)
        assertEx(space.post(f))
        assertEq(space.numberOfConstraints, 3)
        assertEq(space.numberOfConstraints(_.isInstanceOf[DummyConstraint]), 3)
        assertEq(space.numberOfConstraints(_ => false), 0)
        space.checkConsistency

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

        for (x <- vars) {
            if (space.maybeDefiningConstraint(x).isEmpty) {
                assertEx(space.definingConstraint(x), classOf[NoSuchElementException])
            } else {
                assertEq(space.definingConstraint(x), space.maybeDefiningConstraint(x).get)
            }
        }
        for (x <- problemParams) {
            assert(space.maybeDefiningConstraint(x).isEmpty)
        }
        for (x <- searchVars) {
            assert(space.maybeDefiningConstraint(x).isEmpty)
        }
        assertEq(space.definingConstraint(u), c)
        assertEq(space.definingConstraint(w), d)
        assertEq(space.definingConstraint(x), d)
        assertEq(space.definingConstraint(y), e)
        assertEq(space.maybeDefiningConstraint(z), None)

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
    def testPostingAfterInitialization: Unit = {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val z = space.createVariable("z", CompleteIntegerRange)
        val c = new DummyConstraint(space.nextConstraintId, List(x), List(y))
        space.post(c)
        space.initialize()
        val d = new DummyConstraint(space.nextConstraintId, List(y), List(z))
        assertEx(space.post(d))
        assertEq(space.numberOfConstraints, 1)
        space.checkConsistency
    }

    @Test
    def testCycleDetection: Unit = {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val z = space.createVariable("z", CompleteIntegerRange)
        val c = new DummyConstraint(space.nextConstraintId, List(x, x), List(x))
        assert(space.wouldIntroduceCycle(c))
        assertEq(space.numberOfConstraints, 0)
        space.checkConsistency
        assertEx(space.post(c), classOf[CyclicConstraintNetworkException])
        assertEq(space.numberOfConstraints, 0)
        space.checkConsistency
        val d = new DummyConstraint(space.nextConstraintId, List(x, y), List(z))
        assert(! space.wouldIntroduceCycle(d))
        assertEq(space.numberOfConstraints, 0)
        space.checkConsistency
        val e = new DummyConstraint(space.nextConstraintId, List(x, z), List(y))
        assert(! space.wouldIntroduceCycle(e))
        assertEq(space.numberOfConstraints, 0)
        space.checkConsistency
        space.post(d)
        assertEq(space.numberOfConstraints, 1)
        space.checkConsistency
        assert(space.wouldIntroduceCycle(e))
        assertEq(space.numberOfConstraints, 1)
        space.checkConsistency
        assertEx(space.post(e), classOf[CyclicConstraintNetworkException])
        assertEq(space.numberOfConstraints, 1)
        space.checkConsistency
        assert(! space.wouldIntroduceCycle(d))
        assertEq(space.numberOfConstraints, 1)
        space.checkConsistency
    }

    @Test
    def testCycleDetectionAfterInitialization: Unit = {
        val space = new Space(logger, sigint)
        val x = space.createVariable("x", CompleteIntegerRange)
        val y = space.createVariable("y", CompleteIntegerRange)
        val c = new DummyConstraint(space.nextConstraintId, List(x), List(y))
        space.post(c)
        space.initialize()
        val d = new DummyConstraint(space.nextConstraintId, List(y), List(x))
        assertEx(space.wouldIntroduceCycle(d))
        assertEq(space.numberOfConstraints, 1)
        space.checkConsistency
    }

    @Test
    def testNetworkPruning: Unit = {
        val space = new Space(logger, sigint)
        val vars @ IndexedSeq(s, t, u, v, w, x, y, z) =
            for (name <- 's' to 'z') yield space.createVariable(name.toString, CompleteIntegerRange)
        val c = new DummyConstraint(space.nextConstraintId, List(s), List(t, y))
        val d = new DummyConstraint(space.nextConstraintId, List(t), List(u))
        val e = new DummyConstraint(space.nextConstraintId, List(t), List(v))
        val f = new DummyConstraint(space.nextConstraintId, List(t), List(w))
        val g = new DummyConstraint(space.nextConstraintId, List(w), List(x))
        val h = new DummyConstraint(space.nextConstraintId, List(z), Nil)
        space
            .registerOutputVariable(u)
            .registerObjectiveVariable(v)
            .post(c).post(d).post(e).post(f).post(g).post(h)
            .markAsImplicit(h)
            .removeUselessConstraints()
        assertEq(space.numberOfConstraints, 4)
        assertEq(space.numberOfConstraints(_.id == c.id), 1)
        assertEq(space.numberOfConstraints(_.id == d.id), 1)
        assertEq(space.numberOfConstraints(_.id == e.id), 1)
        assertEq(space.numberOfConstraints(_.id == f.id), 0)
        assertEq(space.numberOfConstraints(_.id == g.id), 0)
        assertEq(space.numberOfConstraints(_.id == h.id), 1)
    }

    // A spy constraint maintains the sum of its input variables and,
    // on each call to consult and commit, checks that Space calls these methods
    // according to their contracts.
    private final class Spy
        (id: Id[Constraint], xs: Set[IntegerVariable], sum: IntegerVariable)
        extends Constraint(id)
    {
        require(! xs.isEmpty)

        override def toString = "spy([%s], %s)".format(xs.mkString(", "), sum)
        override def inVariables: Iterable[IntegerVariable] = xs
        override def outVariables: Seq[IntegerVariable] = List(sum)

        private val effects = Vector(sum.reuseableEffect)

        var numberOfPropagations = 0
        var numberOfInitializations = 0
        var numberOfConsultations = 0
        var numberOfCommitments = 0

        override def propagate = {
            numberOfPropagations += 1
            val lhs0 = new Iterable[(IntegerValue, IntegerDomain)] {
                override def iterator = xs.iterator.map(x => (One, x.domain))
            }
            val rhs0 = sum.domain
            val (lhs1, rhs1) = IntegerValueTraits.domainPruner.linEqRule(lhs0, rhs0)
            NoPropagationOccurred.pruneDomains(xs.iterator.zip(lhs1.iterator)).pruneDomain(sum, rhs1)
        }

        override def initialize(now: SearchState) = {
            numberOfInitializations += 1
            effects(0).a = IntegerValue(xs.iterator.map(now.value(_).value).sum)
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

        private def checkContract(before: SearchState, after: SearchState, move: Move): Unit = {
            for (x <- move.involvedVariables) {
                assert(xs.contains(IntegerValueTraits.safeDowncast(x)))
                assertNe(before.value(x), after.value(x))
                assertEq(after.value(x), move.value(x))
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
        val dx = IntegerRange(One, IntegerValue(k * l))

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
            for (qs <- ps.subsets() if ! qs.isEmpty) {
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
    def testPropagation: Unit = {

        val n = 8 // number of networks
        val randomGenerator = new JavaRandomGenerator

        // generate and test n networks
        for (i <- 1 to n) {

            val (space, xs, spies) = createRandomSpyNetwork(randomGenerator)

            // check forward propagation
            space.propagate()
            for (spy <- spies) {
                assertGe(spy.numberOfPropagations, 2)
                assertEq(spy.propagate, NoPropagationOccurred)
            }
            assertEq(space.numberOfPropagations, spies.iterator.map(_.numberOfPropagations - 1).sum)
            for (spy <- spies) {
                spy.numberOfPropagations = 0
            }
            space.numberOfPropagations = 0

            // check criss-cross propagation departing from random variable until domain wipe-out
            try {
                var m = 0
                while (true) {
                    val x = xs(randomGenerator.nextInt(xs.size))
                    x.pruneDomain(x.domain.randomSubrange(randomGenerator))
                    space.propagate(List(x))
                    for (spy <- spies) {
                        assertEq(spy.propagate, NoPropagationOccurred)
                    }
                    assertEq(space.numberOfPropagations, spies.iterator.map(_.numberOfPropagations - 1).sum)
                    for (spy <- spies) {
                        spy.numberOfPropagations = 0
                    }
                    space.numberOfPropagations = 0
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
    def testConsultAndCommit: Unit = {

        val n = 8 // number of networks
        val m = 64 // number of moves per network
        val moveSizeDistribution = Distribution(1, List(60, 30, 10))
        val randomGenerator = new JavaRandomGenerator

        // generate and test n networks
        for (i <- 1 to n) {

            val (space, _, spies) = createRandomSpyNetwork(randomGenerator)

            def checkResults(searchState: SearchState): Unit = {
                for (spy <- spies) {
                    val sum = spy.inVariables.iterator.map(searchState.value(_).value).sum
                    assertEq(searchState.value(spy.outVariables(0)).value, sum)
                }
            }

            def checkSearchStateEquivalence(lhs: SearchState, rhs: SearchState): Unit = {
                assertEq(lhs.mappedVariables, rhs.mappedVariables)
                for (x <- lhs.mappedVariables) {
                    assertEq(lhs.value(x), rhs.value(x))
                }
            }

            // initialize variables
            new RandomInitializer(space, randomGenerator).run()
            // initialize spies
            space.initialize()
            // check that each spy was initialized exactly once
            for (spy <- spies) {
                assertEq(spy.numberOfInitializations, 1)
            }
            assertEq(space.numberOfInitializations, spies.size)
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
                assertEq(space.numberOfConsultations, spies.iterator.map(_.numberOfConsultations).sum)
                // check that consult considered the move
                for (x <- move.involvedVariables) {
                    assertEq(afterConsult.value(x), move.value(x))
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
                assertEq(space.numberOfCommitments, spies.iterator.map(_.numberOfCommitments).sum)
                // check that no propagation happened during consult and commit
                for (spy <- spies) {
                    assertEq(spy.numberOfPropagations, 0)
                }
                assertEq(space.numberOfPropagations, 0)
                // prepare for next round
                for (spy <- spies) {
                    spy.numberOfConsultations = 0
                    spy.numberOfCommitments = 0
                }
                space.numberOfConsultations = 0
                space.numberOfCommitments = 0
            }

        }

    }

    @Test
    def testHandlingOfImplicitConstraints: Unit = {

        val space = new Space(logger, sigint)
        val IndexedSeq(s, t, u, v, w, x, y, z) =
            for (name <- 's' to 'z')
                yield new IntegerVariable(space.nextVariableId, name.toString, CompleteIntegerRange)
        val c = new Spy(space.nextConstraintId, Set(s), t)
        val d = new Spy(space.nextConstraintId, Set(u, v, w), x)
        val e = new Spy(space.nextConstraintId, Set(u, v), y)
        val f = new Spy(space.nextConstraintId, Set(z), u)
        val g = new Spy(space.nextConstraintId, Set(x), z)

        // check book keeping and enforcement of invariants
        assertEx(space.markAsImplicit(c)) // because c was not yet posted
        space.post(c)
        assert(! space.isImplicitConstraint(c))
        space.post(d)
        space.post(e)
        assertEq(space.numberOfImplicitConstraints, 0)
        space.markAsImplicit(c)
        space.markAsImplicit(d)
        assertEx(space.markAsImplicit(e)) // because u and v are already implicitly constrained by d
        assertEq(space.numberOfImplicitConstraints, 2)
        assert(space.isImplicitConstraint(c))
        assert(space.isImplicitConstraint(d))
        assert(! space.isImplicitConstraint(e))
        assertEq(space.implicitlyConstrainedSearchVariables, Set(s, u, v, w))
        for (x <- space.searchVariables) {
            assertEq(
                space.implicitlyConstrainedSearchVariables.contains(x),
                space.isImplicitlyConstrainedSearchVariable(x))
        }
        assertEx(space.post(f)) // because u is already implicitly constrained by d
        space.post(g)
        assertEx(space.markAsImplicit(g)) // because x is an out-variable of d

        // check that implicit constraints are propagated and that domains of implicitly constrained variables
        // get restored after propagation
        u.pruneDomain(NonNegativeIntegerRange)
        v.pruneDomain(NonNegativeIntegerRange)
        w.pruneDomain(NonNegativeIntegerRange)
        x.pruneDomain(ZeroToOneIntegerRange)
        space.propagate()
        assertGt(d.numberOfPropagations, 0)
        assertEq(u.domain, NonNegativeIntegerRange) // domain of u was restored
        assertEq(v.domain, NonNegativeIntegerRange) // domain of v was restored
        assertEq(w.domain, NonNegativeIntegerRange) // domain of w was restored
        assertEq(y.domain, IntegerRange(Zero, Two)) // domain of y was pruned via domains of u and v

        // check that implicit constraints are never initialized and consulted
        space.setValue(u, Zero).setValue(v, Zero).setValue(w, Zero).setValue(x, Zero).initialize()
        assertEq(c.numberOfInitializations, 0)
        assertEq(d.numberOfInitializations, 0)
        val move = new ChangeValue(space.nextMoveId, u, One)
        space.consult(move)
        assertEq(c.numberOfConsultations, 0)
        assertEq(d.numberOfConsultations, 0)
        space.commit(move)
        assertEq(c.numberOfCommitments, 0)
        assertEq(d.numberOfCommitments, 0)

    }

    // See Queens, SendMoreMoney, and SendMostMoney for more tests of propagate, initialize, consult, and commit.

}
