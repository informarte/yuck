package yuck.constraints.test

import org.junit.*

import yuck.constraints.*
import yuck.constraints.OrderingRelation.*
import yuck.core.*
import yuck.test.util.UnitTest

@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
class LinearConstraintBenchmark(relation: OrderingRelation, withUnitCoefficients: Boolean) extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val numberOfTerms = 10
    private val numberOfMoves = 1000
    private val numberOfIterations = numberOfMoves * 10000
    private val initialDomains =
        for i <- 0 until numberOfTerms yield baseDomain.randomSubdomain(randomGenerator)
    private val xs =
        for i <- 0 until numberOfTerms yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i + 1), initialDomains(i))
    private val y = IntegerValueTraits.createChannel(space)
    private val z = new IntegerVariable(space.nextVariableId(), "z", baseDomain.randomSubdomain(randomGenerator))
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)
    private val axs = xs.map(AX(if withUnitCoefficients then One else baseDomain.randomValue(randomGenerator), _))
    private val constraint = new LinearConstraint(space.nextConstraintId(), null, axs, y, relation, z, costs)
    space.post(constraint)
    private val moveSizeDistribution = Distribution(1, for n <- numberOfTerms to 1 by -1 yield n)
    private val neighbourhood = new RandomReassignmentGenerator(space, xs.filter(space.isSearchVariable), randomGenerator, moveSizeDistribution)
    for x <- xs do {
        space.setValue(x, x.domain.randomValue(randomGenerator))
    }
    space.setValue(z, z.domain.randomValue(randomGenerator))
    space.initialize()
    private val moves = for i <- 0 until numberOfMoves yield {
        val move = neighbourhood.nextMove()
        val bulkMove = new BulkMove(space.nextMoveId())
        bulkMove ++= move.effects
    }
    private val now = space.searchState
    private val afters = for move <- moves yield new MoveSimulator(now, move)

    @Test
    def testConsult(): Unit = {
        var i = 0
        while i < numberOfIterations do {
            val move = moves(i % numberOfMoves)
            val after = afters(i % numberOfMoves)
            constraint.consult(now, after, move)
            i += 1
        }
    }

}

object LinearConstraintBenchmark {

    private def configurations =
        for relation <- List(EqRelation, NeRelation, LtRelation, LeRelation)
            withUnitCoefficients <- List(true, false)
        yield
            Array(relation, withUnitCoefficients)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.toArray

}
