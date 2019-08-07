package yuck.constraints.test

import org.junit._

import scala.collection._
import scala.collection.JavaConverters._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
  * @author Michael Marte
  *
  */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
class LinearConstraintPerformanceTest(relation: OrderingRelation, withUnitCoefficients: Boolean) extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val baseDomain = new IntegerRange(Zero, Nine)
    private val numberOfTerms = 10
    private val initialDomains =
      for (i <- 0 until numberOfTerms) yield baseDomain.randomSubdomain(randomGenerator)
    private val xs =
      for (i <- 0 until numberOfTerms) yield
          new IntegerVariable(space.nextVariableId, "x%d".format(i + 1), initialDomains(i))
    private val z = new IntegerVariable(space.nextVariableId, "z", baseDomain.randomSubdomain(randomGenerator))
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
    private val axs = xs.map(AX(if (withUnitCoefficients) One else baseDomain.randomValue(randomGenerator), _))
    private val constraint = LinearConstraint.postLinearConstraint(space, null, axs, relation, z, costs)
    private val moveSizeDistribution = DistributionFactory.createDistribution(1, for (n <- 10 to 1 by -1) yield n)
    private val neighbourhood = new RandomReassignmentGenerator(space, xs, randomGenerator, moveSizeDistribution, None, None)
    private val numberOfMoves = 1000
    private val numberOfIterations = numberOfMoves * 10000
    for (x <- xs) {
        space.setValue(x, x.domain.randomValue(randomGenerator))
    }
    space.setValue(z, z.domain.randomValue(randomGenerator))
    space.initialize
    private val moves = for (i <- 0 until numberOfMoves) yield {
        val move = neighbourhood.nextMove
        val bulkMove = new BulkMove(space.nextMoveId)
        bulkMove ++= move.effects
    }
    private val now = space.searchState
    private val afters = for (move <- moves) yield new MoveSimulator(now, move)

    @Test
    def testConsult: Unit = {
        var i = 0
        while (i < numberOfIterations) {
            val move = moves(i % numberOfMoves)
            val after = afters(i % numberOfMoves)
            constraint.consult(now, after, move)
            i += 1
        }
    }

}

/**
  * @author Michael Marte
  *
  */
final object LinearConstraintPerformanceTest {

    private def configurations =
        for (relation <- List(EqRelation, NeRelation, LtRelation, LeRelation);
             withUnitCoefficients <- List(true, false))
            yield Vector(relation, withUnitCoefficients)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
