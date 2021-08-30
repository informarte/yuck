package yuck.constraints.test

import org.junit._

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.{Table, TableNeighbourhood}
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class TableNeighbourhoodTest extends UnitTest {

    private def createTable(m: Int)(elems: Int*) =
        elems.toIndexedSeq.map(IntegerValue.apply).grouped(m).toIndexedSeq

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    @Test
    def testMoveGeneration: Unit = {
        val xs = Vector("s", "t").map(new IntegerVariable(space.nextVariableId, _, IntegerRange(Zero, Nine)))
        val rows = createTable(2)(0, 1, 0, 7, 1, 1, 1, 5, 3, 3, 5, 1, 5, 9, 8, 2, 8, 3, 9, 9)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val constraint = new Table(space.nextConstraintId, null, xs, rows, costs)
        space.post(constraint)
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[TableNeighbourhood[_]])
        assert(rows.contains(xs.map(now.value(_))))
        assertEq(now.value(costs), True)
        space.initialize()
        val sampleSize = 1000
        for (i <- 1 to sampleSize) {
            val move = neighbourhood.nextMove
            val after = space.consult(move)
            assert(xs.exists(x => now.value(x) != after.value(x)))
            assert(rows.contains(xs.map(now.value(_))))
            if (randomGenerator.nextDecision()) {
                space.commit(move)
                neighbourhood.commit(move)
            }
        }
    }

}
