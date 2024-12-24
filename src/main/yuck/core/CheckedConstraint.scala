package yuck.core

/**
 * Decorator for checking the incremental cost update of constraints.
 *
 * @author Michael Marte
 */
final class CheckedConstraint(constraint: Constraint) extends Constraint(constraint.id) {

    override def toString = constraint.toString

    override val maybeGoal = constraint.maybeGoal

    override def inVariables = constraint.inVariables
    override def outVariables = constraint.outVariables

    override def propagate() = constraint.propagate()

    override def initialize(searchState: SearchState) = constraint.initialize(searchState)

    def consult(before: SearchState, after: SearchState, move: Move) = {
        // Constraint implementations re-use effect objects for efficiency reasons.
        // In particular, the final reset to the state before the move will change these effect objects!
        // Hence, to avoid havoc, we have to clone the effects before proceeding with our sanity checks.
        val effects = constraint.consult(before, after, move).map(_.clone)
        val stateAfterConsultation =
            new MoveSimulator(after, new BulkMove(move.id) ++= effects)
        val stateAfterInitialization =
            new MoveSimulator(after, new BulkMove(move.id) ++= constraint.initialize(after))
        var buggy = false
        for (x <- constraint.outVariables) {
            if (stateAfterConsultation.value(x) != stateAfterInitialization.value(x)) {
                println(
                    "%s: consultation computed %s, initialization computed %s".format(
                        x,
                        stateAfterConsultation.value(x),
                        stateAfterInitialization.value(x)))
                buggy = true
            }
        }
        if (buggy) {
            // replay and console output for debugging
            println("before = %s".format(before))
            println("after = %s".format(after))
            println("move = %s".format(move))
            println("constraint = %s".format(constraint))
            println("constraint.initialize(before) = %s".format(constraint.initialize(before).toList))
            // Should the output from the following statement be inconsistent with the error message, then
            // a likely cause is a buggy commit implementation that fails to maintain the constraint's state.
            println("constraint.consult(before, after, move) = %s".format(constraint
                .consult(before, after, move)
                .toList))
            println("constraint.initialize(after) = %s".format(constraint.initialize(after).toList))
        }
        for (x <- constraint.outVariables) {
            assert(
                stateAfterConsultation.value(x) == stateAfterInitialization.value(x),
                "Consultation failed for output variable %s of %s".format(x, constraint))
        }
        constraint.initialize(before)
        constraint.consult(before, after, move)
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        // Constraint implementations re-use effect objects for efficiency reasons.
        // In particular, the final reset to the state before the move will change these effect objects!
        // Hence, to avoid havoc, we have to clone the effects before proceeding with our sanity checks.
        val effects = constraint.commit(before, after, move).map(_.clone)
        val stateAfterCommitting =
            new MoveSimulator(after, new BulkMove(move.id) ++= effects)
        val stateAfterInitialization =
            new MoveSimulator(after, new BulkMove(move.id) ++= constraint.initialize(after))
        var buggy = false
        for (x <- constraint.outVariables) {
            if (stateAfterCommitting.value(x) != stateAfterInitialization.value(x)) {
                println(
                    "%s: committing computed %s, initialization computed %s".format(
                        x,
                        stateAfterCommitting.value(x),
                        stateAfterInitialization.value(x)))
                buggy = true
            }
        }
        if (buggy) {
            // replay and console output for debugging
            println("before = %s".format(before))
            println("after = %s".format(after))
            println("move = %s".format(move))
            println("constraint = %s".format(constraint))
            println("constraint.initialize(before) = %s".format(constraint.initialize(before)))
            println("constraint.consult(before, after, move) = %s".format(constraint.consult(before, after, move).toList))
            println("constraint.commit(before, after, move) = %s".format(constraint.commit(before, after, move).toList))
            println("constraint.initialize(after) = %s".format(constraint.initialize(after).toList))
        }
        for (x <- constraint.outVariables) {
            assert(
                stateAfterCommitting.value(x) == stateAfterInitialization.value(x),
                "Committing failed for output variable %s of %s".format(x, constraint))
        }
        constraint.initialize(before)
        constraint.consult(before, after, move)
        constraint.commit(before, after, move)
    }

}
