package yuck.core

import scala.collection._

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import yuck.util.logging.LazyLogger

/**
 * This class is used for building and managing a constraint network,
 * it manages a search state, and it provides services for assessing
 * and performing moves.
 *
 * Based on the input and output variables of constraints, we distinguish the
 * following types of variables:
 *  - A ''search variable'' is an input variable that is not an output variable.
 *  - A ''problem parameter'' is an input variable with singleton domain.
 *  - A ''channel variable'' is an output variable.
 *  - A ''dangling variable'' is neither an input nor an output variable - it is unused.
 *
 * @author Michael Marte
 */
final class Space(logger: LazyLogger) {

    private val constraints = new mutable.ArrayBuffer[Constraint] // maintained by post
    private val inVariables = new mutable.HashSet[AnyVariable] // maintained by post
    private val outVariables = new mutable.HashSet[AnyVariable] // maintained by post

    private val impliedConstraints = new mutable.BitSet // indexed by constraint id
    @inline private def isImpliedConstraint(constraint: Constraint) =
        impliedConstraints.contains(constraint.id.rawId)

    private type InflowModel = mutable.HashMap[AnyVariable, mutable.HashSet[Constraint]]
    private val inflowModel = new InflowModel // maintained by post
    private def registerInflow(x: AnyVariable, constraint: Constraint) {
        inflowModel += x -> (inflowModel.get(x).getOrElse(new mutable.HashSet[Constraint]) += constraint)
    }

    private type OutflowModel = mutable.HashMap[AnyVariable, Constraint]
    private val outflowModel = new OutflowModel // maintained by post
    private def registerOutflow(x: AnyVariable, constraint: Constraint) {
        outflowModel += x -> constraint
    }

    private type ConstraintOrder = Array[Int]
    private var constraintOrder: ConstraintOrder = null // created by initialize
    private def sortConstraintsTopologically {
        val constraintNetwork = Graph[Constraint, DiEdge]()
        val noConstraints = new mutable.ArrayBuffer[Constraint]
        for (pred <- constraints) {
            for (x <- pred.outVariables) {
                for (succ <- inflowModel.get(x).getOrElse(noConstraints)) {
                    constraintNetwork += pred ~> succ
                }
            }
        }
        constraintOrder = new Array[Int](constraints.toIterator.map(_.id).max.rawId + 1)
        constraintNetwork.topologicalSort match {
            case Left(witness) =>
                assert(false, "Input graph has cycle %s".format(witness.findCycle.get))
            case Right(order) =>
                for ((constraint, i) <- order.toStream.zipWithIndex) {
                    constraintOrder.update(constraint.id.rawId, i)
                }
        }
    }
    private final object ConstraintOrdering extends Ordering[Constraint] {
        @inline override def compare(lhs: Constraint, rhs: Constraint) =
            constraintOrder(lhs.id.rawId) - constraintOrder(rhs.id.rawId)
    }
    // Iterating over the elements will not yield heap order -
    // the only way to obtain it is to use dequeue!
    private type ConstraintQueue = mutable.PriorityQueue[Constraint]
    private val constraintQueue = new ConstraintQueue()(ConstraintOrdering.reverse)

    private val assignment = new Assignment

    /** Provides the space-specific factory for variable ids. */
    val variableIdFactory = new IdFactory[AnyVariable]

    /** Provides the space-specific factory for constraint ids. */
    val constraintIdFactory = new IdFactory[Constraint]

    /** Provides the space-specific factory for move ids. */
    val moveIdFactory = new IdFactory[Move]

    /** Convenience method for creating variables. */
    def createVariable[Value <: AnyValue](name: String, domain: Domain[Value]): Variable[Value] = {
        val x = new Variable[Value](variableIdFactory.nextId, name, domain)
        if (domain.isSingleton) setValue(x, domain.singleValue)
        x
    }

    /** Assigns the given value to the given variable. */
    def setValue[Value <: AnyValue](x: Variable[Value], a: Value): Space = {
        assignment.setValue(x, a)
        this
    }

    /** Returns the current search state. */
    def searchState: SearchState = assignment

    /** Computes the set of problem parameters. */
    def problemParameters: Set[AnyVariable] = inVariables.filter(_.isParameter)

    /** Decides whether the given variable is a problem parameter. */
    def isProblemParameter(x: AnyVariable): Boolean = x.isParameter && inVariables.contains(x)

    /** Returns the set of channel variables. */
    def channelVariables: Set[AnyVariable] = outVariables

    /** Decides whether the given variable is a channel variable. */
    def isChannelVariable(x: AnyVariable): Boolean = outVariables.contains(x)

    /** Computes the set of search variables. */
    def searchVariables: Set[AnyVariable] = inVariables.filter(! _.isParameter) -- outVariables

    /** Decides whether the given variable is a search variable. */
    def isSearchVariable(x: AnyVariable): Boolean =
        ! x.isParameter && inVariables.contains(x) && ! outVariables.contains(x)

    /** Decides whether the given variable is a dangling variable. */
    def isDanglingVariable(x: AnyVariable): Boolean =
        ! isProblemParameter(x) && ! isSearchVariable(x) && ! isChannelVariable(x)

    /**
     * Finds the search variables involved in computing the value of given variable
     * and returns the empty set when the variable is a search variable.
     *
     * Copes with cycles in the constraint network.
     */
    def involvedSearchVariables(x: AnyVariable): Set[AnyVariable] = {
        val result = new mutable.HashSet[AnyVariable]
        addInvolvedSearchVariables(x, result, new mutable.HashSet[AnyVariable])
        result
    }
    private def addInvolvedSearchVariables(
        x: AnyVariable, result: mutable.Set[AnyVariable], visited: mutable.Set[AnyVariable])
    {
        if (! visited.contains(x)) {
            visited += x
            val maybeConstraint = outflowModel.get(x)
            if (maybeConstraint.isDefined) {
                val constraint = maybeConstraint.get
                val (xs, ys) = constraint.inVariables.toIterator.partition(isSearchVariable)
                result ++= xs
                ys.foreach(addInvolvedSearchVariables(_, result, visited))
            }
        }
    }

    /**
     * Computes the set of search variables the values of which affect the given
     * constraint directly or indirectly.
     */
    final def involvedSearchVariables(constraint: Constraint): Set[AnyVariable] =
        constraint
        .inVariables
        .map(x => if (isSearchVariable(x)) Set(x) else involvedSearchVariables(x).toSet)
        .foldLeft(Set[AnyVariable]())((a, b) => a union b)

    /** Returns the constraint that computes the value of the given variable. */
    def definingConstraint(x: AnyVariable): Option[Constraint] =
        outflowModel.get(x)

    /**
     * Finds the constraints involved in computing the value of the given variable.
     *
     * Copes with cycles in the constraint network.
     */
    def involvedConstraints(x: AnyVariable): Set[Constraint] = {
        val result = new mutable.HashSet[Constraint]
        addInvolvedConstraints(x, result, new mutable.HashSet[AnyVariable])
        result
    }
    private def addInvolvedConstraints(
        x: AnyVariable, result: mutable.Set[Constraint], visited: mutable.Set[AnyVariable])
    {
        if (! visited.contains(x)) {
            visited += x
            val maybeConstraint = outflowModel.get(x)
            if (maybeConstraint.isDefined) {
                val constraint = maybeConstraint.get
                result += constraint
                constraint.inVariables.foreach(addInvolvedConstraints(_, result, visited))
            }
        }
    }

    /** Decides whether adding the given constraint would add a cycle to the constraint network. */
    def wouldIntroduceCycle(constraint: Constraint): Boolean =
        isCyclic(constraint) || findHypotheticalCycle(constraint).isDefined

    private def isCyclic(constraint: Constraint): Boolean =
        constraint.outVariables.exists(constraint.inVariables.toIterator.contains(_))

    /** Looks for a cycle that the given constraint would add to the constraint network. */
    def findHypotheticalCycle(constraint: Constraint): Option[List[Constraint]] =
        findPath(constraint, constraint.inVariables.toSet, new mutable.HashSet[Constraint])
        .map(path => constraint :: path)

    private def findPath(
        constraint: Constraint, D: Set[AnyVariable], visited: mutable.Set[Constraint]): Option[List[Constraint]] =
    {
        if (visited.contains(constraint)) {
            None
        } else {
            visited += constraint
            constraint.outVariables.toStream.map(findPath(_, D, visited)).filter(_.isDefined).headOption.map(_.get)
        }
    }
    private def findPath(
        o: AnyVariable, D: Set[AnyVariable], visited: mutable.Set[Constraint]): Option[List[Constraint]] =
    {
        inflowModel.get(o) match {
            case Some(constraints) =>
                constraints
                .toStream
                .map(
                    constraint =>
                        if (constraint.outVariables.exists(x => D.contains(x))) Some(List(constraint))
                        else findPath(constraint, D, visited).map(path => constraint :: path))
                .filter(maybePath => maybePath.isDefined)
                .map(maybePath => maybePath.get)
                .headOption
            case None => None
        }
    }

    /**
     * Adds the given constraint to the constraint network.
     *
     * Throws when adding the constraint would create a cycle in the network.
     */
    def post(newcomer: Constraint): Space = {
        logger.loggg("Adding %s".format(newcomer))
        require(
            ! newcomer.outVariables.exists(outVariables.contains(_)),
            "%s shares out-variables with the following constraints:\n%s".format(
                newcomer,
                constraints.filter(_.outVariables.exists(newcomer.outVariables.toIterator.contains(_))).mkString("\n")))
        require(! isCyclic(newcomer), "%s is cyclic in itself".format(newcomer))
        val maybeCycle = findHypotheticalCycle(newcomer)
        require(maybeCycle.isEmpty, "%s introduces cycle %s".format(newcomer, maybeCycle.get))
        constraints += newcomer
        // We use data structures based on sets to avoid problems with duplicate in and out variables.
        for (x <- newcomer.inVariables) {
            inVariables += x
            if (! x.isParameter) {
                registerInflow(x, newcomer)
            }
        }
        for (x <- newcomer.outVariables) {
            outVariables += x
            registerOutflow(x, newcomer)
        }
        constraintOrder = null
        this
    }

    /** Returns the number of constraints that were posted. */
    def numberOfConstraints: Int = constraints.size

    /**
     * Marks the given constraint as implied;
     * implied constraints will not be initialized and they will never be consulted.
     */
    def markAsImplied(constraint: Constraint): Space = {
        logger.loggg("Marking %s as implied".format(constraint))
        impliedConstraints += constraint.id.rawId
        constraintOrder = null
        this
    }

    /** Returns the number of constraints that were posted and later marked as implied. */
    def numberOfImpliedConstraints: Int = impliedConstraints.size
 
    /**
     * Initializes the constraint network for local search.
     *
     * The caller has to assign values to all search variables before initializing!
     */
    def initialize: Space = {
        require(constraintQueue.isEmpty)
        sortConstraintsTopologically
        constraintQueue ++= constraints.toIterator.filterNot(isImpliedConstraint)
        while (! constraintQueue.isEmpty) {
            constraintQueue.dequeue.initialize(assignment).foreach(_.setValue(assignment))
        }
        this
    }

    /**
     * Initializes the constraint network from the given search state.
     *
     * The given search state has to provide value assignments for all search variables!
     */
    def initialize(searchState: SearchState): Space = {
        assignment.setValues(searchState)
        initialize
    }

    abstract private class MoveProcessor(val move: Move) {
        // Commits can be made cheaper by storing the affected constraints in the order of
        // processing while consulting.
        // This makes consulting more expensive, increases code complexity, and does not
        // pay off because commits are very rare at the normal operating temperatures of
        // simulated annealing.
        require(constraintOrder != null, "Call initialize after posting the last constraint")
        require(constraintQueue.isEmpty)
        private val diff = new BulkMove(move.id)
        private val diffs = new mutable.HashMap[Constraint, BulkMove]
        private def propagateEffect(effect: AnyEffect) {
            if (assignment.anyValue(effect.anyVariable) != effect.anyValue) {
                val maybeAffectedConstraints = inflowModel.get(effect.anyVariable)
                if (maybeAffectedConstraints.isDefined) {
                    for (constraint <- maybeAffectedConstraints.get if ! isImpliedConstraint(constraint)) {
                        val diff = diffs.getOrElseUpdate(constraint, new BulkMove(move.id))
                        if (diff.isEmpty) {
                            constraintQueue += constraint
                        }
                        diff += effect
                    }
                }
                diff += effect
            }
        }
        def processConstraint(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move): TraversableOnce[AnyEffect]
        def run: Move = {
            move.effects.foreach(propagateEffect)
            while (! constraintQueue.isEmpty) {
                val constraint = constraintQueue.dequeue
                val diff = diffs(constraint)
                val after = new MoveSimulator(assignment, diff)
                processConstraint(constraint, assignment, after, diff).foreach(propagateEffect)
            }
            diff
        }
    }

    /** Counts how often Constraint.consult was called. */
    var numberOfConsultations = 0

    private class EffectComputer(move: Move, checkConstraintPropagation: Boolean) extends MoveProcessor(move) {
        override def processConstraint(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move) =
        {
            numberOfConsultations += 1
            if (checkConstraintPropagation) {
                checkedConsult(constraint, before, after, move)
            } else {
                constraint.consult(before, after, move)
            }
        }
        private def checkedConsult(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move) =
        {
            // Constraint implementations re-use effect objects for efficiency reasons.
            // In particular, the final reset to the state before the move will change these effect objects!
            // Hence, to avoid havoc, we have to clone the effects before proceeding with our sanity checks.
            val effects = constraint.consult(before, after, move).map(_.clone).toIterable
            val stateAfterConsultation =
                new MoveSimulator(after, new BulkMove(move.id) ++= effects)
            val stateAfterInitialization =
                new MoveSimulator(after, new BulkMove(move.id) ++= constraint.initialize(after))
            var buggy = false
            for (x <- constraint.outVariables) {
                if (stateAfterConsultation.anyValue(x) != stateAfterInitialization.anyValue(x)) {
                    println(
                        "%s: consultation computed %s, initialization computed %s".format(
                            x,
                            stateAfterConsultation.anyValue(x),
                            stateAfterInitialization.anyValue(x)))
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
                // Should the output from the following statement be inconsistent with the error message, then
                // a likely cause is a buggy commit implementation that fails to maintain the constraint's state.
                println("constraint.consult(before, after, move) = %s".format(constraint.consult(before, after, move)))
                println("constraint.initialize(after) = %s".format(constraint.initialize(after)))
            }
            for (x <- constraint.outVariables) {
                assert(
                    stateAfterConsultation.anyValue(x) == stateAfterInitialization.anyValue(x),
                    "Consultation failed for output variable %s of %s".format(x, constraint))
            }
            constraint.initialize(before)
            constraint.consult(before, after, move)
        }
    }

    private var idOfMostRecentlyAssessedMove = moveIdFactory.nextId

    /**
     * Computes the search state that would result from applying the given move to
     * the current search state.
     *
     * The move must involve search variables only!
     * (For efficieny reasons, this requirement is not enforced.)
     */
    def consult(move: Move, checkConstraintPropagation: Boolean = false): SearchState = {
        idOfMostRecentlyAssessedMove = move.id
        new MoveSimulator(assignment, new EffectComputer(move, checkConstraintPropagation).run)
    }

    private class EffectPropagator(move: Move, checkConstraintPropagation: Boolean) extends MoveProcessor(move) {
        override def processConstraint(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move) =
        {
            numberOfCommitments += 1
            if (checkConstraintPropagation) {
                checkedCommit(constraint, before, after, move)
            } else {
                constraint.commit(before, after, move)
            }
        }
        private def checkedCommit(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move) =
        {
            // Constraint implementations re-use effect objects for efficiency reasons.
            // In particular, the final reset to the state before the move will change these effect objects!
            // Hence, to avoid havoc, we have to clone the effects before proceeding with our sanity checks.
            val effects = constraint.commit(before, after, move).map(_.clone).toIterable
            val stateAfterCommitting =
                new MoveSimulator(after, new BulkMove(move.id) ++= effects)
            val stateAfterInitialization =
                new MoveSimulator(after, new BulkMove(move.id) ++= constraint.initialize(after))
            var buggy = false
            for (x <- constraint.outVariables) {
                if (stateAfterCommitting.anyValue(x) != stateAfterInitialization.anyValue(x)) {
                    println(
                        "%s: committing computed %s, initialization computed %s".format(
                            x,
                            stateAfterCommitting.anyValue(x),
                            stateAfterInitialization.anyValue(x)))
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
                println("constraint.consult(before, after, move) = %s".format(constraint.consult(before, after, move)))
                println("constraint.commit(before, after, move) = %s".format(constraint.commit(before, after, move)))
                println("constraint.initialize(after) = %s".format(constraint.initialize(after)))
            }
            for (x <- constraint.outVariables) {
                assert(
                    stateAfterCommitting.anyValue(x) == stateAfterInitialization.anyValue(x),
                    "Committing failed for output variable %s of %s".format(x, constraint))
            }
            constraint.initialize(before)
            constraint.consult(before, after, move)
            constraint.commit(before, after, move)
        }
    }

    /** Counts how often Constraint.commit was called. */
    var numberOfCommitments = 0

    /**
     * Performs the given move.
     *
     * Throws when consult was not called before commit.
     */
    def commit(move: Move, checkConstraintPropagation: Boolean = false): Space = {
        require(move.id == idOfMostRecentlyAssessedMove)
        new EffectPropagator(move, checkConstraintPropagation).run.effects.foreach(_.setValue(assignment))
        this
    }

    /** Throws when the constraint network has a cycle. */
    def checkConsistency {
        sortConstraintsTopologically
    }

}
