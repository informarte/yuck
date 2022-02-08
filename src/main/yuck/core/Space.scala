package yuck.core

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.traverse.{BreadthFirstIterator, TopologicalOrderIterator}

import scala.collection.*
import scala.jdk.CollectionConverters.*

import yuck.util.arm.Sigint
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
final class Space(
    logger: LazyLogger,
    sigint: Sigint,
    checkIncrementalCostUpdate: Boolean = false,
    checkAssignmentsToNonChannelVariables: Boolean = false)
{

    private val constraints = new mutable.HashSet[Constraint] // maintained by post and removeUselessConstraints
    private val implicitConstraints = new mutable.HashSet[Constraint] // maintained by markAsImplicit
    private val inVariables = new mutable.HashSet[AnyVariable] // maintained by post
    private val inVariablesOfImplicitConstraints = new mutable.HashSet[AnyVariable] // maintained by markAsImplicit
    private val outVariables = new mutable.HashSet[AnyVariable] // maintained by post

    // The inflow model allows to find out which constraints are affected by changing
    // the value of a given variable.
    private type InflowModel = mutable.AnyRefMap[AnyVariable, mutable.HashSet[Constraint]]
    private val inflowModel = new InflowModel // maintained by post
    private def registerInflow(x: AnyVariable, constraint: Constraint): Unit = {
        inVariables += x
        inflowModel += x -> (inflowModel.getOrElse(x, new mutable.HashSet[Constraint]) += constraint)
    }
    private def deregisterInflow(x: AnyVariable, constraint: Constraint): Unit = {
        if ((inflowModel(x) -= constraint).isEmpty) {
            inflowModel -= x
            inVariables -= x
        }
    }

    /** Returns the set of constraints directly affected by changing the value of the given variable. */
    inline def directlyAffectedConstraints(x: AnyVariable): Set[Constraint] =
        inflowModel.getOrElse(x, Set.empty)

    // The outflow model allows to find out which constraint, if any, computes the value of a
    // given variable.
    private type OutflowModel = mutable.AnyRefMap[AnyVariable, Constraint]
    private val outflowModel = new OutflowModel // maintained by post
    private def registerOutflow(x: AnyVariable, constraint: Constraint): Unit = {
        outVariables += x
        outflowModel += x -> constraint
    }
    private def deregisterOutflow(x: AnyVariable): Unit = {
        outflowModel -= x
        outVariables -= x
    }

    /** Returns the constraint that computes the value of the given variable, if any. */
    inline def maybeDefiningConstraint(x: AnyVariable): Option[Constraint] =
        outflowModel.get(x)

    /** Returns the constraint that computes the value of the given variable. */
    inline def definingConstraint(x: AnyVariable): Constraint =
        outflowModel(x)

    // The flow model is a DAG which describes the flow of value changes through the
    // network of variables spanned by the constraints.
    private type FlowModel = DefaultDirectedGraph[Constraint, DefaultEdge]
    private var flowModel = new FlowModel(classOf[DefaultEdge]) // maintained by post and discarded by initialize
    private def addToFlowModel(constraint: Constraint): Unit = {
        if (isCyclic(constraint)) {
            throw new CyclicConstraintNetworkException(constraint)
        }
        flowModel.addVertex(constraint)
        for (x <- constraint.inVariables) {
            val maybePred = maybeDefiningConstraint(x)
            if (maybePred.isDefined) {
                flowModel.addEdge(maybePred.get, constraint)
            }
        }
        for (y <- constraint.outVariables) {
            for (succ <- directlyAffectedConstraints(y)) {
                flowModel.addEdge(constraint, succ)
            }
        }
        // BFS seems to be faster than DFS
        val i = new BreadthFirstIterator[Constraint, DefaultEdge](flowModel, constraint) {
            override def encounterVertexAgain(vertex: Constraint, edge: DefaultEdge): Unit = {
                if (vertex == constraint) {
                    flowModel.removeVertex(constraint)
                    throw new CyclicConstraintNetworkException(constraint)
                } else {
                    super.encounterVertexAgain(vertex, edge)
                }
            }
        }
        while (i.hasNext) {
            i.next()
        }
    }
    private def removeFromFlowModel(constraint: Constraint): Unit = {
        flowModel.removeVertex(constraint)
    }

    private type ConstraintOrder = Array[Int]
    private var constraintOrder: ConstraintOrder = null // created by initialize
    private def sortConstraintsTopologically(): Unit = {
        constraintOrder = new ConstraintOrder(constraints.iterator.map(_.id).max.rawId + 1)
        // The topological ordering exists because it was possible to build the flow model.
        for ((constraint, i) <- new TopologicalOrderIterator[Constraint, DefaultEdge](flowModel).asScala.zipWithIndex) {
            constraintOrder.update(constraint.id.rawId, i)
        }
    }
    private object ConstraintOrdering extends Ordering[Constraint] {
        override def compare(lhs: Constraint, rhs: Constraint) =
            constraintOrder(lhs.id.rawId) - constraintOrder(rhs.id.rawId)
    }

    private val assignment = new ArrayBackedAssignment

    private val variableIdFactory = new IdFactory[AnyVariable]

    /** Provides a unique variable id. */
    def nextVariableId() = variableIdFactory.nextId()

    private val constraintIdFactory = new IdFactory[Constraint]

    /** Provides a unique constraint id. */
    def nextConstraintId() = constraintIdFactory.nextId()

    private val moveIdFactory = new IdFactory[Move]

    /** Provides a unique move id. */
    def nextMoveId() = moveIdFactory.nextId()

    /** Convenience method for creating variables. */
    def createVariable
        [V <: AnyValue]
        (name: String, domain: Domain[V])
        (implicit valueTraits: ValueTraits[V]):
        Variable[V] =
    {
        valueTraits.createVariable(this, name, domain)
    }

    private val objectiveVariables = new mutable.HashSet[AnyVariable]

    /**
     * Registers the given variable as objective variable.
     *
     * There is no need to register objective variables but registering them will
     * speed up consultation.
     *
     * Important: When you decide to register objective variables, you have to
     * register all of them, otherwise the result of consultation will not provide the
     * effects on the variables that were not registered.
     */
    def registerObjectiveVariable(x: AnyVariable): Space = {
        objectiveVariables += x
        this
    }

    /** Returns true iff the given variable is an objective variable. */
    inline def isObjectiveVariable(x: AnyVariable): Boolean =
        objectiveVariables.isEmpty || objectiveVariables.contains(x)

    private val outputVariables = new mutable.HashSet[AnyVariable]

    /** Registers the given variable as output variable. */
    def registerOutputVariable(x: AnyVariable): Space = {
        outputVariables += x
        this
    }

    /** Returns true iff the given variable is an output variable. */
    inline def isOutputVariable(x: AnyVariable): Boolean =
        outputVariables.isEmpty || outputVariables.contains(x)

    /** Assigns the given value to the given variable. */
    def setValue[V <: AnyValue](x: Variable[V], a: V): Space = {
        if (checkAssignmentsToNonChannelVariables && (isProblemParameter(x) || isSearchVariable(x))) {
            require(
                x.domain.contains(a),
                "Domain %s of variable %s does not contain value %s".format(x.domain, x, a))
        }
        assignment.setValue(x, a)
        this
    }

    /** Returns the current search state. */
    def searchState: SearchState = assignment

    /** Computes the set of problem parameters. */
    def problemParameters: Set[AnyVariable] = inVariables.diff(outVariables).filter(_.domain.isSingleton)

    /** Decides whether the given variable is a problem parameter. */
    def isProblemParameter(x: AnyVariable): Boolean =
        x.domain.isSingleton && inVariables.contains(x) && ! outVariables.contains(x)

    /** Returns the set of channel variables. */
    def channelVariables: Set[AnyVariable] = outVariables

    /** Decides whether the given variable is a channel variable. */
    def isChannelVariable(x: AnyVariable): Boolean = outVariables.contains(x)

    /** Computes the set of search variables. */
    def searchVariables: Set[AnyVariable] =
        inVariables.filter(x => ! x.domain.isSingleton && ! outVariables.contains(x))

    /** Decides whether the given variable is a search variable. */
    def isSearchVariable(x: AnyVariable): Boolean =
        ! x.domain.isSingleton && inVariables.contains(x) && ! outVariables.contains(x)

    /** Computes the set of search variables involved in implicit constraints. */
    def implicitlyConstrainedSearchVariables: Set[AnyVariable] =
        inVariablesOfImplicitConstraints.filter(! _.domain.isSingleton)

    /** Decides whether the given variable is an implicitly constrained search variable. */
    def isImplicitlyConstrainedSearchVariable(x: AnyVariable): Boolean =
        ! x.domain.isSingleton && inVariablesOfImplicitConstraints.contains(x)

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
        x: AnyVariable, result: mutable.Set[AnyVariable], visited: mutable.Set[AnyVariable]): Unit =
    {
        if (! visited.contains(x)) {
            visited += x
            val maybeConstraint = maybeDefiningConstraint(x)
            if (maybeConstraint.isDefined) {
                addInvolvedSearchVariables(maybeConstraint.get, result, visited)
            }
        }
    }

    /**
     * Computes the set of search variables the values of which affect the given
     * constraint directly or indirectly.
     *
     * Copes with cycles in the constraint network.
     */
    def involvedSearchVariables(constraint: Constraint): Set[AnyVariable] = {
        val result = new mutable.HashSet[AnyVariable]
        val visited = new mutable.HashSet[AnyVariable]
        addInvolvedSearchVariables(constraint, result, visited)
        result
    }
    private def addInvolvedSearchVariables(
        constraint: Constraint, result: mutable.Set[AnyVariable], visited: mutable.Set[AnyVariable]): Unit =
    {
        for (x <- constraint.inVariables) {
            if (isSearchVariable(x)) {
                result += x
            } else {
                addInvolvedSearchVariables(x, result, visited)
            }
        }
    }

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
        x: AnyVariable, result: mutable.Set[Constraint], visited: mutable.Set[AnyVariable]): Unit =
    {
        if (! visited.contains(x)) {
            visited += x
            val maybeConstraint = maybeDefiningConstraint(x)
            if (maybeConstraint.isDefined) {
                val constraint = maybeConstraint.get
                result += constraint
                constraint.inVariables.foreach(addInvolvedConstraints(_, result, visited))
            }
        }
    }

    /**
     * Decides whether adding the given constraint would add a cycle to the constraint network.
     *
     * Notice that this method is quite expensive because it inserts and removes the constraint.
     * Hence, for cycle avoidance, just try to post the constraint; when an exception occurs,
     * you can try another approach to modeling your problem, otherwise everything is fine.
     */
    def wouldIntroduceCycle(constraint: Constraint): Boolean = {
        require(constraintOrder.eq(null), "Space has already been initialized")
        if (isCyclic(constraint)) true
        else if (constraints.isEmpty) false
        else if (constraints.contains(constraint)) false
        else try {
            addToFlowModel(constraint)
            removeFromFlowModel(constraint)
            false
        }
        catch {
            case _: CyclicConstraintNetworkException => true
        }
    }

    private def isCyclic(constraint: Constraint): Boolean =
        constraint.outVariables.exists(constraint.inVariables.iterator.contains)

    /**
     * Adds the given constraint to the constraint network.
     *
     * Throws a CyclicConstraintNetworkException when adding the constraint would create a cycle in the network.
     */
    def post(constraint: Constraint): Space = {
        logger.loggg("Adding %s".format(constraint))
        require(constraintOrder.eq(null), "Space has already been initialized")
        require(
            ! constraint.outVariables.exists(outVariables.contains),
            "%s shares out-variables with the following constraints:\n%s".format(
                constraint,
                constraints.filter(_.outVariables.exists(constraint.outVariables.iterator.contains)).mkString("\n")))
        require(
            ! constraint.outVariables.exists(inVariablesOfImplicitConstraints.contains),
            "%s has out-variables that are in-variables to the following implicit constraints:\n%s".format(
                constraint,
                implicitConstraints.filter(_.inVariables.exists(constraint.outVariables.iterator.contains)).mkString("\n")))
        addToFlowModel(constraint)
        constraints += constraint
        for (x <- constraint.inVariables) {
            registerInflow(x, constraint)
        }
        for (x <- constraint.outVariables) {
            registerOutflow(x, constraint)
        }
        constraintOrder = null
        this
    }

    /** Returns the number of constraints that were posted. */
    def numberOfConstraints: Int = constraints.size

    /** Returns the number of constraints that were posted and satisfy the given predicate. */
    def numberOfConstraints(p: Constraint => Boolean): Int =
        constraints.count(p)

    /**
     * Registers the given constraint as implicit.
     *
     * Implicit constraints will not be initialized and they will never be consulted.
     *
     * Throws when the constraint has not yet been posted or cannot be registered as implicit.
     */
    def registerImplicitConstraint(constraint: Constraint): Space = {
        logger.loggg("Registering %s as implicit".format(constraint))
        require(
            constraints.contains(constraint),
            "%s cannot be registered as implicit because it has not yet been posted".format(constraint))
        require(
            ! constraint.inVariables.exists(isChannelVariable),
            "%s cannot be registered as implicit because the following in-variables are channels: %s".format(
                constraint,
                constraint.inVariables.filter(isChannelVariable).mkString(", ")))
        require(
            ! constraint.inVariables.exists(isImplicitlyConstrainedSearchVariable),
            "%s cannot be registered as implicit because the following in-variables are already implicitly constrained: %s".format(
                constraint,
                constraint.inVariables.filter(isImplicitlyConstrainedSearchVariable).mkString(", ")))
        implicitConstraints.add(constraint)
        inVariablesOfImplicitConstraints ++= constraint.inVariables
        constraintOrder = null
        this
    }

    /** Returns true iff the given constraint was registered as implicit. */
    inline def isImplicitConstraint(constraint: Constraint): Boolean =
        implicitConstraints.contains(constraint)

    /** Returns the number of constraints that were posted and later registered as implicit. */
    def numberOfImplicitConstraints: Int = implicitConstraints.size

    /**
     * This method finds and removes useless constraints.
     *
     * A constraint is considered useful if one of its output variables is an objective or output variable
     * or input to another constraint.
     */
    def removeUselessConstraints(): Space = {
        val uselessConstraints = new mutable.HashSet[Constraint]
        while {
            uselessConstraints.clear()
            for (constraint <- constraints) {
                if (! isImplicitConstraint(constraint) &&
                    constraint.outVariables.forall(
                        x => ! isOutputVariable(x) && ! isObjectiveVariable(x) && directlyAffectedConstraints(x).isEmpty))
                {
                    uselessConstraints += constraint
                }
            }
            for (constraint <- uselessConstraints) {
                logger.log("Removing useless constraint %s".format(constraint))
                for (x <- constraint.inVariables.toSet) {
                    deregisterInflow(x, constraint)
                }
                for (x <- constraint.outVariables) {
                    deregisterOutflow(x)
                }
                removeFromFlowModel(constraint)
                constraints -= constraint
            }
            (! uselessConstraints.isEmpty)
        } do ()
        constraintOrder = null
        this
    }

    /** Counts how often Constraint.propagate was called. */
    var numberOfPropagations = 0

    /**
      * Prunes domains by propagating constraints.
      *
      * Terminates when interrupted or when a fixpoint is reached.
      *
      * Throws a DomainWipeOutException when unsatisfiability was proved.
      *
      * Restores domains of implicitly constrained search variables after propagation.
      *
      * Notice that, after propagation, there may be search variables with values outside their domains.
      */
    def propagate(): Space = {
        propagate {
            val tasks = new mutable.HashSet[Constraint]
            for (constraint <- constraints) {
                val effects = constraint.propagate()
                numberOfPropagations += 1
                for (x <- effects.affectedVariables) {
                    tasks ++= directlyAffectedConstraints(x)
                    tasks ++= maybeDefiningConstraint(x)
                }
                if (effects.rescheduleStep) {
                    tasks += constraint
                }
            }
            propagate(tasks)
        }
        this
    }

    /**
      * Departing from the given variables, prunes domains by propagating constraints.
      *
      * Terminates when interrupted or when a fixpoint is reached.
      *
      * Throws a DomainWipeOutException when unsatisfiability was proved.
      *
      * Restores domains of implicitly constrained search variables after propagation.
      *
      * Notice that, after propagation, there may be search variables with values outside their domains.
      */
    def propagate(xs: Iterable[AnyVariable]): Space = {
        propagate {
            val tasks = new mutable.HashSet[Constraint]
            for (x <- xs) {
                tasks ++= directlyAffectedConstraints(x)
                tasks ++= maybeDefiningConstraint(x)
            }
            propagate(tasks)
        }
        this
    }

    private def propagate(tasks: mutable.HashSet[Constraint]): Unit = {
        while (! tasks.isEmpty && ! sigint.isSet) {
            val constraint = tasks.head
            val effects = constraint.propagate()
            numberOfPropagations += 1
            for (x <- effects.affectedVariables) {
                tasks ++= directlyAffectedConstraints(x)
                tasks ++= maybeDefiningConstraint(x)
            }
            if (! effects.rescheduleStep) {
                tasks.remove(constraint)
            }
        }
    }

    private def propagate(propagationJob: => Unit): Unit = {

        // collect domains of implicitly constrained search variables
        val backup = new mutable.ArrayBuffer[(AnyVariable, () => Unit)]
        for (x <- inVariablesOfImplicitConstraints) {
            backup += x -> x.createDomainRestorer
        }

        // propagate constraints
        propagationJob

        // restore domains of implicitly constrained search variables
        for ((x, domainRestorer) <- backup) {
            domainRestorer.apply()
        }

    }

    /** Counts how often Constraint.initialize was called. */
    var numberOfInitializations = 0

    /**
     * Initializes the constraint network for local search.
     *
     * The caller has to assign values to all search variables before initializing!
     */
    def initialize(): Space = {
        if (constraintOrder.eq(null)) {
            sortConstraintsTopologically()
            // free memory
            flowModel = null
        }
        for (constraint <- constraints.iterator.filterNot(isImplicitConstraint(_)).toBuffer.sorted(ConstraintOrdering)) {
            constraint.initialize(assignment).foreach(_.affect(this))
            numberOfInitializations += 1
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
        initialize()
    }

    abstract private class MoveProcessor(val move: Move) {
        // Commits could be made cheaper by storing the affected constraints in the order of
        // processing while consulting.
        // This makes consulting more expensive, increases code complexity, and does not
        // pay off because commits are very rare at the normal operating temperatures of
        // simulated annealing.
        require(constraintOrder.ne(null), "Call initialize after posting the last constraint")
        protected val diff = new BulkMove(move.id)
        private val diffs = new java.util.TreeMap[Constraint, BulkMove](ConstraintOrdering)
        private def propagateEffect(effect: AnyMoveEffect): Unit = {
            if (assignment.value(effect.x) != effect.a) {
                val affectedConstraints = directlyAffectedConstraints(effect.x)
                for (constraint <- affectedConstraints) {
                    if (! isImplicitConstraint(constraint)) {
                        var diff = diffs.get(constraint)
                        if (diff.eq(null)) {
                            diff = new BulkMove(move.id)
                            diffs.put(constraint, diff)
                        }
                        diff += effect
                    }
                }
                recordEffect(effect)
            }
        }
        protected def processConstraint(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move): Iterable[AnyMoveEffect]
        protected def recordEffect(effect: AnyMoveEffect): Unit
        def run(): Move = {
            move.effectsIterator.foreach(propagateEffect)
            while (! diffs.isEmpty) {
                val entry = diffs.pollFirstEntry
                val constraint = entry.getKey
                val diff = entry.getValue
                val after = new MoveSimulator(assignment, diff)
                processConstraint(constraint, assignment, after, diff).foreach(propagateEffect)
            }
            diff
        }
    }

    /** Counts how often Constraint.consult was called. */
    var numberOfConsultations = 0

    private final class EffectComputer(move: Move) extends MoveProcessor(move) {
        override protected def processConstraint(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move) =
        {
            numberOfConsultations += 1
            if (checkIncrementalCostUpdate) {
                checkedConsult(constraint, before, after, move)
            } else {
                constraint.consult(before, after, move)
            }
        }
        override protected def recordEffect(effect: AnyMoveEffect): Unit = {
            if (isObjectiveVariable(effect.x)) {
                diff += effect
            }
        }
        private def checkedConsult(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move) =
        {
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
                println("constraint.consult(before, after, move) = %s".format(constraint.consult(before, after, move).toList))
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
    }

    private var idOfMostRecentlyAssessedMove = moveIdFactory.nextId()

    /**
     * Computes the search state that would result from applying the given move to
     * the current search state.
     *
     * The move must involve search variables only!
     * (For efficiency reasons, this requirement is not enforced.)
     */
    def consult(move: Move): SearchState = {
        idOfMostRecentlyAssessedMove = move.id
        new MoveSimulator(assignment, new EffectComputer(move).run())
    }

    private final class EffectPropagator(move: Move) extends MoveProcessor(move) {
        override protected def processConstraint(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move) =
        {
            numberOfCommitments += 1
            if (checkIncrementalCostUpdate) {
                checkedCommit(constraint, before, after, move)
            } else {
                constraint.commit(before, after, move)
            }
        }
        override protected def recordEffect(effect: AnyMoveEffect): Unit = {
            diff += effect
        }
        private def checkedCommit(
            constraint: Constraint, before: SearchState, after: SearchState, move: Move) =
        {
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

    /** Counts how often Constraint.commit was called. */
    var numberOfCommitments = 0

    /**
     * Performs the given move.
     *
     * Throws when consult was not called before commit.
     */
    def commit(move: Move): Space = {
        require(move.id == idOfMostRecentlyAssessedMove)
        new EffectPropagator(move).run().effectsIterator.foreach(_.affect(this))
        this
    }

    /** Throws when the internal data structures are inconsistent. */
    def checkConsistency(): Unit = {
        if (flowModel.ne(null)) {
            assert(flowModel.vertexSet().size() == constraints.size)
        }
    }

}
