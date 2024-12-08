package yuck.core

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.traverse.{BreadthFirstIterator, NotDirectedAcyclicGraphException, TopologicalOrderIterator}

import scala.collection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

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
    checkAssignmentsToNonChannelVariables: Boolean = false,
    delayCycleCheckingUntilInitialization: Boolean = false)
{

    private val constraints = new mutable.HashSet[Constraint] // maintained by post and removeUselessConstraints
    private val implicitConstraints = new mutable.HashSet[Constraint] // maintained by registerImplicitConstraint
    private val inVariables = new mutable.HashSet[AnyVariable] // maintained by post
    private val inVariablesOfImplicitConstraints = new mutable.HashSet[AnyVariable] // maintained by registerImplicitConstraint
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
        require(! isCyclic(constraint), "%s is cyclic".format(constraint))
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
        if (! delayCycleCheckingUntilInitialization) {
            // BFS seems to be faster than DFS
            val i = new BreadthFirstIterator[Constraint, DefaultEdge](flowModel, constraint) {
                override def encounterVertexAgain(vertex: Constraint, edge: DefaultEdge): Unit = {
                    if (vertex == constraint) {
                        flowModel.removeVertex(constraint)
                        require(false, "%s would introduce a cycle".format(constraint))
                    } else {
                        super.encounterVertexAgain(vertex, edge)
                    }
                }
            }
            while (i.hasNext) {
                i.next()
            }
        }
    }
    private def removeFromFlowModel(constraint: Constraint): Unit = {
        flowModel.removeVertex(constraint)
    }

    private def sortConstraintsTopologically(): Unit = {
        val constraintOrder = new Array[Int](constraints.iterator.map(_.id).max.rawId + 1)
        try {
            for ((constraint, i) <- new TopologicalOrderIterator[Constraint, DefaultEdge](flowModel).asScala.zipWithIndex) {
                constraintOrder.update(constraint.id.rawId, i)
            }
        } catch {
            case error: NotDirectedAcyclicGraphException =>
                throw new CyclicConstraintNetworkException
        }
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
        [V <: Value[V]]
        (name: String, domain: Domain[V])
        (using valueTraits: ValueTraits[V]):
        Variable[V] =
    {
        valueTraits.createVariable(this, name, domain)
    }

    private val objectiveVariables = new mutable.HashSet[AnyVariable]

    /**
     * Registers the given variable as objective variable.
     *
     * You need to register all variables for which you want to observe effects
     * due to consultation.
     *
     * Notice that registering other variables will do no harm except for slowing down
     * consultation.
     */
    def registerObjectiveVariable(x: AnyVariable): Space = {
        objectiveVariables += x
        this
    }

    /** Returns true iff the given variable is an objective variable. */
    inline def isObjectiveVariable(x: AnyVariable): Boolean =
        objectiveVariables.contains(x)

    /** Assigns the given value to the given variable. */
    def setValue[V <: Value[V]](x: Variable[V], a: V): Space = {
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
        x: AnyVariable, result: mutable.HashSet[AnyVariable], visited: mutable.HashSet[AnyVariable]): Unit =
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
        constraint: Constraint, result: mutable.HashSet[AnyVariable], visited: mutable.HashSet[AnyVariable]): Unit =
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
        x: AnyVariable, result: mutable.HashSet[Constraint], visited: mutable.HashSet[AnyVariable]): Unit =
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
        require(! initialized, "Space has already been initialized")
        if (isCyclic(constraint)) true
        else if (constraints.isEmpty) false
        else if (constraints.contains(constraint)) false
        else try {
            addToFlowModel(constraint)
            removeFromFlowModel(constraint)
            false
        }
        catch {
            case _: IllegalArgumentException => true
        }
    }

    private def isCyclic(constraint: Constraint): Boolean =
        constraint.outVariables.exists(constraint.inVariables.iterator.contains)

    /**
     * Adds the given constraint to the constraint network.
     *
     * Throws when adding the constraint would create a cycle in the network.
     */
    def post(constraint: Constraint): Space = {
        logger.log("Adding %s".format(constraint))
        require(! initialized, "Space has already been initialized")
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
        initialized = false
        this
    }

    /** Counts how often retract was called. */
    var numberOfRetractions = 0

    /**
     * Retracts the given constraint.
     *
     * Throws if the given constraint feeds into another constraint.
     */
    def retract(constraint: Constraint): Space = {
        logger.log("Retracting %s".format(constraint))
        require(
            constraint.outVariables.forall(x => directlyAffectedConstraints(x).isEmpty),
            "%s feeds into another constraint".format(constraint))
        for (x <- constraint.inVariables.toSet) {
            deregisterInflow(x, constraint)
        }
        for (x <- constraint.outVariables) {
            deregisterOutflow(x)
        }
        removeFromFlowModel(constraint)
        constraints -= constraint
        if (isImplicitConstraint(constraint)) {
            implicitConstraints -= constraint
            inVariablesOfImplicitConstraints --= constraint.inVariables
        }
        objectiveVariables --= constraint.outVariables
        initialized = false
        numberOfRetractions += 1
        this
    }

    /** Returns the number of constraints that were posted. */
    def numberOfConstraints: Int = constraints.size

    /** Returns the number of posted constraints which satisfy the given predicate. */
    def numberOfConstraints(p: Constraint => Boolean): Int =
        constraints.count(p)

    /** Returns the number of posted constraints of the given type. */
    def numberOfConstraints[T <: Constraint](using classTag: ClassTag[T]): Int =
        numberOfConstraints(classTag.runtimeClass.isInstance)

    /**
     * Registers the given constraint as implicit.
     *
     * Implicit constraints will not be initialized and they will never be consulted.
     *
     * Throws when the constraint has not yet been posted or cannot be registered as implicit.
     */
    def registerImplicitConstraint(constraint: Constraint): Space = {
        logger.log("Registering %s as implicit".format(constraint))
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
        initialized = false
        this
    }

    /** Returns true iff the given constraint was registered as implicit. */
    inline def isImplicitConstraint(constraint: Constraint): Boolean =
        implicitConstraints.contains(constraint)

    /** Returns the number of constraints that were posted and later registered as implicit. */
    def numberOfImplicitConstraints: Int = implicitConstraints.size

    /** Finds and retracts useless constraints. */
    def retractUselessConstraints(isUseless: Constraint => Boolean): Space = {
        val uselessConstraints = new mutable.HashSet[Constraint]
        while {
            uselessConstraints.clear()
            constraints.view.filter(isUseless).foreach(uselessConstraints.add)
            uselessConstraints.foreach(retract)
            !uselessConstraints.isEmpty
        } do ()
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

    private var initialized = false

    /**
     * Initializes the constraint network for local search.
     *
     * The caller has to assign values to all search variables before initializing!
     */
    def initialize(): Space = {

        if (! initialized) {
            if (delayCycleCheckingUntilInitialization) {
                sortConstraintsTopologically()
            }
            flowModel = null // free memory
            initialized = true
        }

        val layers = computeLayers()
        for (i <- layers.indices; constraint <- layers(i)) {
            constraint.layer = i
            constraint.after = null
            if (! isImplicitConstraint(constraint)) {
                constraint.initialize(assignment).foreach(_.affect(this))
                numberOfInitializations += 1
            }
        }
        queue = Vector.fill(layers.size)(new mutable.ArrayBuffer[Constraint] {
            inline override def clear() = {
                // No need to clear the underlying array!
                size0 = 0
            }
        })

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

    private def computeLayers(): IndexedSeq[Iterable[Constraint]] = {
        val layers = new mutable.ArrayBuffer[mutable.HashSet[Constraint]]
        val availableInputs = new mutable.HashSet[AnyVariable]
        availableInputs ++= searchVariables
        val candidates = new mutable.HashSet[Constraint]
        candidates ++= availableInputs.view.flatMap(directlyAffectedConstraints)
        candidates ++= constraints.view.filter(_.inVariables.view.filterNot(isProblemParameter).isEmpty)
        while (! candidates.isEmpty) {
            val layer = new mutable.HashSet[Constraint]
            layer ++= candidates.view.filter(_.inVariables.view.filterNot(isProblemParameter).toSet.subsetOf(availableInputs))
            layers += layer
            availableInputs ++= layer.view.flatMap(_.outVariables)
            candidates --= layer
            candidates ++= layer.view.flatMap(_.outVariables).flatMap(directlyAffectedConstraints)
        }
        assert(! layers.exists(_.isEmpty))
        assert(layers.view.map(_.size).sum == constraints.size)
        assert(layers.foldLeft(mutable.HashSet.empty[Constraint])((acc, layer) => acc ++= layer) == constraints)
        layers
    }

    /** Counts how often Constraint.consult was called. */
    var numberOfConsultations = 0

    private var idOfMostRecentlyAssessedMove = moveIdFactory.nextId()
    private var queue: Vector[mutable.ArrayBuffer[Constraint]] = null // per layer

    /** Returns the number of layers the constraints were partitioned into. */
    def numberOfLayers: Int = queue.size

    /**
     * Computes the search state that would result from applying the given move to
     * the current search state.
     *
     * The move must involve search variables only!
     * (For efficiency reasons, this requirement is not enforced.)
     */
    def consult(move: Move): SearchState = {
        require(initialized, "Call initialize after posting the last constraint")
        idOfMostRecentlyAssessedMove = move.id
        val acc = new BulkMove(move.id)
        var i = 0
        val n = queue.size
        while (i < n) {
            queue(i).clear()
            i += 1
        }
        propagateEffects(move, move.effectsIterator, acc)
        i = 0
        while (i < n) {
            val layer = queue(i)
            var j = layer.size - 1
            while (j >= 0) {
                val constraint = layer(j)
                val after = constraint.after
                val effects =
                    if checkIncrementalCostUpdate
                    then checkedConsult(constraint, assignment, after, after.move)
                    else constraint.consult(assignment, after, after.move)
                propagateEffects(move, effects.iterator, acc)
                numberOfConsultations += 1
                j -= 1
            }
            i += 1
        }
        new MoveSimulator(assignment, acc)
    }

    private def propagateEffects(move: Move, effectsIt: Iterator[AnyMoveEffect], acc: BulkMove): Unit = {
        while (effectsIt.hasNext) {
            val effect = effectsIt.next()
            if (assignment.value(effect.x) != effect.a) {
                val constraintsIt = directlyAffectedConstraints(effect.x).iterator
                while (constraintsIt.hasNext) {
                    val constraint = constraintsIt.next()
                    if (! isImplicitConstraint(constraint)) {
                        if (constraint.after != null && constraint.after.move == move) {
                            constraint.after.move.asInstanceOf[BulkMove] += effect
                        } else {
                            val diff = new BulkMove(move.id)
                            diff += effect
                            constraint.after = new MoveSimulator(assignment, diff)
                            queue(constraint.layer) += constraint
                        }
                    }
                }
                if (isObjectiveVariable(effect.x)) {
                    acc += effect
                }
            }
        }
    }

    /** Counts how often Constraint.commit was called. */
    var numberOfCommitments = 0

    private val pendingChanges = new mutable.ArrayBuffer[AnyMoveEffect] {
        inline override def clear() = {
            // No need to clear the underlying array!
            size0 = 0
        }
    } // for internal use by commit

    /**
     * Performs the given move.
     *
     * Throws when consult was not called before commit.
     */
    def commit(move: Move): Space = {
        require(move.id == idOfMostRecentlyAssessedMove)
        pendingChanges.clear()
        pendingChanges ++= move.effectsIterator
        var i = queue.size - 1
        while (i >= 0) {
            val layer = queue(i)
            var j = layer.size - 1
            while (j >= 0) {
                val constraint = layer(j)
                val after = constraint.after
                val effects =
                    if checkIncrementalCostUpdate
                    then checkedCommit(constraint, assignment, after, after.move)
                    else constraint.commit(assignment, after, after.move)
                pendingChanges ++= effects
                numberOfCommitments += 1
                j -= 1
            }
            i -= 1
        }
        var j = pendingChanges.size - 1
        while (j >= 0) {
            pendingChanges(j).affect(this)
            j -= 1
        }
        this
    }

    /** Throws when the internal data structures are inconsistent. */
    def checkConsistency(): Unit = {
        if (flowModel.ne(null)) {
            assert(flowModel.vertexSet().size() == constraints.size)
        }
    }

    private def checkedConsult(constraint: Constraint, before: SearchState, after: SearchState, move: Move): Iterable[AnyMoveEffect] = {
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

    private def checkedCommit(constraint: Constraint, before: SearchState, after: SearchState, move: Move): Iterable[AnyMoveEffect] = {
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
