package yuck.annealing

import scala.collection.*

import yuck.core.*
import yuck.util.arm.Sigint

/**
 * Executes the search strategy defined by the given annealing schedule
 * and the given neighbourhood.
 *
 * Terminates when the given objective is reached, the given annealing schedule
 * freezes up, or when the optional round limit is reached.
 *
 * Keeps track of the best proposal and restores it upon interruption or termination.
 *
 * Supports the propagation of the bounds established by progressive tightening.
 * Notice that bound propagation may prove optimality and that, in such a case, the underlying
 * space might be left unusable due to variables with empty domains.
 *
 * @author Michael Marte
 */
final class SimulatedAnnealing(
    override val name: String,
    space: Space,
    schedule: AnnealingSchedule,
    neighbourhood: Neighbourhood,
    randomGenerator: RandomGenerator,
    objective: AnyObjective,
    maybeRoundLimit: Option[Int],
    maybeMonitor: Option[AnnealingMonitor],
    maybeUserData: Option[Object],
    sigint: Sigint)
    extends Solver
{
    private var roundCount = 0
    private var temperature = 0.0
    private var heatingPhase = false
    private var numberOfRemainingMonteCarloAttempts = 0
    private val currentProposal = space.searchState
    private var costsOfCurrentProposal: Costs = null
    private var bestProposal: SearchState = null
    private var costsOfBestProposal: Costs = null
    private var proposalBeforeSuspension: SearchState = null
    private val roundLogs = new mutable.ArrayBuffer[RoundLog]

    {
        space.initialize()
        costsOfCurrentProposal = objective.costs(currentProposal)
        bestProposal = currentProposal.clone
        costsOfBestProposal = costsOfCurrentProposal
    }

    private def wasInterrupted = sigint.isSet

    override def hasFinished =
        (maybeRoundLimit.isDefined && roundCount >= maybeRoundLimit.get) ||
        schedule.isFrozen ||
        objective.isGoodEnough(costsOfBestProposal)

    override def call()  =
        if (hasFinished) {
            val result = createResult()
            if (roundCount == 0 && maybeMonitor.isDefined) {
                // The given, initial assignment is good enough and hence no search is necessary.
                // In this case, notify the monitor about the initial assignment.
                val monitor = maybeMonitor.get
                monitor.onSolverLaunched(result)
                monitor.onBetterProposal(result)
                monitor.onSolverFinished(result)
            }
            result
        }
        else if (proposalBeforeSuspension.ne(null)) resume()
        else start()

    private def start(): AnnealingResult = {
        if (maybeMonitor.isDefined) {
            val monitor = maybeMonitor.get
            monitor.onSolverLaunched(createResult())
        }
        anneal()
        createResult()
    }

    private def resume(): AnnealingResult = {
        space.initialize(proposalBeforeSuspension)
        costsOfCurrentProposal = objective.costs(currentProposal)
        if (maybeMonitor.isDefined) {
            maybeMonitor.get.onSolverResumed(createResult())
        }
        anneal()
        createResult()
    }

    private def anneal(): Unit = {

        // main annealing loop
        while (! wasInterrupted && ! hasFinished) {
            nextRound()
        }

        // book-keeping
        val costsOfFinalProposal = costsOfCurrentProposal
        proposalBeforeSuspension = currentProposal.clone

        // revert to best assignment
        if (objective.isLowerThan(costsOfBestProposal, costsOfCurrentProposal)) {
            space.initialize(bestProposal)
            costsOfCurrentProposal = objective.costs(currentProposal)
            assert(costsOfCurrentProposal == costsOfBestProposal)
        }

        // public relations
        if (maybeMonitor.isDefined) {
            if (hasFinished) {
                maybeMonitor.get.onSolverFinished(createResult())
            } else {
                maybeMonitor.get.onSolverSuspended(createResult())
            }
        }

    }

    private def nextRound(): Unit = {

        // prepare for new round
        if (numberOfRemainingMonteCarloAttempts == 0) {
            if (! roundLogs.isEmpty) {
                if (schedule.temperature <= temperature) {
                    if (heatingPhase) {
                        heatingPhase = false
                        if (maybeMonitor.isDefined) {
                            maybeMonitor.get.onReheatingFinished(createResult())
                        }
                    }
                } else if (! heatingPhase) {
                    heatingPhase = true
                    if (maybeMonitor.isDefined) {
                        maybeMonitor.get.onReheatingStarted(createResult())
                    }
                }
            }
            temperature = schedule.temperature
            numberOfRemainingMonteCarloAttempts = schedule.numberOfMonteCarloAttempts
            val roundLog = new RoundLog(roundLogs.size)
            roundLogs += roundLog
            roundLog.temperature = temperature
            assert(costsOfCurrentProposal == objective.costs(currentProposal))
            roundLog.costsOfInitialProposal = costsOfCurrentProposal
            roundLog.costsOfBestProposal = costsOfCurrentProposal
        }
        val roundLog = roundLogs.last

        // Monte Carlo simulation
        val startTimeInMillis = System.currentTimeMillis
        monteCarloSimulation()
        val endTimeInMillis = System.currentTimeMillis

        // book-keeping
        roundLog.runtimeInMillis += (endTimeInMillis - startTimeInMillis)
        roundLog.numberOfMonteCarloAttempts = schedule.numberOfMonteCarloAttempts - numberOfRemainingMonteCarloAttempts
        roundLog.costsOfFinalProposal = costsOfCurrentProposal
        roundLog.updateAcceptanceRatio()
        assert(! objective.isHigherThan(roundLog.costsOfBestProposal, roundLog.costsOfInitialProposal))
        assert(! objective.isHigherThan(roundLog.costsOfBestProposal, roundLog.costsOfFinalProposal))
        // If roundLog.numberOfAcceptedUphillMoves == 0, roundLog.costsOfFinalProposal may still be
        // higher than roundLog.costsOfBestProposal due to tightening.
        roundLog.numberOfConsultations += space.numberOfConsultations
        space.numberOfConsultations = 0
        roundLog.numberOfCommitments += space.numberOfCommitments
        space.numberOfCommitments = 0

        // cool down
        if (numberOfRemainingMonteCarloAttempts == 0) {
            roundLog.roundWasFutile =
                roundCount > 0 &&
                (! objective.isLowerThan(
                    roundLog.costsOfBestProposal,
                    roundLogs.apply(roundCount - 1).costsOfBestProposal))
            if (maybeMonitor.isDefined) {
                maybeMonitor.get.onNextRound(createResult())
            }
            schedule.nextRound(roundLog)
            roundCount += 1
        }

    }

    private def monteCarloSimulation(): Unit = {
        val roundLog = roundLogs.last
        while (numberOfRemainingMonteCarloAttempts > 0 && ! wasInterrupted && ! objective.isGoodEnough(costsOfBestProposal)) {
            val move = neighbourhood.nextMove()
            val before = space.searchState
            val after = space.consult(move)
            val delta = objective.assessMove(before, after)
            if (delta <= 0 || randomGenerator.nextProbability() <= scala.math.exp(-delta / temperature)) {
                space.commit(move)
                neighbourhood.commit(move)
                postprocessMove(roundLog)
                if (delta > 0) {
                    roundLog.numberOfAcceptedUphillMoves += 1
                }
            } else {
                roundLog.numberOfRejectedMoves += 1
            }
            numberOfRemainingMonteCarloAttempts -= 1
        }
    }

    private def postprocessMove(roundLog: RoundLog): Unit = {
        objective.findActualObjectiveValue(space)
        costsOfCurrentProposal = objective.costs(currentProposal)
        if (objective.isLowerThan(costsOfCurrentProposal, roundLog.costsOfBestProposal)) {
            roundLog.costsOfBestProposal = costsOfCurrentProposal
        }
        val bestProposalWasImproved = objective.isLowerThan(costsOfCurrentProposal, costsOfBestProposal)
        if (bestProposalWasImproved) {
            roundLog.bestProposalWasImproved = true
            costsOfBestProposal = costsOfCurrentProposal
            bestProposal = currentProposal.clone
        }
        val tightenedVariables = objective.tighten(space)
        if (maybeMonitor.isDefined) {
            for (x <- tightenedVariables) {
                maybeMonitor.get.onObjectiveTightened(x)
            }
        }
        costsOfCurrentProposal = objective.costs(currentProposal)
        if (bestProposalWasImproved && maybeMonitor.isDefined) {
            maybeMonitor.get.onBetterProposal(createResult())
        }
    }

    private def createResult(): AnnealingResult =
        new AnnealingResult(maybeUserData, name, objective, bestProposal, roundLogs)

}
