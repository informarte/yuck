package yuck.annealing

import yuck.core._
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
    private var proposalBeforeSuspension: SearchState = null
    private var costsOfCurrentProposal: Costs = null
    private val result = new AnnealingResult(name, space, objective, maybeUserData)

    {
        space.initialize
        costsOfCurrentProposal = objective.costs(currentProposal)
        result.bestProposal = currentProposal.clone
        result.costsOfInitialProposal = costsOfCurrentProposal
        result.costsOfBestProposal = costsOfCurrentProposal
    }

    private def wasInterrupted = sigint.isSet

    override def hasFinished =
        (maybeRoundLimit.isDefined && roundCount >= maybeRoundLimit.get) ||
        schedule.isFrozen ||
        result.isGoodEnough

    override def call  =
        if (hasFinished) {
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
        else if (proposalBeforeSuspension != null) resume
        else start

    private def start: AnnealingResult = {
        if (maybeMonitor.isDefined) {
            maybeMonitor.get.onSolverLaunched(result)
        }
        try {
            anneal
        }
        catch {
            case error: Throwable =>
                if (maybeMonitor.isDefined) {
                    maybeMonitor.get.onSolverFinished(result)
                }
                throw error
        }
        result
    }

    private def resume: AnnealingResult = {
        space.initialize(proposalBeforeSuspension)
        costsOfCurrentProposal = objective.costs(currentProposal)
        if (maybeMonitor.isDefined) {
            maybeMonitor.get.onSolverResumed(result)
        }
        anneal
        result
    }

    private def anneal: Unit = {

        // main annealing loop
        while (! wasInterrupted && ! hasFinished) {
            nextRound
        }

        // book-keeping
        result.costsOfFinalProposal = costsOfCurrentProposal
        proposalBeforeSuspension = currentProposal.clone

        // revert to best assignment
        if (objective.isLowerThan(result.costsOfBestProposal, result.costsOfFinalProposal)) {
            space.initialize(result.bestProposal)
            costsOfCurrentProposal = objective.costs(currentProposal)
            assert(costsOfCurrentProposal == result.costsOfBestProposal)
            result.costsOfFinalProposal = result.costsOfBestProposal
        }

        // public relations
        if (maybeMonitor.isDefined) {
            if (hasFinished) {
                maybeMonitor.get.onSolverFinished(result)
            } else {
                maybeMonitor.get.onSolverSuspended(result)
            }
        }

    }

    private def nextRound: Unit = {

        // prepare for new round
        if (numberOfRemainingMonteCarloAttempts == 0) {
            if (! result.roundLogs.isEmpty) {
                if (schedule.temperature <= temperature) {
                    if (heatingPhase) {
                        heatingPhase = false
                        if (maybeMonitor.isDefined) {
                            maybeMonitor.get.onReheatingFinished(result)
                        }
                    }
                } else if (! heatingPhase) {
                    heatingPhase = true
                    if (maybeMonitor.isDefined) {
                        maybeMonitor.get.onReheatingStarted(result)
                    }
                }
            }
            temperature = schedule.temperature
            numberOfRemainingMonteCarloAttempts = schedule.numberOfMonteCarloAttempts
            val roundLog = new RoundLog(result.roundLogs.size)
            result.roundLogs += roundLog
            roundLog.temperature = temperature
            assert(costsOfCurrentProposal == objective.costs(currentProposal))
            roundLog.costsOfInitialProposal = costsOfCurrentProposal
            roundLog.costsOfBestProposal = costsOfCurrentProposal
        }
        val roundLog = result.roundLogs.last

        // Monte Carlo simulation
        val startTime = System.currentTimeMillis
        monteCarloSimulation
        val endTime = System.currentTimeMillis

        // book-keeping
        roundLog.runtimeInMillis += (endTime - startTime)
        roundLog.numberOfMonteCarloAttempts = schedule.numberOfMonteCarloAttempts - numberOfRemainingMonteCarloAttempts
        roundLog.costsOfFinalProposal = costsOfCurrentProposal
        roundLog.updateAcceptanceRatio
        assert(! objective.isHigherThan(roundLog.costsOfBestProposal, roundLog.costsOfInitialProposal))
        assert(! objective.isHigherThan(roundLog.costsOfBestProposal, roundLog.costsOfFinalProposal))
        roundLog.numberOfConsultations += space.numberOfConsultations
        space.numberOfConsultations = 0
        roundLog.numberOfCommitments += space.numberOfCommitments
        space.numberOfCommitments = 0

        // cool down
        if (numberOfRemainingMonteCarloAttempts == 0) {
            roundLog.roundWasFutile =
                roundCount > 0 &&
                ! objective.isLowerThan(
                    roundLog.costsOfBestProposal,
                    result.roundLogs.apply(roundCount - 1).costsOfBestProposal)
            if (maybeMonitor.isDefined) {
                maybeMonitor.get.onNextRound(result)
            }
            schedule.nextRound(roundLog)
            roundCount += 1
        }

    }

    private def monteCarloSimulation: Unit = {
        val roundLog = result.roundLogs.last
        while (numberOfRemainingMonteCarloAttempts > 0 && ! wasInterrupted && ! result.isGoodEnough) {
            val move = neighbourhood.nextMove
            val before = space.searchState
            val after = space.consult(move)
            val delta = objective.assessMove(before, after)
            if (delta <= 0 || randomGenerator.nextProbability <= scala.math.exp(-delta / temperature)) {
                space.commit(move)
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
        val costsBeforeTightenining = objective.costs(currentProposal)
        val TighteningResult(tightenedProposal, maybeTightenedVariable) = objective.tighten(space)
        val costsAfterTightening = objective.costs(tightenedProposal)
        assert(! objective.isHigherThan(costsAfterTightening, costsBeforeTightenining))
        if (objective.isLowerThan(costsAfterTightening, roundLog.costsOfBestProposal)) {
            roundLog.costsOfBestProposal = costsAfterTightening
        }
        if (objective.isLowerThan(costsAfterTightening, result.costsOfBestProposal)) {
            roundLog.bestProposalWasImproved = true
            result.costsOfBestProposal = costsAfterTightening
            result.indexOfRoundWithBestProposal = result.roundLogs.length - 1
            result.bestProposal =
                if (tightenedProposal.eq(space.searchState)) tightenedProposal.clone else tightenedProposal
            if (maybeMonitor.isDefined) {
                maybeMonitor.get.onBetterProposal(result)
            }
        }
        if (maybeMonitor.isDefined && maybeTightenedVariable.isDefined) {
            maybeMonitor.get.onObjectiveTightened(maybeTightenedVariable.get)
        }
        costsOfCurrentProposal = objective.costs(currentProposal)
    }

}
