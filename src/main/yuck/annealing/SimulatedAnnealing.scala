package yuck.annealing

import scala.collection.*

import yuck.core.*
import yuck.util.arm.Sigint

/**
 * Starts the given annealing schedule with the given temperature and then executes
 * the search strategy defined by the schedule and the given neighbourhood.
 *
 * Terminates when the given objective is reached or when the optional round limit
 * is reached.
 *
 * When the schedules freezes up, the current assignment is perturbed and the schedule
 * is restarted with the given restart temperature.
 *
 * Keeps track of the best proposal and restores it upon interruption or termination.
 *
 * @author Michael Marte
 */
final class SimulatedAnnealing(
    override val name: String,
    space: Space,
    objective: AnyObjective,
    neighbourhood: Neighbourhood,
    schedule: AnnealingSchedule,
    startTemperature: Double,
    restartTemperature: Double,
    restartPerturbationProbability: Probability,
    maybeSharedBound: Option[SharedBound],
    randomGenerator: RandomGenerator,
    maybeRoundLimit: Option[Int],
    maybeMonitor: Option[AnnealingMonitor],
    maybeUserData: Option[Object],
    sigint: Sigint)
    extends Solver
{

    require(restartPerturbationProbability.value > 0)

    private var roundCount = 0
    private var temperature = 0.0
    private var heatingPhase = false
    private var numberOfRemainingMonteCarloAttempts = 0
    private val currentProposal = space.searchState
    private var costsOfCurrentProposal: Costs = null
    private var bestProposal: SearchState = null
    private var costsOfBestProposal: Costs = null
    private var proposalBeforeSuspension: SearchState = null
    private var maybeLastSeenBound: Option[Costs] = None
    private val roundLogs = new mutable.ArrayBuffer[RoundLog]
    private var numberOfMonteCarloAttempts = 0L
    private var runtimeInMillis = 0L
    private var numberOfPerturbations = 0

    {
        space.initialize()
        costsOfCurrentProposal = objective.costs(currentProposal)
        bestProposal = currentProposal.clone
        costsOfBestProposal = costsOfCurrentProposal
        schedule.start(startTemperature, 0)
    }

    private def wasInterrupted = sigint.isSet

    override def hasFinished =
        (maybeRoundLimit.isDefined && roundCount >= maybeRoundLimit.get) ||
        objective.isGoodEnough(costsOfBestProposal)

    override def call() =
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
            maybeMonitor.get.onSolverLaunched(createResult())
        }
        if (objective.isSolution(currentProposal)) {
            // Sometimes the initial assignment is a solution.
            objective.findActualObjectiveValue(space)
            costsOfCurrentProposal = objective.costs(currentProposal)
            tightenObjective(costsOfCurrentProposal)
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
        val startTimeInMillis = System.currentTimeMillis
        while (! wasInterrupted && ! hasFinished) {
            nextRound()
            if (schedule.isFrozen) {
                restart()
            }
        }
        val endTimeInMillis = System.currentTimeMillis

        // book-keeping
        val costsOfFinalProposal = costsOfCurrentProposal
        proposalBeforeSuspension = currentProposal.clone
        runtimeInMillis += (endTimeInMillis - startTimeInMillis)

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
                        numberOfPerturbations += 1
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

        // ingest shared bound
        ingestSharedBound()

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
                processMove(move, roundLog)
                if (delta > 0) {
                    roundLog.numberOfAcceptedUphillMoves += 1
                }
            } else {
                roundLog.numberOfRejectedMoves += 1
            }
            numberOfMonteCarloAttempts += 1
            numberOfRemainingMonteCarloAttempts -= 1
        }
    }

    private def processMove(move: Move, roundLog: RoundLog): Unit = {
        space.commit(move)
        neighbourhood.commit(move)
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
            if (maybeMonitor.isDefined) {
                maybeMonitor.get.onBetterProposal(createResult())
            }
            tightenObjective(costsOfBestProposal)
        }
    }

    private def ingestSharedBound(): Unit = {
        if (maybeSharedBound.isDefined) {
            val maybeBound = maybeSharedBound.get.maybeBound()
            if (maybeBound.isDefined) {
                val bound = maybeBound.get
                if ((maybeLastSeenBound.isEmpty && objective.isLowerThan(bound, costsOfBestProposal)) ||
                    (maybeLastSeenBound.isDefined && objective.isLowerThan(bound, maybeLastSeenBound.get))) {
                    tightenObjective(bound)
                    maybeLastSeenBound = maybeBound
                }
            }
        }
    }

    private def restart(): Unit = {
        neighbourhood.perturb(restartPerturbationProbability)
        numberOfPerturbations += 1
        objective.findActualObjectiveValue(space)
        costsOfCurrentProposal = objective.costs(currentProposal)
        if (objective.isLowerThan(costsOfCurrentProposal, costsOfBestProposal)) {
            costsOfBestProposal = costsOfCurrentProposal
            bestProposal = currentProposal.clone
            if (maybeMonitor.isDefined) {
                maybeMonitor.get.onBetterProposal(createResult())
            }
            tightenObjective(costsOfBestProposal)
        }
        schedule.start(restartTemperature, 0)
        if (maybeMonitor.isDefined) {
            maybeMonitor.get.onScheduleRestarted(createResult())
        }
    }

    private def tightenObjective(bound: Costs): Unit = {
        val tightenedVariables = objective.tighten(space, bound)
        if (maybeMonitor.isDefined) {
            val monitor = maybeMonitor.get
            for (x <- tightenedVariables) {
                monitor.onObjectiveTightened(createResult(), x)
            }
        }
        costsOfCurrentProposal = objective.costs(currentProposal)
    }

    private def createResult(): AnnealingResult =
        new AnnealingResult(
            maybeUserData, name, objective, bestProposal,
            numberOfMonteCarloAttempts, runtimeInMillis,
            space.numberOfConsultations, space.numberOfCommitments,
            numberOfPerturbations,
            roundLogs)

}
