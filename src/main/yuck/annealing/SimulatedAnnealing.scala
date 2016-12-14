package yuck.annealing

import yuck.core._

/**
 * Executes the search strategy defined by the given annealing schedule
 * and the given move generator.
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
    moveGenerator: MoveGenerator,
    randomGenerator: RandomGenerator,
    objective: AnyObjective,
    maybeRoundLimit: Option[Int],
    monitor: AnnealingMonitor,
    userData: Object,
    checkConstraintPropagation: Boolean)
    extends Solver
    with StandardSolverInterruptionSupport
{

    require(moveGenerator.searchVariables.toSet == space.searchVariables)

    private var roundCount = 0
    private var temperature = 0.0
    private var heatingPhase = false
    private var numberOfMonteCarloAttempts = 0
    private val currentProposal = space.searchState
    private var proposalBeforeSuspension: SearchState = null
    private var costsOfCurrentProposal: Costs = null
    private val result = new AnnealingResult(name, space, objective, userData)

    {
        space.initialize
        costsOfCurrentProposal = objective.costs(currentProposal)
        result.bestProposal = currentProposal.clone
        result.costsOfInitialProposal = costsOfCurrentProposal
        result.costsOfBestProposal = costsOfCurrentProposal
    }

    override def hasFinished =
        (maybeRoundLimit.isDefined && roundCount >= maybeRoundLimit.get) ||
        schedule.isFrozen ||
        result.isGoodEnough

    override def call  =
        if (hasFinished) {
            if (roundCount == 0 && monitor != null) {
                // The given, initial assignment is good enough and hence no search is necessary.
                // In this case, notify the monitor about the initial assignment.
                monitor.onSolverLaunched(result)
                monitor.onBetterProposal(result)
                monitor.onSolverFinished(result)
            }
            result
        }
        else if (proposalBeforeSuspension != null) continue
        else start

    private def start: AnnealingResult = {
        if (monitor != null) {
            monitor.onSolverLaunched(result)
        }
        anneal
        result
    }

    private def continue: AnnealingResult = {
        space.initialize(proposalBeforeSuspension)
        costsOfCurrentProposal = objective.costs(currentProposal)
        if (monitor != null) {
            monitor.onSolverResumed(result)
        }
        anneal
        result
    }

    private def anneal {

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
            if (checkConstraintPropagation) {
                assert(costsOfCurrentProposal == result.costsOfBestProposal)
            }
            result.costsOfFinalProposal = result.costsOfBestProposal
        }

        // public relations
        if (monitor != null) {
            if (hasFinished) {
                monitor.onSolverFinished(result)
            } else {
                monitor.onSolverSuspended(result)
            }
        }

    }

    private def nextRound {

        // prepare for new round
        if (numberOfMonteCarloAttempts == 0) {
            if (! result.roundLogs.isEmpty) {
                if (schedule.temperature <= temperature) {
                    if (heatingPhase) {
                        heatingPhase = false
                        if (monitor != null) {
                            monitor.onReheatingFinished(result)
                        }
                    }
                } else if (! heatingPhase) {
                    heatingPhase = true
                    if (monitor != null) {
                        monitor.onReheatingStarted(result)
                    }
                }
            }
            temperature = schedule.temperature
            numberOfMonteCarloAttempts = schedule.numberOfMonteCarloAttempts
            val roundLog = new RoundLog(result.roundLogs.size)
            result.roundLogs += roundLog
            roundLog.temperature = temperature
            roundLog.costsOfInitialProposal = objective.costs(currentProposal)
            roundLog.costsOfBestProposal = roundLog.costsOfInitialProposal
        }
        val roundLog = result.roundLogs.last

        // Monte Carlo simulation
        val startTime = System.currentTimeMillis
        monteCarloSimulation
        val endTime = System.currentTimeMillis

        // book-keeping
        roundLog.runtimeInMillis += (endTime - startTime)
        roundLog.numberOfMonteCarloAttempts = schedule.numberOfMonteCarloAttempts - numberOfMonteCarloAttempts
        roundLog.costsOfFinalProposal = costsOfCurrentProposal
        roundLog.updateAcceptanceRatio
        assert(
            roundLog.uphillAcceptanceRatio > 0.0 ||
            ! objective.isHigherThan(roundLog.costsOfFinalProposal, roundLog.costsOfInitialProposal))
        roundLog.numberOfConsultations += space.numberOfConsultations
        space.numberOfConsultations = 0
        roundLog.numberOfCommitments += space.numberOfCommitments
        space.numberOfCommitments = 0

        // cool down
        if (numberOfMonteCarloAttempts == 0) {
            roundLog.roundWasFutile =
                roundCount > 0 &&
                ! objective.isLowerThan(
                    roundLog.costsOfBestProposal,
                    result.roundLogs.apply(roundCount - 1).costsOfBestProposal)
            if (monitor != null) {
                monitor.onNextRound(result)
            }
            schedule.nextRound(roundLog)
            roundCount += 1
        }

    }

    private def monteCarloSimulation {
        val roundLog = result.roundLogs.last
        while (numberOfMonteCarloAttempts > 0 && ! wasInterrupted && ! result.isGoodEnough) {
            val move = moveGenerator.nextMove
            val before = space.searchState
            val after = space.consult(move, checkConstraintPropagation)
            val delta = objective.assessMove(before, after)
            if (delta <= 0 || randomGenerator.nextProbability <= scala.math.exp(-delta / temperature)) {
                space.commit(move, checkConstraintPropagation)
                costsOfCurrentProposal = objective.costs(currentProposal)
                if (delta > 0) {
                    roundLog.numberOfAcceptedUphillMoves += 1
                } else {
                    if (objective.isLowerThan(costsOfCurrentProposal, roundLog.costsOfBestProposal)) {
                        roundLog.costsOfBestProposal = costsOfCurrentProposal
                    }
                    if (objective.isLowerThan(costsOfCurrentProposal, result.costsOfBestProposal)) {
                        roundLog.bestProposalWasImproved = true
                        result.costsOfBestProposal = costsOfCurrentProposal
                        result.indexOfRoundWithBestProposal = result.roundLogs.length - 1
                        result.bestProposal = currentProposal.clone
                        if (monitor != null) {
                            monitor.onBetterProposal(result)
                        }
                    }
                }
            } else {
                roundLog.numberOfRejectedMoves += 1
            }
            numberOfMonteCarloAttempts -= 1
        }
    }

}
