package yuck.fj

import yuck.core.*
import yuck.util.arm.Sigint

/**
 * This class implements a generalization of the Feasibility Jump procedure,
 * inspired by the 2024 CPAIOR paper on VolationLS by Davies et al.
 *
 * Terminates when the given objective is reached.
 *
 * Keeps track of the best proposal and restores it upon interruption or termination.
 */
final class FeasibilityJump
    (override val name: String,
     space: Space,
     objective: AnyObjective,
     neighbourhood: FeasibilityJumpNeighbourhood,
     numberOfMovesPerRound: Int,
     numberOfSuccessiveFutileRoundsUntilPerturbation: Int,
     perturbationProbability: Probability,
     maybeSharedBound: Option[SharedBound],
     randomGenerator: RandomGenerator,
     maybeMonitor: Option[FeasibilityJumpMonitor],
     maybeUserData: Option[Object],
     sigint: Sigint)
    extends Solver
{

    require(numberOfMovesPerRound > 0)
    require(numberOfSuccessiveFutileRoundsUntilPerturbation > 0)
    require(perturbationProbability.value > 0)

    private val currentProposal = space.searchState
    private var costsOfCurrentProposal: Costs = null
    private var bestProposal: SearchState = null
    private var costsOfBestProposal: Costs = null
    private var proposalBeforeSuspension: SearchState = null

    private var numberOfSuccessiveFutileRounds: Int = 0

    private var maybeLastSeenBound: Option[Costs] = None

    private var numberOfMoves: Long = 0
    private var runtimeInMillis: Long = 0
    private var numberOfPerturbations: Int = 0

    {
        space.initialize()
        costsOfCurrentProposal = objective.costs(currentProposal)
        bestProposal = currentProposal.clone
        costsOfBestProposal = costsOfCurrentProposal
    }

    private var wasStarted = false

    private def wasInterrupted = sigint.isSet

    override def hasFinished = objective.isGoodEnough(costsOfBestProposal)

    override def call() =
        if hasFinished then {
            val result = createResult()
            if ! wasStarted && maybeMonitor.isDefined then {
                // The given, initial assignment is good enough and hence no search is necessary.
                // In this case, notify the monitor about the initial assignment.
                val monitor = maybeMonitor.get
                monitor.onSolverLaunched(result)
                monitor.onBetterProposal(result)
                monitor.onSolverFinished(result)
            }
            result
        } else if proposalBeforeSuspension.ne(null) then {
            resume()
        } else {
            start()
        }

    private def start(): FeasibilityJumpResult = {
        wasStarted = true
        if maybeMonitor.isDefined then {
            maybeMonitor.get.onSolverLaunched(createResult())
        }
        if objective.isSolution(currentProposal) then {
            // Sometimes the initial assignment is a solution.
            objective.findActualObjectiveValue(space)
            costsOfCurrentProposal = objective.costs(currentProposal)
            tightenObjective(costsOfCurrentProposal)
        }
        fj()
        createResult()
    }

    private def resume(): FeasibilityJumpResult = {
        space.initialize(proposalBeforeSuspension)
        costsOfCurrentProposal = objective.costs(currentProposal)
        if maybeMonitor.isDefined then {
            maybeMonitor.get.onSolverResumed(createResult())
        }
        fj()
        createResult()
    }

    private def fj(): Unit = {

        // iterated local search
        val startTimeInMillis = System.currentTimeMillis
        ils()
        val endTimeInMillis = System.currentTimeMillis

        // book-keeping
        proposalBeforeSuspension = currentProposal.clone
        runtimeInMillis += (endTimeInMillis - startTimeInMillis)

        // revert to best assignment
        if objective.isLowerThan(costsOfBestProposal, costsOfCurrentProposal) then {
            space.initialize(bestProposal)
            costsOfCurrentProposal = objective.costs(currentProposal)
            assert(costsOfCurrentProposal == costsOfBestProposal)
        }

        // public relations
        if maybeMonitor.isDefined then {
            if hasFinished then {
                maybeMonitor.get.onSolverFinished(createResult())
            } else {
                maybeMonitor.get.onSolverSuspended(createResult())
            }
        }

    }

    private def ils(): Unit = {
        while ! wasInterrupted && ! hasFinished do {
            if maybeMonitor.isDefined then {
                maybeMonitor.get.onNextRound(createResult())
            }
            if gls() then {
                numberOfSuccessiveFutileRounds = 0
            } else {
                numberOfSuccessiveFutileRounds += 1
                if numberOfSuccessiveFutileRounds >= numberOfSuccessiveFutileRoundsUntilPerturbation then {
                    perturb()
                    numberOfSuccessiveFutileRounds = 0
                }
            }
            if ingestSharedBound() then {
                numberOfSuccessiveFutileRounds = 0
            }
        }
    }

    private def gls(): Boolean = {
        var numberOfRemainingMoves = numberOfMovesPerRound
        var bestProposalWasImproved = false
        while numberOfRemainingMoves > 0 && ! wasInterrupted && ! hasFinished do {
            val move = neighbourhood.nextMove()
            if ! move.isEmpty then {
                bestProposalWasImproved |= performMove(move)
                neighbourhood.commit(move)
                numberOfMoves += 1
            }
            numberOfRemainingMoves -= 1
        }
        bestProposalWasImproved
    }

    private def perturb(): Unit = {
        neighbourhood.perturb(perturbationProbability)
        postprocessChange()
        if maybeMonitor.isDefined then {
            maybeMonitor.get.onPerturbation(createResult())
        }
        numberOfPerturbations += 1
    }

    private def performMove(move: Move): Boolean = {
        space.consult(move)
        space.commit(move)
        postprocessChange()
    }

    private def postprocessChange(): Boolean = {
        objective.findActualObjectiveValue(space)
        costsOfCurrentProposal = objective.costs(currentProposal)
        val bestProposalWasImproved = objective.isLowerThan(costsOfCurrentProposal, costsOfBestProposal)
        if bestProposalWasImproved then {
            costsOfBestProposal = costsOfCurrentProposal
            bestProposal = currentProposal.clone
            if maybeMonitor.isDefined then {
                maybeMonitor.get.onBetterProposal(createResult())
            }
            if objective.isSolution(costsOfBestProposal) then {
                tightenObjective(costsOfBestProposal)
                neighbourhood.onObjectiveTightened()
            }
        }
        bestProposalWasImproved
    }

    private def ingestSharedBound(): Boolean = {
        if maybeSharedBound.isDefined then {
            val maybeBound = maybeSharedBound.get.maybeBound()
            if maybeBound.isDefined then {
                val bound = maybeBound.get
                if (maybeLastSeenBound.isEmpty && objective.isLowerThan(bound, costsOfBestProposal)) ||
                    (maybeLastSeenBound.isDefined && objective.isLowerThan(bound, maybeLastSeenBound.get)) then
                {
                    tightenObjective(bound)
                    neighbourhood.onObjectiveTightened()
                    maybeLastSeenBound = maybeBound
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    private def tightenObjective(bound: Costs): Unit = {
        val tightenedVariables = objective.tighten(space, bound)
        if maybeMonitor.isDefined then {
            val monitor = maybeMonitor.get
            for x <- tightenedVariables do {
                monitor.onObjectiveTightened(createResult(), x)
            }
        }
        costsOfCurrentProposal = objective.costs(currentProposal)
    }

    private def createResult(): FeasibilityJumpResult =
        new FeasibilityJumpResult(
            maybeUserData, name, objective, bestProposal,
            numberOfMoves, runtimeInMillis,
            space.numberOfConsultations, space.numberOfCommitments,
            numberOfPerturbations)

}
