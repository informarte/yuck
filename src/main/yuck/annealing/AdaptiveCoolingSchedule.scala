package yuck.annealing

/**
 * Counts the number n of futile rounds in succession; the counter is reset when
 * the solver (that uses this schedule) reports a successful round.
 *
 * The temperature for the next round depends on n and the given cooling rate r:
 * t' = t * r^n and hence the temperature does not drop after a successful round.
 *
 * Freezes up when the temperature drops below the given final temperature or when
 * n exceeds the given threshold, that is when the search gets stuck in an attraction
 * basin.
 *
 * @author Michael Marte
 */
final class AdaptiveCoolingSchedule(
    finalTemperature: Double,
    minimumUphillAcceptanceRatio: Double,
    coolingRate: Double,
    override val numberOfMonteCarloAttempts: Int,
    maximumNumberOfSuccessiveFutileRoundsInAttractionBasin: Int)
    extends AnnealingSchedule
{

    require(finalTemperature >= 0.0)
    require(minimumUphillAcceptanceRatio >= 0.0 && minimumUphillAcceptanceRatio <= 1.0)
    require(coolingRate > 0.0 && coolingRate < 1.0)
    require(maximumNumberOfSuccessiveFutileRoundsInAttractionBasin > 0)

    private var currentTemperature = 1.0
    private var numberOfSuccessiveFutileRounds = 0
    private var numberOfSuccessiveFutileRoundsInAttractionBasin = 0

    override def nextRound(roundLog: RoundLog) = {
        require(! isFrozen)
        if (roundLog.roundWasFutile) {
            numberOfSuccessiveFutileRounds += 1
            if (roundLog.uphillAcceptanceRatio <= minimumUphillAcceptanceRatio) {
                numberOfSuccessiveFutileRoundsInAttractionBasin += 1
            } else {
                numberOfSuccessiveFutileRoundsInAttractionBasin = 0
            }
        } else {
            numberOfSuccessiveFutileRounds = 0
            numberOfSuccessiveFutileRoundsInAttractionBasin = 0
        }
        if (! isFrozen) {
            if (numberOfSuccessiveFutileRounds > 0) {
                currentTemperature *= Math.pow(coolingRate, numberOfSuccessiveFutileRounds)
            }
        }
    }

    override def temperature =
        currentTemperature

    override def isFrozen =
        currentTemperature <= finalTemperature ||
        numberOfSuccessiveFutileRoundsInAttractionBasin >= maximumNumberOfSuccessiveFutileRoundsInAttractionBasin

    override def start(temperature: Double, progress: Double) = {
        require(temperature > finalTemperature)
        currentTemperature = temperature
        numberOfSuccessiveFutileRounds = 0
        numberOfSuccessiveFutileRoundsInAttractionBasin = 0
    }

}
