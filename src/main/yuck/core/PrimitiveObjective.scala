package yuck.core

import java.lang.Math.{abs, log, signum}

/**
 * Objective for optimizing the value of a variable.
 *
 * @author Michael Marte
 */
abstract class PrimitiveObjective extends AnyObjective {

    /** The objective variable. */
    val x: AnyVariable

    final override def primitiveObjectives = Seq(this)
    final override def objectiveVariables = Seq(x)

    private var deltaScale = 0.0
    private var sampleSize = 0.0
    protected def computeDelta(before: SearchState, after: SearchState): Double

    final override def assessMove(before: SearchState, after: SearchState) = {
        var delta = computeDelta(before, after)
        if (delta != 0) {
            val sign = signum(delta)
            delta = abs(delta)
            // scale compression (log(1) = 0, so shift curve to the left)
            delta = log(0.1 + delta)
            // scale normalization (see http://math.stackexchange.com/questions/106700/incremental-averageing)
            sampleSize += 1
            deltaScale += (delta - deltaScale) / sampleSize
            delta /= deltaScale
            delta *= sign
        }
        delta
    }

    /** Returns the kind of optimization performed by this objective. */
    val optimizationMode: OptimizationMode

}
