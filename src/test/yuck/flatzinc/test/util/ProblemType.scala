package yuck.flatzinc.test.util

/**
 * @author Michael Marte
 *
 */
trait ProblemType

trait SatisfiabilityProblem extends ProblemType
trait MinimizationProblem extends ProblemType
trait MaximizationProblem extends ProblemType
trait UnsuitableProblem extends ProblemType
