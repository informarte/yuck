package yuck.test.util

import org.junit.platform.engine.TestExecutionResult
import org.junit.platform.launcher.{TestExecutionListener, TestIdentifier, TestPlan}

/**
 * Prints a character for each test result.
 */
final class DotReporter extends TestExecutionListener {

    override def executionFinished(testIdentifier: TestIdentifier, result: TestExecutionResult): Unit = {
        if testIdentifier.isTest then {
            print(result.getStatus match {
                case TestExecutionResult.Status.SUCCESSFUL => "."
                case TestExecutionResult.Status.ABORTED => "A"
                case TestExecutionResult.Status.FAILED => "E"
            })
        }
    }

    override def testPlanExecutionFinished(testPlan: TestPlan) = {
        println
    }

}
