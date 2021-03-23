package yuck.test.util

import org.junit.rules.TestRule
import org.junit.runner.Description
import org.junit.runners.model.Statement

import yuck.util.arm.{ManagedResource, scoped}

/**
 * @author Michael Marte
 *
 */
class ManagedResourceAsTestRule(resourceFactory: => ManagedResource) extends TestRule {

    override def apply(base: Statement, description: Description) =
        new Statement {
            override def evaluate =
                scoped(resourceFactory) {
                    base.evaluate()
                }
    }

}
