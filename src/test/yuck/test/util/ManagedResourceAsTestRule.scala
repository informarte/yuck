package yuck.test.util

import org.junit.rules.ExternalResource

import yuck.util.arm.ManagedResource

/**
 * @author Michael Marte
 *
 */
class ManagedResourceAsTestRule(resourceFactory: => ManagedResource) extends ExternalResource {
    private lazy val resource = resourceFactory
    override def before() = resource.open()
    override def after() = resource.close()
}
