package yuck.test.util

import scala.compiletime.uninitialized

import org.junit.jupiter.api.extension.{AfterEachCallback, BeforeEachCallback, ExtensionContext}

import yuck.util.arm.ManagedResource

final class ManagedResourceAsExtension(resourceFactory: ExtensionContext => ManagedResource)
    extends BeforeEachCallback
       with AfterEachCallback
{

    private var resource: ManagedResource = uninitialized

    override def beforeEach(context: ExtensionContext) = {
        resource = resourceFactory(context)
        resource.open()
    }

    override def afterEach(context: ExtensionContext) = {
        resource.close()
    }

}
