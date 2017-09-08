package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetDomainTest extends UnitTest {

    @Test
    def testCasting {
        assertEx(IntegerSetValueTraits.dynamicDowncast(UnboundedIntegerDomain))
    }

}
