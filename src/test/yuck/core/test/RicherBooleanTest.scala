package yuck.core.test

import org.junit._

import scala.collection._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class RicherBooleanTest extends UnitTest {

    @Test
    def testStrictDisjunction: Unit = {
        for ((a, b) <- List((false, false), (false, true), (true, false), (true, true))) {
            val buf = new mutable.HashSet[String]
            assertEq({buf += "a"; a} ||| {buf += "b"; b}, a || b)
            assertEq(buf, Set("a", "b"))
        }
    }

}
