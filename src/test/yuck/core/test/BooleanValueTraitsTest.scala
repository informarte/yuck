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
final class BooleanValueTraitsTest extends UnitTest {

    import BooleanValueTraits._

    @Test
    def testSpecialDomains {
        assertEq(emptyDomain, EmptyBooleanDomain)
        assertEq(completeDomain, CompleteBooleanDomain)
    }

    @Test
    def testDomainFactories {
        assertEq(createDomain(Set()), EmptyBooleanDomain)
        assertEq(createDomain(Set(False)), FalseDomain)
        assertEq(createDomain(Set(True)), TrueDomain)
        assertEq(createDomain(Set(False, True)), CompleteBooleanDomain)
        assertEq(createDomain(False, False), FalseDomain)
        assertEq(createDomain(False, True), CompleteBooleanDomain)
        assertEq(createDomain(True, False), EmptyBooleanDomain)
        assertEq(createDomain(True, True), TrueDomain)
    }

    @Test
    def testCasting {
        safeDowncast(False)
        assertEx(safeDowncast(Zero))
        safeDowncast(CompleteBooleanDomain)
        assertEx(safeDowncast(CompleteIntegerRange))
    }

}
