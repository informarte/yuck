package yuck.util.alg.rtree.test

import com.conversantmedia.util.collection.geometry.Rect2d
import com.conversantmedia.util.collection.spatial.SpatialSearches

import org.junit.*

import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class RTreeIntakeTest extends UnitTest {

    private val rTree = SpatialSearches.rTree[Rect2d](new Rect2d.Builder)

    // When a rectangle is inserted n times, the R tree will contain it n times.

    @Test
    def testDoubleInsertion1: Unit = {
        val r = new Rect2d(0, 0, 7, 7)
        rTree.add(r)
        rTree.add(r)
        assertEq(rTree.getEntryCount, 2)
    }

    @Test
    def testDoubleInsertion2: Unit = {
        val r1 = new Rect2d(0, 0, 7, 7)
        val r2 = new Rect2d(0, 0, 7, 7)
        rTree.add(r1)
        rTree.add(r2)
        assertEq(rTree.getEntryCount, 2)
    }

    // On deletion, however, all identical rectangles will be deleted.

    @Test
    def testAmbiguousDeletion1: Unit = {
        val r = new Rect2d(0, 0, 7, 7)
        rTree.add(r)
        rTree.add(r)
        rTree.remove(r)
        assertEq(rTree.getEntryCount, 0)
    }

    @Test
    def testAmbiguousDeletion2: Unit = {
        val r1 = new Rect2d(0, 0, 7, 7)
        val r2 = new Rect2d(0, 0, 7, 7)
        rTree.add(r1)
        rTree.add(r2)
        rTree.remove(r1)
        assertEq(rTree.getEntryCount, 0)
    }

    @Test
    def testContains: Unit = {
        val r1 = new Rect2d(0, 0, 7, 7)
        val r2 = new Rect2d(0, 0, 8, 8)
        rTree.add(r2)
        assert(! rTree.contains(r1))
        assert(rTree.contains(r2))
        rTree.add(r1)
        assert(rTree.contains(r1))
        assert(rTree.contains(r2))
    }

}
