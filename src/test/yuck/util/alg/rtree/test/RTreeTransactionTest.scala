package yuck.util.alg.rtree.test

import com.conversantmedia.util.collection.geometry.Rect2d
import com.conversantmedia.util.collection.spatial.SpatialSearches

import org.junit.*

import yuck.test.util.UnitTest
import yuck.util.alg.rtree.RTreeTransaction

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class RTreeTransactionTest extends UnitTest {

    private val rectBuilder = new Rect2d.Builder
    private val rTree = SpatialSearches.rTree[Rect2d](rectBuilder)
    private val rTreeTransaction = new RTreeTransaction[Rect2d](rTree, rectBuilder)
    private val r1 = new Rect2d(0, 0, 1, 1)
    private val r2 = new Rect2d(1, 1, 2, 2)

    @Test
    def testAdd: Unit = {
        rTreeTransaction.add(r1)
        rTreeTransaction.add(r2)
        rTreeTransaction.commit
        assert(rTree.contains(r1))
        assert(rTree.contains(r2))
        assertEq(rTree.getEntryCount, 2)
    }

    @Test
    def testRemove: Unit = {
        rTree.add(r1)
        rTree.add(r2)
        rTreeTransaction.remove(r1)
        rTreeTransaction.commit
        assert(! rTree.contains(r1))
        assert(rTree.contains(r2))
        assertEq(rTree.getEntryCount, 1)
    }

    @Test
    def testAddAndRemove: Unit = {
        rTreeTransaction.add(r1)
        rTreeTransaction.add(r2)
        rTreeTransaction.remove(r1)
        rTreeTransaction.commit
        assert(! rTree.contains(r1))
        assert(rTree.contains(r2))
        assertEq(rTree.getEntryCount, 1)
    }

    @Test
    def testRemoveAndAdd: Unit = {
        rTree.add(r1)
        rTreeTransaction.remove(r1)
        rTreeTransaction.add(r1)
        rTreeTransaction.add(r2)
        rTreeTransaction.commit
        assert(rTree.contains(r1))
        assert(rTree.contains(r2))
        assertEq(rTree.getEntryCount, 2)
    }

    @Test
    def testRollback: Unit = {
        rTree.add(r1)
        rTreeTransaction.remove(r1)
        rTreeTransaction.add(r2)
        rTreeTransaction.rollback
        rTreeTransaction.commit
        assert(rTree.contains(r1))
        assertEq(rTree.getEntryCount, 1)
    }

}
