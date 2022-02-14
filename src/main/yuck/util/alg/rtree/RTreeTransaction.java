package yuck.util.alg.rtree;

import com.conversantmedia.util.collection.spatial.HyperRect;
import com.conversantmedia.util.collection.spatial.RectBuilder;
import com.conversantmedia.util.collection.spatial.SpatialSearch;
import com.conversantmedia.util.collection.spatial.Stats;

import java.util.Collection;
import java.util.HashSet;
import java.util.function.Consumer;

/**
 * Facilitates the simulation of small changes to a given R tree.
 *
 * @author Michael Marte
 */
final public class RTreeTransaction<T> implements SpatialSearch<T> {

    final SpatialSearch<T> rTree;
    final RectBuilder<T> builder;
    final HashSet<T> added = new HashSet<T>();
    final HashSet<T> removed = new HashSet<T>();

    public RTreeTransaction(final SpatialSearch<T> rTree, final RectBuilder<T> builder) {
        this.rTree = rTree;
        this.builder = builder;
    }

    @Override
    public int intersects(final HyperRect rect, final T[] t) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void intersects(final HyperRect rect, final Consumer<T> consumer) {
        for (final T t : added) {
            if (rect.intersects(builder.getBBox(t))) {
                consumer.accept(t);
            }
        }
        rTree.intersects(
            rect,
            new Consumer<T>() {
                @Override
                public void accept(final T t) {
                    if (! removed.contains(t)) {
                        consumer.accept(t);
                    }
                }
            });
    }

    @Override
    public int search(final HyperRect rect, final T[] t) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void search(final HyperRect rect, final Consumer<T> consumer) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void search(final HyperRect rect, final Collection<T> collection) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean contains(final T t) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void add(final T t) {
        if (! removed.remove(t)) {
            added.add(t);
        }
    }

    @Override
    public void remove(final T t) {
        if (! added.remove(t)) {
            removed.add(t);
        }
    }

    @Override
    public void update(final T t1, final T t2) {
        remove(t1);
        add(t2);
    }

    @Override
    public void forEach(final Consumer<T> consumer) {
        for (final T t : added) {
            consumer.accept(t);
        }
        rTree.forEach(
            new Consumer<T>() {
                @Override
                public void accept(final T t) {
                    if (! removed.contains(t)) {
                        consumer.accept(t);
                    }
                }
            });
    }

    @Override
    public int getEntryCount() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Stats collectStats() {
        throw new UnsupportedOperationException();
    }

    public void rollback() {
        added.clear();
        removed.clear();
    }

    public void commit() {
        for (final T t: removed) {
            rTree.remove(t);
        }
        removed.clear();
        for (final T t: added) {
            rTree.add(t);
        }
        added.clear();
    }

}
