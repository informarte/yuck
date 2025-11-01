package yuck.util

import java.util.Arrays
import java.util.stream.Stream
import java.util.stream.IntStream

import scala.collection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

import it.unimi.dsi.fastutil.ints.{Int2ObjectOpenHashMap, IntOpenHashSet}
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap

import java.util.function.{Consumer, IntConsumer}

/**
 * Collection of extensions method with a focus on avoiding closures and integer boxing.
 *
 * @author Michael Marte
 *
 */
object Collections {

    type IntArraySeq = immutable.ArraySeq.ofInt

    object IntArraySeq {

        def apply(values: Int*): IntArraySeq = new IntArraySeq(values.toArray)

    }

    extension (stream: IntStream) {

        def fasterDistinct: IntStream = {
            val seen = new IntOpenHashSet
            stream.filter(seen.add)
        }

        inline def foreach(action: IntConsumer): Unit = stream.forEach(action)

        inline def toArraySeq: IntArraySeq = stream.toArray.toArraySeq

        def toMapUsingKeyGenerator[K <: AnyRef](key: Int => K): Object2IntOpenHashMap[K] = {
            val map = new Object2IntOpenHashMap[K]
            stream.forEach(i => map.put(key(i), i))
            map
        }

        def toMapUsingValueGenerator[V <: AnyRef](value: Int => V): Int2ObjectOpenHashMap[V] = {
            val map = new Int2ObjectOpenHashMap[V]
            stream.forEach(i => map.put(i, value(i)))
            map
        }

    }

    extension [T <: AnyRef](stream: Stream[T]) {

        inline def foreach(action: Consumer[T]): Unit = stream.forEach(action)

        inline def toArraySeq(implicit t: ClassTag[T]): immutable.ArraySeq[T] =
            new immutable.ArraySeq.ofRef(stream.toArray(n => Array.ofDim[T](n)))

    }

    extension (range: Range) {

        def stream: IntStream = {
            val start = range.start
            val end = range.end
            val step = range.step
            val isInclusive = range.isInclusive
            if step == 1
            then if isInclusive
                 then IntStream.rangeClosed(start, end)
                 else IntStream.range(start, end)
            else {
                val limit =
                    (if step > 0 then (end - start) / step else (start - end) / -step) +
                        (if isInclusive then 1 else 0)
                IntStream.iterate(start, _ + step).limit(limit)
            }
        }

        inline def inlineForall(inline p: Int => Boolean): Boolean = {
            var i = range.start
            val end = range.end
            val step = range.step
            val isInclusive = range.isInclusive
            var result = true
            while (
                result &&
                (if step < 0
                 then if isInclusive then i >= end else i > end
                 else if isInclusive then i <= end else i < end))
            {
                result &= p(i)
                i += step
            }
            result
        }

        inline def inlineExists(inline p: Int => Boolean): Boolean = {
            var i = range.start
            val end = range.end
            val step = range.step
            val isInclusive = range.isInclusive
            var result = false
            while (
                ! result &&
                (if step < 0
                 then if isInclusive then i >= end else i > end
                 else if isInclusive then i <= end else i < end))
            {
                result |= p(i)
                i += step
            }
            result
        }

    }

    extension [T](array: Array[T]) {

        inline def inlineForeach(inline f: T => Unit): Unit = {
            var i = array.length
            while (i > 0) {
                i -= 1
                f(array(i))
            }
        }

        inline def inlineForeach(inline f: (Int, T) => Unit): Unit = {
            var i = array.length
            while (i > 0) {
                i -= 1
                f(i, array(i))
            }
        }

        inline def inlineForall(inline p: T => Boolean): Boolean = {
            var i = array.length
            var result = true
            while (result && i > 0) {
                i -= 1
                result &= p(array(i))
            }
            result
        }

        inline def inlineExists(inline p: T => Boolean): Boolean = {
            var i = array.length
            var result = false
            while (! result && i > 0) {
                i -= 1
                result |= p(array(i))
            }
            result
        }

    }

    extension (array: Array[Int]) {

        inline def toArraySeq: IntArraySeq = new IntArraySeq(array)

        inline def sortInPlace(): Array[Int] = {
            Arrays.sort(array)
            array
        }

        def isSorted: Boolean =
            Range.Exclusive(0, array.length - 1, 1).inlineForall(i => array(i) <= array(i + 1))

    }

    extension [T <: AnyRef](iterable: Iterable[T]) {

        def stream: Stream[T] = iterable.asJavaCollection.stream

    }

    extension [T <: AnyRef](array: IndexedSeq[T]) {

        def isSorted[A](s: IndexedSeq[A])(implicit ord: Ordering[A]): Boolean =
            (0 until s.length - 1).forall(i => ord.lteq(s(i), s(i + 1)))

        inline def inlineForeach(inline f: T => Unit): Unit = {
            var i = array.size
            while (i > 0) {
                i -= 1
                f(array(i))
            }
        }

        inline def inlineForeach(inline f: (Int, T) => Unit): Unit = {
            var i = array.size
            while (i > 0) {
                i -= 1
                f(i, array(i))
            }
        }

        inline def inlineForall(inline p: T => Boolean): Boolean = {
            var i = array.length
            var result = true
            while (result && i > 0) {
                i -= 1
                result &= p(array(i))
            }
            result
        }

        inline def inlineExists(inline p: T => Boolean): Boolean = {
            var i = array.length
            var result = false
            while (! result && i > 0) {
                i -= 1
                result |= p(array(i))
            }
            result
        }

    }

    extension (seq: IntArraySeq) {

        inline def stream: IntStream = Arrays.stream(seq.unsafeArray)

        inline def isSorted: Boolean = seq.unsafeArray.isSorted

        inline def fasterContains(i: Int): Boolean = Arrays.binarySearch(seq.unsafeArray, i) >= 0

        inline def inlineForeach(inline f: Int => Unit): Unit = seq.unsafeArray.inlineForeach(f)

        inline def inlineForeach(inline f: (Int, Int) => Unit): Unit = seq.unsafeArray.inlineForeach(f)

        inline def inlineForall(inline p: Int => Boolean): Boolean = seq.unsafeArray.inlineForall(p)

        inline def inlineExists(inline p: Int => Boolean): Boolean = seq.unsafeArray.inlineExists(p)

    }

}
