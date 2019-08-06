package yuck.core

import scala.math.{ceil, floor}

import yuck.core.IntegerDomain.createRange

/**
 * Provides methods for pruning integer domains.
 *
 * @author Michael Marte
 */
final object IntegerDomainPruner extends NumericalDomainPruner[IntegerValue] {

    type DomainImpl = IntegerDomain

    override def eq
        [Domain >: DomainImpl <: yuck.core.Domain[IntegerValue]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
    {
        val intersection = lhs.asInstanceOf[DomainImpl].intersect(rhs)
        (intersection, intersection)
    }

    override def ne
        [Domain >: DomainImpl <: yuck.core.Domain[IntegerValue]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
    {
        (if (rhs.isSingleton) lhs.asInstanceOf[DomainImpl].diff(rhs) else lhs,
         if (lhs.isSingleton) rhs.asInstanceOf[DomainImpl].diff(lhs) else rhs)
    }

    override def lt
        [Domain >: DomainImpl <: OrderedDomain[IntegerValue]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
    {
        if (lhs.isEmpty || rhs.isEmpty) (EmptyIntegerRange, EmptyIntegerRange)
        else (createRange(null, if (! rhs.hasUb) null else rhs.ub - One).intersect(lhs),
              createRange(if (! lhs.hasLb) null else lhs.lb + One, null).intersect(rhs))
    }

    override def le
        [Domain >: DomainImpl <: OrderedDomain[IntegerValue]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
    {
        if (lhs.isEmpty || rhs.isEmpty) (EmptyIntegerRange, EmptyIntegerRange)
        else (createRange(null, rhs.ub).intersect(lhs), createRange(lhs.lb, null).intersect(rhs))
    }

    override def min
        [Domain >: DomainImpl <: OrderedDomain[IntegerValue]]
        (lhs0: Iterable[Domain], rhs0: Domain):
        (TraversableOnce[Domain], Domain) =
    {
        require(! lhs0.isEmpty)
        if (rhs0.isEmpty || lhs0.exists(_.isEmpty)) {
            (for (_ <- lhs0.iterator) yield EmptyIntegerRange, EmptyIntegerRange)
        } else {
            val lhs1 = lhs0.iterator.map(d => le(rhs0, d)._2)
            val minLb = lhs0.iterator.filter(_.hasLb).map(_.lb).reduceLeftOption((a, b) => if (a < b) a else b)
            val minUb = lhs0.iterator.filter(_.hasUb).map(_.ub).reduceLeftOption((a, b) => if (a < b) a else b)
            val rhs1 = createRange(minLb.orNull, minUb.orNull).intersect(rhs0)
            (lhs1, rhs1)
        }
    }

    override def max
        [Domain >: DomainImpl <: OrderedDomain[IntegerValue]]
        (lhs0: Iterable[Domain], rhs0: Domain):
        (TraversableOnce[Domain], Domain) =
    {
        require(! lhs0.isEmpty)
        if (rhs0.isEmpty || lhs0.exists(_.isEmpty)) {
            (for (_ <- lhs0.iterator) yield EmptyIntegerRange, EmptyIntegerRange)
        } else {
            val lhs1 = lhs0.iterator.map(d => le[Domain](d, rhs0)._1)
            val maxLb = lhs0.iterator.filter(_.hasLb).map(_.lb).reduceLeftOption((a, b) => if (a > b) a else b)
            val maxUb = lhs0.iterator.filter(_.hasUb).map(_.ub).reduceLeftOption((a, b) => if (a > b) a else b)
            val rhs1 = createRange(maxLb.orNull, maxUb.orNull).intersect(rhs0)
            (lhs1, rhs1)
        }
    }

    override def linEq
        [Domain >: DomainImpl <: NumericalDomain[IntegerValue]]
        (lhs0: Iterable[(IntegerValue, Domain)], rhs0: Domain):
        (TraversableOnce[Domain], Domain) =
    {
        //     sum a_i * x_i  = b
        // <-> sum a_i * x_i <= b & sum  a_i * x_i >=  b
        // <-> sum a_i * x_i <= b & sum -a_i * x_i <= -b
        def negativeValue(a: IntegerValue): IntegerValue =
            IntegerValue.get(safeNeg(a.value))
        def mirrorRange(range: NumericalDomain[IntegerValue]): NumericalDomain[IntegerValue] =
            if (range.isEmpty) range
            else createRange(range.maybeUb.map(negativeValue).orNull, range.maybeLb.map(negativeValue).orNull)
        val (lhs1, rhs1) = linLe(lhs0, rhs0)
        val (lhs2, rhs2) = linLe(lhs0.map{case (a, d) => (negativeValue(a), d)}, mirrorRange(rhs0.hull))
        val lhs3 = for ((d, e) <- lhs1.iterator.zip(lhs2.iterator)) yield d.asInstanceOf[DomainImpl].intersect(e)
        val rhs3 = rhs1.asInstanceOf[DomainImpl].intersect(mirrorRange(rhs2))
        (lhs3, rhs3)
    }

    // We follow K. R. Apt, Principles of Constraint Programming, p. 194.
    // This code implements rule LINEAR_EQUALITY 1 with extensions to prune rhs.
    // Does not compute a fixed point!
    private def linLe
        [Domain >: DomainImpl <: NumericalDomain[IntegerValue]]
        (lhs0: Iterable[(IntegerValue, Domain)], rhs0: Domain):
        (TraversableOnce[Domain], Domain) =
    {
        if (rhs0.isEmpty || lhs0.exists{case (_, d) => d.isEmpty}) {
            (for (_ <- lhs0.iterator) yield EmptyIntegerRange, EmptyIntegerRange)
        } else if (lhs0.forall{case (a, d) => if (a.value >= 0) d.hasLb else d.hasUb}) {
            val lhs1 =
                if (rhs0.hasUb) {
                    lazy val posTerm = lhs0.foldLeft(0){case (sum, (a, d)) => safeAdd(sum, if (a.value >= 0) safeMul(a.value, d.lb.value) else 0)}
                    lazy val negTerm = lhs0.foldLeft(0){case (sum, (a, d)) => safeAdd(sum, if (a.value < 0) safeMul(safeNeg(a.value), d.ub.value) else 0)}
                    for ((a, d) <- lhs0.iterator) yield {
                        if (a.value >= 0) {
                            val alpha = safeAdd(safeSub(rhs0.ub.value, safeSub(posTerm, a.value * d.lb.value)), negTerm).toDouble / a.value
                            createRange(null, IntegerValue.get(floor(alpha).toInt)).intersect(d)
                        } else {
                            // In the book, a is positive, but here it is negative, so we have to use -a!
                            val beta = safeSub(safeAdd(safeNeg(rhs0.ub.value), posTerm), safeSub(negTerm, -a.value * d.ub.value)).toDouble / -a.value
                            createRange(IntegerValue.get(ceil(beta).toInt), null).intersect(d)
                        }
                    }
                } else {
                    lhs0.iterator.map(_._2)
                }
            val lhs0Lb = lhs0.foldLeft(0){case (sum, (a, d)) => safeAdd(sum, safeMul(a.value, if (a.value >= 0) d.lb.value else d.ub.value))}
            val rhs1 = createRange(IntegerValue.get(lhs0Lb), null).intersect(rhs0)
            (lhs1, rhs1)
        } else {
            (lhs0.iterator.map(_._2), rhs0)
        }
    }

    // We follow K. R. Apt, Principles of Constraint Programming, p. 217.
    override def times
        [Domain >: DomainImpl <: NumericalDomain[IntegerValue]]
        (dx0: Domain, dy0: Domain, dz0: Domain):
        (Domain, Domain, Domain) =
    {
        val dx1 = dx0.asInstanceOf[DomainImpl]
        val dy1 = dy0.asInstanceOf[DomainImpl]
        val dz1 = dz0.asInstanceOf[DomainImpl]
        // MULTIPLICATION 1
        val dz2 =
            if (dx1.isFinite && dy1.isFinite) {
                dz1.intersect(dx1.hull.mult(dy1.hull))
            } else {
                dz1
            }
        // MULTIPLICATION 2
        val dx2 =
            if (dy1.isFinite && dz1.isFinite) {
                dx1.intersect(dz1.hull.div(dy1.hull))
            } else {
                dx1
            }
        // MULTIPLICATION 3
        val dy2 =
            if (dx1.isFinite && dz1.isFinite) {
                dy1.intersect(dz1.hull.div(dx1.hull))
            } else {
                dy1
            }
        (dx2, dy2, dz2)
    }

}
