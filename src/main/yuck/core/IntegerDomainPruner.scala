package yuck.core

import java.lang.Math.{ceil, floor}

/**
 * Provides methods for pruning integer domains.
 *
 * @author Michael Marte
 */
object IntegerDomainPruner extends NumericalDomainPruner[IntegerValue] {

    override protected val valueTraits = IntegerValueTraits

    override def eqRule
        (lhs: Domain[IntegerValue], rhs: Domain[IntegerValue]):
        (IntegerDomain, IntegerDomain) =
    {
        val intersection = lhs.asInstanceOf[IntegerDomain].intersect(rhs)
        (intersection, intersection)
    }

    override def neRule
        (lhs0: Domain[IntegerValue], rhs0: Domain[IntegerValue]):
        (IntegerDomain, IntegerDomain) =
    {
        val lhs1 = lhs0.asInstanceOf[IntegerDomain]
        val rhs1 = rhs0.asInstanceOf[IntegerDomain]
        (if (rhs1.isSingleton) lhs1.diff(rhs1) else lhs1,
         if (lhs1.isSingleton) rhs1.diff(lhs1) else rhs1)
    }

    override def ltRule
        (lhs: OrderedDomain[IntegerValue], rhs: OrderedDomain[IntegerValue]):
        (IntegerDomain, IntegerDomain) =
    {
        if (lhs.isEmpty || rhs.isEmpty) (EmptyIntegerRange, EmptyIntegerRange)
        else (IntegerRange(null, if (! rhs.hasUb) null else rhs.ub - One).intersect(lhs),
              IntegerRange(if (! lhs.hasLb) null else lhs.lb + One, null).intersect(rhs))
    }

    override def leRule
        (lhs: OrderedDomain[IntegerValue], rhs: OrderedDomain[IntegerValue]):
        (IntegerDomain, IntegerDomain) =
    {
        if (lhs.isEmpty || rhs.isEmpty) (EmptyIntegerRange, EmptyIntegerRange)
        else (IntegerRange(null, rhs.ub).intersect(lhs), IntegerRange(lhs.lb, null).intersect(rhs))
    }

    override def minRule
        (lhs0: Iterable[OrderedDomain[IntegerValue]], rhs0: OrderedDomain[IntegerValue]):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        require(! lhs0.isEmpty)
        if (rhs0.isEmpty || lhs0.exists(_.isEmpty)) {
            (for (_ <- lhs0.iterator) yield EmptyIntegerRange, EmptyIntegerRange)
        } else {
            val lhs1 = lhs0.iterator.map(d => leRule(rhs0, d)._2)
            val maybeMinLb = lhs0.iterator.filter(_.hasLb).map(_.lb).reduceLeftOption((a, b) => if (a < b) a else b)
            val maybeMinUb = lhs0.iterator.filter(_.hasUb).map(_.ub).reduceLeftOption((a, b) => if (a < b) a else b)
            val rhs1 = IntegerRange(maybeMinLb.orNull, maybeMinUb.orNull).intersect(rhs0)
            (lhs1, rhs1)
        }
    }

    override def maxRule
        (lhs0: Iterable[OrderedDomain[IntegerValue]], rhs0: OrderedDomain[IntegerValue]):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        require(! lhs0.isEmpty)
        if (rhs0.isEmpty || lhs0.exists(_.isEmpty)) {
            (for (_ <- lhs0.iterator) yield EmptyIntegerRange, EmptyIntegerRange)
        } else {
            val lhs1 = lhs0.iterator.map(d => leRule(d, rhs0)._1)
            val maybeMaxLb = lhs0.iterator.filter(_.hasLb).map(_.lb).reduceLeftOption((a, b) => if (a > b) a else b)
            val maybeMaxUb = lhs0.iterator.filter(_.hasUb).map(_.ub).reduceLeftOption((a, b) => if (a > b) a else b)
            val rhs1 = IntegerRange(maybeMaxLb.orNull, maybeMaxUb.orNull).intersect(rhs0)
            (lhs1, rhs1)
        }
    }

    override def absRule
        (lhs0: NumericalDomain[IntegerValue], rhs0: NumericalDomain[IntegerValue]):
        (IntegerDomain, IntegerDomain) =
    {
        val lhs1 = lhs0.asInstanceOf[IntegerDomain]
        val rhs1 = NonNegativeIntegerRange.intersect(rhs0)
        val lhs2 = lhs1.intersect(rhs1.union(rhs1.mirrored))
        val rhs2 = rhs1.intersect(lhs1.union(lhs1.mirrored))
        (lhs2, rhs2)
    }

    override def linEqRule
        (lhs: Iterable[(IntegerValue, NumericalDomain[IntegerValue])], rhs: NumericalDomain[IntegerValue]):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        try {
            unsafeLinEqRule(lhs, rhs)
        }
        catch {
            case _: ArithmeticException =>
                (lhs.iterator.map(_._2.asInstanceOf[IntegerDomain]), rhs.asInstanceOf[IntegerDomain])
        }
    }

    private def unsafeLinEqRule
        (lhs0: Iterable[(IntegerValue, NumericalDomain[IntegerValue])], rhs0: NumericalDomain[IntegerValue]):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        //     sum a_i * x_i  = b
        // <-> sum a_i * x_i <= b & sum  a_i * x_i >=  b
        // <-> sum a_i * x_i <= b & sum -a_i * x_i <= -b
        val (lhs1, rhs1) = linLeRule(lhs0, rhs0)
        val (lhs2, rhs2) = linLeRule(lhs0.map{case (a, d) => (a.negated, d)}, rhs0.hull.mirrored)
        val lhs3 = for ((d, e) <- lhs1.iterator.zip(lhs2.iterator)) yield d.intersect(e)
        val rhs3 = rhs1.intersect(rhs2.mirrored)
        (lhs3, rhs3)
    }

    private def linLeRule
        (lhs: Iterable[(IntegerValue, NumericalDomain[IntegerValue])], rhs: NumericalDomain[IntegerValue]):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        try {
            unsafeLinLeRule(lhs, rhs)
        }
        catch {
            case _: ArithmeticException =>
                (lhs.iterator.map(_._2.asInstanceOf[IntegerDomain]), rhs.asInstanceOf[IntegerDomain])
        }
    }

    // We follow K. R. Apt, Principles of Constraint Programming, p. 194.
    // This code implements rule LINEAR_EQUALITY 1 with extensions to prune rhs.
    // Does not compute a fixed point!
    private def unsafeLinLeRule
        (lhs0: Iterable[(IntegerValue, NumericalDomain[IntegerValue])], rhs0: NumericalDomain[IntegerValue]):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        if (rhs0.isEmpty || lhs0.exists{case (_, d) => d.isEmpty}) {
            (for (_ <- lhs0.iterator) yield EmptyIntegerRange, EmptyIntegerRange)
        } else if (lhs0.forall{case (a, d) => if (a.value >= 0) d.hasLb else d.hasUb}) {
            val lhs1 =
                if (rhs0.hasUb) {
                    lazy val posTerm = lhs0.foldLeft(0){case (sum, (a, d)) => safeAdd(sum, if (a.value >= 0) safeMul(a.value, d.lb.value) else 0)}
                    lazy val negTerm = lhs0.foldLeft(0){case (sum, (a, d)) => safeAdd(sum, if (a.value < 0) safeMul(safeNeg(a.value), d.ub.value) else 0)}
                    for ((a, d) <- lhs0.iterator) yield {
                        if (a.value > 0) {
                            val alpha = safeAdd(safeSub(rhs0.ub.value, safeSub(posTerm, a.value * d.lb.value)), negTerm).toDouble / a.value
                            IntegerRange(null, IntegerValue(floor(alpha).toInt)).intersect(d)
                        } else if (a.value < 0) {
                            // In the book, a is positive, but here it is negative, so we have to use -a!
                            val beta = safeSub(safeAdd(safeNeg(rhs0.ub.value), posTerm), safeSub(negTerm, safeNeg(a.value) * d.ub.value)).toDouble / safeNeg(a.value)
                            IntegerRange(IntegerValue(ceil(beta).toInt), null).intersect(d)
                        } else {
                            d.asInstanceOf[IntegerDomain]
                        }
                    }
                } else {
                    lhs0.iterator.map(_._2.asInstanceOf[IntegerDomain])
                }
            val lhs0Lb = lhs0.foldLeft(0){case (sum, (a, d)) => safeAdd(sum, safeMul(a.value, if (a.value >= 0) d.lb.value else d.ub.value))}
            val rhs1 = IntegerRange(IntegerValue(lhs0Lb), null).intersect(rhs0)
            (lhs1, rhs1)
        } else {
            (lhs0.iterator.map(_._2.asInstanceOf[IntegerDomain]), rhs0.asInstanceOf[IntegerDomain])
        }
    }

    // We follow K. R. Apt, Principles of Constraint Programming, p. 217.
    override def timesRule
        (dx0: NumericalDomain[IntegerValue], dy0: NumericalDomain[IntegerValue], dz0: NumericalDomain[IntegerValue]):
        (IntegerDomain, IntegerDomain, IntegerDomain) =
    {
        val dx1 = dx0.asInstanceOf[IntegerDomain]
        val dy1 = dy0.asInstanceOf[IntegerDomain]
        val dz1 = dz0.asInstanceOf[IntegerDomain]
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
