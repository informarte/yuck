package yuck.core

import java.lang.Math.{ceil, floor}

/**
 * Provides methods for pruning integer domains.
 */
object IntegerDomainPruner extends NumericalDomainPruner[IntegerValue, IntegerDomain] {

    override protected val typeTraits = IntegerTypeTraits

    override def eqRule
        (lhs: IntegerDomain, rhs: IntegerDomain):
        (IntegerDomain, IntegerDomain) =
    {
        val intersection = lhs.intersect(rhs)
        (intersection, intersection)
    }

    override def neRule
        (lhs: IntegerDomain, rhs: IntegerDomain):
        (IntegerDomain, IntegerDomain) =
    {
        (if rhs.isSingleton then lhs.diff(rhs) else lhs,
         if lhs.isSingleton then rhs.diff(lhs) else rhs)
    }

    override def ltRule
        (lhs: IntegerDomain, rhs: IntegerDomain):
        (IntegerDomain, IntegerDomain) =
    {
        if lhs.isEmpty || rhs.isEmpty
        then (EmptyIntegerRange, EmptyIntegerRange)
        else (IntegerRange(null, if ! rhs.hasUb then null else rhs.ub - One).intersect(lhs),
              IntegerRange(if ! lhs.hasLb then null else lhs.lb + One, null).intersect(rhs))
    }

    override def leRule
        (lhs: IntegerDomain, rhs: IntegerDomain):
        (IntegerDomain, IntegerDomain) =
    {
        if lhs.isEmpty || rhs.isEmpty
        then (EmptyIntegerRange, EmptyIntegerRange)
        else (IntegerRange(null, rhs.ub).intersect(lhs), IntegerRange(lhs.lb, null).intersect(rhs))
    }

    override def minRule
        (lhs0: Iterable[IntegerDomain], rhs0: IntegerDomain):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        require(! lhs0.isEmpty)
        if rhs0.isEmpty || lhs0.exists(_.isEmpty) then {
            (for _ <- lhs0.iterator yield EmptyIntegerRange, EmptyIntegerRange)
        } else {
            val lhs1 = lhs0.iterator.map(d => leRule(rhs0, d)._2)
            val maybeMinLb = lhs0.iterator.filter(_.hasLb).map(_.lb).reduceLeftOption((a, b) => if a < b then a else b)
            val maybeMinUb = lhs0.iterator.filter(_.hasUb).map(_.ub).reduceLeftOption((a, b) => if a < b then a else b)
            val rhs1 = IntegerRange(maybeMinLb.orNull, maybeMinUb.orNull).intersect(rhs0)
            (lhs1, rhs1)
        }
    }

    override def maxRule
        (lhs0: Iterable[IntegerDomain], rhs0: IntegerDomain):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        require(! lhs0.isEmpty)
        if rhs0.isEmpty || lhs0.exists(_.isEmpty) then {
            (for _ <- lhs0.iterator yield EmptyIntegerRange, EmptyIntegerRange)
        } else {
            val lhs1 = lhs0.iterator.map(d => leRule(d, rhs0)._1)
            val maybeMaxLb = lhs0.iterator.filter(_.hasLb).map(_.lb).reduceLeftOption((a, b) => if a > b then a else b)
            val maybeMaxUb = lhs0.iterator.filter(_.hasUb).map(_.ub).reduceLeftOption((a, b) => if a > b then a else b)
            val rhs1 = IntegerRange(maybeMaxLb.orNull, maybeMaxUb.orNull).intersect(rhs0)
            (lhs1, rhs1)
        }
    }

    override def absRule
        (lhs0: IntegerDomain, rhs0: IntegerDomain):
        (IntegerDomain, IntegerDomain) =
    {
        val lhs1 = lhs0
        val rhs1 = NonNegativeIntegerRange.intersect(rhs0)
        val lhs2 = lhs1.intersect(rhs1.union(rhs1.mirrored))
        val rhs2 = rhs1.intersect(lhs1.union(lhs1.mirrored))
        (lhs2, rhs2)
    }

    override def linEqRule
        (lhs: Iterable[(IntegerValue, IntegerDomain)], rhs: IntegerDomain):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        try {
            unsafeLinEqRule(lhs, rhs)
        }
        catch {
            case _: ArithmeticException => (lhs.iterator.map(_._2), rhs)
        }
    }

    private def unsafeLinEqRule
        (lhs0: Iterable[(IntegerValue, IntegerDomain)], rhs0: IntegerDomain):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        //     sum a_i * x_i  = b
        // <-> sum a_i * x_i <= b & sum  a_i * x_i >=  b
        // <-> sum a_i * x_i <= b & sum -a_i * x_i <= -b
        val (lhs1, rhs1) = linLeRule(lhs0, rhs0)
        val (lhs2, rhs2) = linLeRule(lhs0.map((a, d) => (a.negated, d)), rhs0.hull.mirrored)
        val lhs3 = for (d, e) <- lhs1.iterator.zip(lhs2.iterator) yield d.intersect(e)
        val rhs3 = rhs1.intersect(rhs2.mirrored)
        (lhs3, rhs3)
    }

    private def linLeRule
        (lhs: Iterable[(IntegerValue, IntegerDomain)], rhs: IntegerDomain):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        try {
            unsafeLinLeRule(lhs, rhs)
        }
        catch {
            case _: ArithmeticException => (lhs.iterator.map(_._2), rhs)
        }
    }

    // We follow K. R. Apt, Principles of Constraint Programming, p. 194.
    // This code implements rule LINEAR_EQUALITY 1 with extensions to prune rhs.
    // Does not compute a fixed point!
    private def unsafeLinLeRule
        (lhs0: Iterable[(IntegerValue, IntegerDomain)], rhs0: IntegerDomain):
        (Iterator[IntegerDomain], IntegerDomain) =
    {
        if rhs0.isEmpty || lhs0.exists((_, d) => d.isEmpty) then {
            (for _ <- lhs0.iterator yield EmptyIntegerRange, EmptyIntegerRange)
        } else if lhs0.forall((a, d) => if a.value >= 0 then d.hasLb else d.hasUb) then {
            val lhs1 =
                if rhs0.hasUb then {
                    lazy val posTerm = lhs0.foldLeft(0L){case (sum, (a, d)) => safeAdd(sum, if a.value >= 0 then safeMul(a.value, d.lb.value) else 0)}
                    lazy val negTerm = lhs0.foldLeft(0L){case (sum, (a, d)) => safeAdd(sum, if a.value < 0 then safeMul(safeNeg(a.value), d.ub.value) else 0)}
                    for (a, d) <- lhs0.iterator yield {
                        if a.value > 0 then {
                            val alpha = safeAdd(safeSub(rhs0.ub.value, safeSub(posTerm, a.value * d.lb.value)), negTerm).toDouble / a.value
                            IntegerRange(null, IntegerValue(floor(alpha).toInt)).intersect(d)
                        } else if a.value < 0 then {
                            // In the book, a is positive, but here it is negative, so we have to use -a!
                            val beta = safeSub(safeAdd(safeNeg(rhs0.ub.value), posTerm), safeSub(negTerm, safeNeg(a.value) * d.ub.value)).toDouble / safeNeg(a.value)
                            IntegerRange(IntegerValue(ceil(beta).toInt), null).intersect(d)
                        } else {
                            d
                        }
                    }
                } else {
                    lhs0.iterator.map(_._2)
                }
            val lhs0Lb = lhs0.foldLeft(0L){case (sum, (a, d)) => safeAdd(sum, safeMul(a.value, if a.value >= 0 then d.lb.value else d.ub.value))}
            val rhs1 = IntegerRange(IntegerValue(lhs0Lb), null).intersect(rhs0)
            (lhs1, rhs1)
        } else {
            (lhs0.iterator.map(_._2), rhs0)
        }
    }

    // We follow K. R. Apt, Principles of Constraint Programming, p. 217.
    override def timesRule
        (dx0: IntegerDomain, dy0: IntegerDomain, dz0: IntegerDomain):
        (IntegerDomain, IntegerDomain, IntegerDomain) =
    {
        // MULTIPLICATION 1
        val dz1 =
            if dx0.isFinite && dy0.isFinite
            then dz0.intersect(dx0.hull.mult(dy0.hull))
            else dz0
        // MULTIPLICATION 2
        val dx1 =
            if dy0.isFinite && dz0.isFinite
            then dx0.intersect(dz0.hull.div(dy0.hull))
            else dx0
        // MULTIPLICATION 3
        val dy1 =
            if dx0.isFinite && dz0.isFinite
            then dy0.intersect(dz0.hull.div(dx0.hull))
            else dy0
        (dx1, dy1, dz1)
    }

}
