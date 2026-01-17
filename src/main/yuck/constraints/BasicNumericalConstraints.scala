package yuck.constraints

import yuck.core.*

final class Plus
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: X)
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s + %s".format(z, x, y)
    override def op(a: A, b: A) = a + b
    override def propagate() = {
        import typeTraits.one
        val lhs0 = Seq((one, x.domain), (one, y.domain))
        val (lhs1, dz1) = typeTraits.domainPruner.linEqRule(lhs0, z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

final class Minus
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: X)
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s - %s".format(z, x, y)
    override def op(a: A, b: A) = a - b
    override def propagate() = {
        import typeTraits.{one, zero}
        val lhs0 = Seq((one, x.domain), (zero - one, y.domain))
        val (lhs1, dz1) = typeTraits.domainPruner.linEqRule(lhs0, z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

final class Times
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: X)
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s * %s".format(z, x, y)
    override def op(a: A, b: A) = a * b
    override def propagate() = {
        val (dx1, dy1, dz1) = typeTraits.domainPruner.timesRule(x.domain, y.domain, z.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

final class Div
    [A <: IntegralValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: X)
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s / %s".format(z, x, y)
    override def propagate() =
        NoPropagationOccurred.pruneDomain(y, y.domain.diff(typeTraits.createDomain(Set(typeTraits.zero))))
    override def op(a: A, b: A) =
        // When y is a channel variable, b may be zero!
        // Nevertheless, we have to provide some value for z.
       if b == typeTraits.zero then a else a / b
}

final class Mod
    [A <: IntegralValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: X)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s %% %s".format(z, x, y)
    override def op(a: A, b: A) = a % b
}

final class Power
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: X)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s ^ %s".format(z, x, y)
    override def op(a: A, b: A) = a ^ b
}

final class Abs
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X)
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends BinaryConstraint(id, x, y)
{
    override def toString = "%s = abs(%s)".format(y, x)
    override def op(a: A) = a.abs
    override def propagate() = {
        val (dx1, dy1) = typeTraits.domainPruner.absRule(x.domain, y.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1)
    }
}

final class Even
    [A <: IntegralValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: BooleanVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "even(%s, %s)".format(x, y)
    override def op(a: A) = if a.isEven then True else False
}

final class Uneven
    [A <: IntegralValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: BooleanVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "uneven(%s, %s)".format(x, y)
    override def op(a: A) = if a.isEven then False else True
}
