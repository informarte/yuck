package yuck.flatzinc.compiler

import scala.language.implicitConversions
import scala.collection._

import yuck.core._
import yuck.flatzinc.ast._
import yuck.flatzinc.parser._

/**
 * The given compilation context may be modified to transport information between phases.
 *
 * @author Michael Marte
 */
abstract class CompilationPhase(
    val cc: CompilationContext,
    val randomGenerator: RandomGenerator)
    extends Runnable
{

    protected final def compilesToConst(a: Expr): Boolean =
        a.isConst || cc.domains(a).isSingleton

    protected final def compilesToConst
        [Value <: AnyValue]
        (a: Expr, b: Value)
        (implicit valueTraits: AnyValueTraits[Value]): Boolean =
    {
        val maybeC = tryGetAnyConst(a)
        ! maybeC.isEmpty && valueTraits.dynamicCast(maybeC.get) == b
    }

    protected final def getConst
        [Value <: AnyValue]
        (a: Expr)
        (implicit valueTraits: AnyValueTraits[Value]): Value =
    {
        valueTraits.dynamicCast(getAnyConst(a))
    }

    protected final def getAnyConst(a: Expr): AnyValue =
        tryGetAnyConst(a).get

    private final def tryGetAnyConst(a: Expr): Option[AnyValue] = {
        a match {
            case BoolConst(a) => Some(if (a) True else False)
            case IntConst(a) => Some(IntegerValue.get(a))
            case IntSetConst(IntRange(lb, ub)) => Some(new IntegerSetValue(createIntegerDomain(lb, ub)))
            case IntSetConst(IntSet(set)) => Some(new IntegerSetValue(createIntegerDomain(set)))
            case _ if cc.domains(a).isSingleton => Some(cc.domains(a).singleValue)
            case _ => None
        }
    }

    protected final def getArrayElems(expr: Expr): immutable.Iterable[Expr] = expr match {
        case ArrayConst(elems) => elems
        case Term(id, Nil) if cc.ast.varDeclsByName.get(id).isDefined =>
            val decl = cc.ast.varDeclsByName(id)
            if (decl.optionalValue.isDefined) {
                val ArrayConst(elems) = decl.optionalValue.get
                elems
            } else {
                val ArrayType(Some(IntRange(1, n)), IntType(_)) = decl.varType
                (1 to n).toStream.map(i => ArrayAccess(id, IntConst(i)))
            }
        case Term(id, Nil) if cc.ast.paramDeclsByName.get(id).isDefined =>
            val ArrayConst(elems) = cc.ast.paramDeclsByName(id).value
            elems
    }

    protected final def createIntegerDomain(lb: Int, ub: Int): IntegerDomain =
        new IntegerDomain(IntegerValue.get(lb), IntegerValue.get(ub))

    protected final def createIntegerDomain(set: Set[Int]): IntegerDomain =
        new IntegerDomain(set.map(IntegerValue.get(_)))

    implicit protected final def compileArray
        [Value <: AnyValue]
        (expr: Expr)
        (implicit valueTraits: AnyValueTraits[Value]):
        immutable.IndexedSeq[Variable[Value]] =
    {
        valueTraits.dynamicCast(compileAnyArray(expr))
    }

    protected final def compileAnyArray(expr: Expr): immutable.IndexedSeq[AnyVariable] = expr match {
        case _ if cc.arrayConsts.get(expr).isDefined =>
            cc.arrayConsts(expr)
        case Term(id, Nil) =>
            cc.arrays.get(expr).get
        case ArrayConst(elems) =>
            val array = elems.toIterator.map(elem => compileAnyExpr(elem)).toIndexedSeq
            cc.arrayConsts += expr -> array
            array
    }

    implicit protected final def compileExpr
        [Value <: AnyValue]
        (expr: Expr)
        (implicit valueTraits: AnyValueTraits[Value]):
        Variable[Value] =
    {
        valueTraits.dynamicCast(compileAnyExpr(expr))
    }

    protected final def compileAnyExpr(expr: Expr): AnyVariable = expr match {
        case _ if cc.consts.get(expr).isDefined =>
            cc.consts(expr)
        case BoolConst(value) =>
            val domain = if (value) ZeroIntegerDomain else OneIntegerDomain
            val x = cc.space.createVariable(expr.toString, domain)
            cc.consts += expr -> x
            x
        case IntConst(value) =>
            val domain = createIntegerDomain(value, value)
            val x = cc.space.createVariable(expr.toString, domain)
            cc.consts += expr -> x
            x
        case IntSetConst(IntRange(lb, ub)) =>
            val domain = new SingletonIntegerSetDomain(createIntegerDomain(lb, ub))
            val x = cc.space.createVariable(expr.toString, domain)
            cc.consts += expr -> x
            x
        case IntSetConst(IntSet(set)) =>
            val domain = new SingletonIntegerSetDomain(createIntegerDomain(set))
            val x = cc.space.createVariable(expr.toString, domain)
            cc.consts += expr -> x
            x
        case Term(id, Nil) =>
            cc.vars(expr)
        case ArrayAccess(id, IntConst(idx)) =>
            cc.arrays(Term(id, Nil))(idx - 1)
    }

    implicit protected final def compileConstant(a: BooleanValue): Variable[BooleanValue] =
        compileExpr(BoolConst(a.value))

    implicit protected final def compileConstant(a: IntegerValue): Variable[IntegerValue] =
        compileExpr(IntConst(a.value))

    implicit protected final def compileConstant(a: IntegerDomain): Variable[IntegerSetValue] =
        if (a.isFinite && a.ranges.size == 1) compileExpr[IntegerSetValue](IntSetConst(IntRange(a.lb.value, a.ub.value)))
        else cc.space.createVariable(a.toString, new SingletonIntegerSetDomain(a))

    protected final def createChannel
        [Value <: AnyValue]
        (implicit valueTraits: AnyValueTraits[Value]):
        Variable[Value] =
        cc.space.createVariable[Value]("", valueTraits.unboundedDomain)

    protected final def createNonNegativeChannel
        [Value <: NumericalValue[Value]]
        (implicit valueTraits: NumericalValueTraits[Value]):
        Variable[Value] =
        cc.space.createVariable[Value]("", valueTraits.nonNegativeDomain)

    protected final def nextConstraintId: Id[yuck.core.Constraint] =
        cc.space.constraintIdFactory.nextId

    implicit protected final def xs2axs
        [Value <: NumericalValue[Value]]
        (xs: immutable.IndexedSeq[Variable[Value]])
        (implicit valueTraits: NumericalValueTraits[Value]):
        immutable.IndexedSeq[AX[Value]] =
        for (x <- xs) yield new AX(valueTraits.one, x)

    // As domains of problem variables are unbounded until they get pruned by DomainEnforcer,
    // the method Variable[Value]#domain cannot be used to retrieve a variable's domain;
    // instead CompilationContext#domains has to be consulted.
    // The opposite is true for variables resulting from compiling constant expressions:
    // their domains are not tracked in CompilationContext#domains, so Variable[Value]#domain
    // has to be used.
    // This method hides all these technicalities.
    protected final def domain
        [Value <: AnyValue]
        (expr: Expr)
        (implicit valueTraits: AnyValueTraits[Value]):
        Domain[Value] =
    {
        cc.domains.get(expr).map(valueTraits.dynamicCast(_)).getOrElse(compileExpr[Value](expr).domain)
    }

    // Use this method to retrieve the ordered domain associated with an expression.
    // For the details, see the comment on domain.
    protected final def orderedDomain
        [Value <: OrderedValue[Value]]
        (expr: Expr)
        (implicit valueTraits: OrderedValueTraits[Value]):
        OrderedDomain[Value] =
    {
        valueTraits.staticCast(domain(expr))
    }

}
