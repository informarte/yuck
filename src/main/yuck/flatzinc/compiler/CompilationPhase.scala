package yuck.flatzinc.compiler

import scala.collection.*
import scala.language.implicitConversions

import yuck.core.*
import yuck.flatzinc.ast.*

/**
 * The given compilation context may be modified to transport information between phases.
 */
abstract class CompilationPhase extends Runnable {

    protected val cc: CompilationContext

    protected final def compilesToConst(a: Expr): Boolean =
        a.isConst || cc.domains(a).isSingleton

    protected final def compilesToConst
        [A <: Value[A], D <: Domain[A, D]]
        (a: Expr, b: A)
        (using typeTraits: TypeTraits[A, D, ?]): Boolean =
    {
        val maybeC = tryGetAnyConst(a)
        maybeC.isDefined && typeTraits.safeDowncast(maybeC.get) == b
    }

    private def getConst
        [A <: Value[A], D <: Domain[A, D]]
        (a: Expr)
        (using typeTraits: TypeTraits[A, D, ?]): A =
    {
        tryGetConst(a).get
    }

    private def tryGetConst
        [A <: Value[A], D <: Domain[A, D]]
        (a: Expr)
        (using typeTraits: TypeTraits[A, D, ?]): Option[A] =
    {
        tryGetAnyConst(a).map(typeTraits.safeDowncast)
    }

    private def getAnyConst(a: Expr): AnyValue =
        tryGetAnyConst(a).get

    private def tryGetAnyConst(a: Expr): Option[AnyValue] = {
        a match {
            case BoolConst(a) => Some(if a then True else False)
            case IntConst(a) => Some(IntegerValue(a))
            case IntSetConst(IntRange(lb, ub)) => Some(new IntegerSetValue(IntegerRange(lb, ub)))
            case IntSetConst(IntSet(set)) => Some(new IntegerSetValue(IntegerDomain(set)))
            case FloatConst(_) => throw new UnsupportedFlatZincTypeException(FloatType(None))
            case _ if cc.vars(a).domain.isSingleton => Some(cc.vars(a).domain.singleValue)
            case _ => None
        }
    }

    protected final def normalizeBool(a: Expr): Expr =
        tryGetConst(a)(using BooleanTypeTraits).map(_.truthValue).map(BoolConst.apply).getOrElse(a)

    protected final def normalizeInt(a: Expr): Expr =
        tryGetConst(a)(using IntegerTypeTraits).map(_.value).map(IntConst.apply).getOrElse(a)

    protected final def normalizeArray(a: Expr): Expr = a match {
        case ArrayConst(a) => ArrayConst(a)
        case a => ArrayConst(getArrayElems(a))
    }

    protected final def boolDomain(a: Expr): BooleanDomain = a match {
        case BoolConst(false) => FalseDomain
        case BoolConst(true) => TrueDomain
        case _ => cc.domains(a).asInstanceOf[BooleanDomain]
    }

    protected final def intDomain(a: Expr): IntegerDomain = a match {
        case IntConst(a) => IntegerRange(a, a)
        case IntSetConst(IntRange(lb, ub)) => IntegerRange(lb, ub)
        case IntSetConst(IntSet(set)) => IntegerDomain(set)
        case _ => cc.domains(a).asInstanceOf[IntegerDomain]
    }

    protected final def intSetDomain(a: Expr): IntegerSetDomain = a match {
        case IntSetConst(IntRange(lb, ub)) =>
            val d0 = IntegerRange(lb, ub)
            val d = if d0.isSubsetOf(SixtyFourBitSet.ValueRange) then SixtyFourBitSet(lb, ub) else d0
            new SingletonIntegerSetDomain(d)
        case IntSetConst(IntSet(set)) =>
            val d0 = IntegerDomain(set)
            val d = if d0.isSubsetOf(SixtyFourBitSet.ValueRange) then SixtyFourBitSet(d0) else d0
            new SingletonIntegerSetDomain(d)
        case _ => cc.domains(a).asInstanceOf[IntegerSetDomain]
    }

    protected final def compileAnyExpr(expr: Expr): AnyVariable = expr match {
        case _ if cc.consts.contains(expr) =>
            cc.consts(expr)
        case BoolConst(_) =>
            val x = cc.space.createVariable(expr.toString, boolDomain(expr))
            cc.consts += expr -> x
            x
        case IntConst(_) =>
            val x = cc.space.createVariable(expr.toString, intDomain(expr))
            cc.consts += expr -> x
            x
        case IntSetConst(_) =>
            val x = cc.space.createVariable(expr.toString, intSetDomain(expr))
            cc.consts += expr -> x
            x
        case FloatConst(_) =>
            throw new UnsupportedFlatZincTypeException(FloatType(None))
        case Term(_, Nil) =>
            cc.vars(expr)
        case ArrayAccess(id, IntConst(idx)) =>
            cc.arrays(Term(id, Nil))(safeToInt(idx - 1))
    }

    protected final def compileAnyArray(expr: Expr): immutable.IndexedSeq[AnyVariable] = expr match {
        case _ if cc.arrayConsts.contains(expr) =>
            cc.arrayConsts(expr)
        case Term(_, Nil) =>
            cc.arrays(expr)
        case ArrayConst(elems) =>
            val array = elems.iterator.map(elem => compileAnyExpr(elem)).toVector
            cc.arrayConsts += expr -> array
            array
    }

    protected final def getArrayElems(expr: Expr): immutable.IndexedSeq[Expr] =
        cc.ast.getArrayElems(expr)

    protected trait LowestPriorityImplicits {

        implicit final def compileBoolExpr(expr: Expr): BooleanVariable =
            compileAnyExpr(expr).asInstanceOf[BooleanVariable]

        implicit final def compileConstBoolExpr(expr: Expr): BooleanValue =
            getConst(expr)(using BooleanTypeTraits)

        implicit final def compileIntExpr(expr: Expr): IntegerVariable =
            compileAnyExpr(expr).asInstanceOf[IntegerVariable]

        implicit final def compileConstIntExpr(expr: Expr): IntegerValue =
            getConst(expr)(using IntegerTypeTraits)

        implicit final def compileIntSetExpr(expr: Expr): IntegerSetVariable =
            compileAnyExpr(expr).asInstanceOf[IntegerSetVariable]

        implicit final def compileConstIntSetExpr(expr: Expr): IntegerSetValue =
            getConst(expr)(using IntegerSetTypeTraits)

        implicit final def compileBoolArray(expr: Expr): immutable.IndexedSeq[BooleanVariable] = {
            val xs = compileAnyArray(expr)
            xs.foreach(BooleanTypeTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[BooleanVariable]]
        }

        implicit final def compileIntArray(expr: Expr): immutable.IndexedSeq[IntegerVariable] = {
            val xs = compileAnyArray(expr)
            xs.foreach(IntegerTypeTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[IntegerVariable]]
        }

        implicit final def compileIntSetArray(expr: Expr): immutable.IndexedSeq[IntegerSetVariable] = {
            val xs = compileAnyArray(expr)
            xs.foreach(IntegerSetTypeTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[IntegerSetVariable]]
        }

    }

    protected trait LowPriorityImplicits extends LowestPriorityImplicits {

        implicit final def compileOrdExpr
            [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
            (expr: Expr)
            (using typeTraits: OrderedTypeTraits[A, D, X]):
            X =
        {
            typeTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileOrdArray
            [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
            (expr: Expr)
            (using typeTraits: OrderedTypeTraits[A, D, X]):
            immutable.IndexedSeq[X] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(typeTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[X]]
        }

    }

    protected trait MediumPriorityImplicits extends LowPriorityImplicits {

        implicit final def compileNumExpr
            [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
            (expr: Expr)
            (using typeTraits: NumericalTypeTraits[A, D, X]):
            X =
        {
            typeTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileNumArray
            [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
            (expr: Expr)
            (using typeTraits: NumericalTypeTraits[A, D, X]):
            immutable.IndexedSeq[X] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(typeTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[X]]
        }

    }

    protected object HighPriorityImplicits extends MediumPriorityImplicits {

        implicit final def compileExpr
            [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
            (expr: Expr)
            (using typeTraits: TypeTraits[A, D, X]):
            X =
        {
            typeTraits.safeDowncast(compileAnyExpr(expr))
        }


        implicit final def compileArray
            [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
            (expr: Expr)
            (using typeTraits: TypeTraits[A, D, X]):
            immutable.IndexedSeq[X] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(typeTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[X]]
        }
    }

    implicit protected final def compileConstant(a: BooleanValue): BooleanVariable =
        HighPriorityImplicits.compileBoolExpr(BoolConst(a.truthValue))

    implicit protected final def compileConstant(a: IntegerValue): IntegerVariable =
        HighPriorityImplicits.compileIntExpr(IntConst(a.value))

    implicit protected final def compileConstant(a: IntegerDomain): IntegerSetVariable =
        if a.isFinite && ! a.hasGaps
        then HighPriorityImplicits.compileIntSetExpr(IntSetConst(IntRange(a.lb.value, a.ub.value)))
        else IntegerSetTypeTraits.createVariable(cc.space, a.toString, new SingletonIntegerSetDomain(a))

    protected final def createChannel
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
        ()
        (using typeTraits: TypeTraits[A, D, X]):
        X =
    {
        typeTraits.createChannel(cc.space)
    }

    protected final def createOrdChannel
        [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
        ()
        (using typeTraits: OrderedTypeTraits[A, D, X]):
        X =
    {
        typeTraits.createChannel(cc.space)
    }

    protected final def createNumChannel
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        ()
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        X =
    {
        typeTraits.createChannel(cc.space)
    }

    protected final def createNonNegativeChannel
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        ()
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        X =
    {
        typeTraits.createVariable(cc.space, "", typeTraits.nonNegativeDomain)
    }

    protected final def createBoolChannel(): BooleanVariable = BooleanTypeTraits.createChannel(cc.space)
    protected final def createIntChannel(): IntegerVariable = IntegerTypeTraits.createChannel(cc.space)
    protected final def createNonNegativeIntChannel(): IntegerVariable = {
        val x = createIntChannel()
        x.pruneDomain(NonNegativeIntegerRange)
        x
    }
    protected final def createIntSetChannel(): IntegerSetVariable = IntegerSetTypeTraits.createChannel(cc.space)

    protected abstract class CompilationHelper[A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]] {
        val typeTraits: TypeTraits[A, D, X]
        def compileExpr(expr: Expr): X
        def compileArray(expr: Expr): immutable.IndexedSeq[X]
        def createChannel(): X
    }

    protected abstract class OrderedCompilationHelper
        [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
        extends CompilationHelper[A, D, X]
    {
        override val typeTraits: OrderedTypeTraits[A, D, X]
    }

    protected abstract class NumericalCompilationHelper
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        extends OrderedCompilationHelper[A, D, X]
    {
        override val typeTraits: NumericalTypeTraits[A, D, X]
    }

    protected given BooleanCompilationHelper: OrderedCompilationHelper[BooleanValue, BooleanDomain, BooleanVariable] with {
        import HighPriorityImplicits.*
        override val typeTraits = BooleanTypeTraits
        override def compileExpr(expr: Expr) = compileBoolExpr(expr)
        override def compileArray(expr: Expr) = compileBoolArray(expr)
        override def createChannel() = createBoolChannel()
    }

    protected given IntegerCompilationHelper: NumericalCompilationHelper[IntegerValue, IntegerDomain, IntegerVariable] with {
        import HighPriorityImplicits.*
        override val typeTraits = IntegerTypeTraits
        override def compileExpr(expr: Expr) = compileIntExpr(expr)
        override def compileArray(expr: Expr) = compileIntArray(expr)
        override def createChannel() = createIntChannel()
    }

    protected given IntegerSetCompilationHelper: OrderedCompilationHelper[IntegerSetValue, IntegerSetDomain, IntegerSetVariable] with {
        import HighPriorityImplicits.*
        override val typeTraits = IntegerSetTypeTraits
        override def compileExpr(expr: Expr) = compileIntSetExpr(expr)
        override def compileArray(expr: Expr) = compileIntSetArray(expr)
        override def createChannel() = createIntSetChannel()
    }

    protected final def nextConstraintId(): Id[yuck.core.Constraint] =
        cc.space.nextConstraintId()

    implicit protected final def xs2axs
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (xs: immutable.IndexedSeq[X])
        (using typeTraits: NumericalTypeTraits[A, D, X]):
        immutable.IndexedSeq[AX[A, D, X]] =
    {
        for x <- xs yield new AX(typeTraits.one, x)
    }

}
