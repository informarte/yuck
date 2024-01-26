package yuck.flatzinc.compiler

import scala.collection.*
import scala.language.implicitConversions

import yuck.core.{given, *}
import yuck.flatzinc.ast.*
import yuck.flatzinc.parser.*

/**
 * The given compilation context may be modified to transport information between phases.
 *
 * @author Michael Marte
 */
abstract class CompilationPhase extends Runnable {

    protected val cc: CompilationContext

    protected final def compilesToConst(a: Expr): Boolean =
        a.isConst || cc.domains(a).isSingleton

    protected final def compilesToConst
        [V <: Value[V]]
        (a: Expr, b: V)
        (using valueTraits: ValueTraits[V]): Boolean =
    {
        val maybeC = tryGetAnyConst(a)
        maybeC.isDefined && valueTraits.safeDowncast(maybeC.get) == b
    }

    protected final def getConst
        [V <: Value[V]]
        (a: Expr)
        (using valueTraits: ValueTraits[V]): V =
    {
        tryGetConst(a).get
    }

    protected final def tryGetConst
        [V <: Value[V]]
        (a: Expr)
        (using valueTraits: ValueTraits[V]): Option[V] =
    {
        tryGetAnyConst(a).map(valueTraits.safeDowncast)
    }

    protected final def getAnyConst(a: Expr): AnyValue =
        tryGetAnyConst(a).get

    private final def tryGetAnyConst(a: Expr): Option[AnyValue] = {
        a match {
            case BoolConst(a) => Some(if (a) True else False)
            case IntConst(a) => Some(IntegerValue(a))
            case IntSetConst(IntRange(lb, ub)) => Some(new IntegerSetValue(IntegerRange(lb, ub)))
            case IntSetConst(IntSet(set)) => Some(new IntegerSetValue(IntegerDomain(set)))
            case FloatConst(_) => throw new UnsupportedFlatZincTypeException(FloatType(None))
            case _ if cc.vars(a).domain.isSingleton => Some(cc.vars(a).domain.singleValue)
            case _ => None
        }
    }

    protected final def normalizeBool(a: Expr): Expr =
        tryGetConst[BooleanValue](a).map(_.truthValue).map(BoolConst.apply).getOrElse(a)

    protected final def normalizeInt(a: Expr): Expr =
        tryGetConst[IntegerValue](a).map(_.value).map(IntConst.apply).getOrElse(a)

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
            val d = if (d0.isSubsetOf(SixtyFourBitSet.ValueRange)) SixtyFourBitSet(lb, ub) else d0
            new SingletonIntegerSetDomain(d)
        case IntSetConst(IntSet(set)) =>
            val d0 = IntegerDomain(set)
            val d = if (d0.isSubsetOf(SixtyFourBitSet.ValueRange)) SixtyFourBitSet(d0) else d0
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
            getConst[BooleanValue](expr)

        implicit final def compileIntExpr(expr: Expr): IntegerVariable =
            compileAnyExpr(expr).asInstanceOf[IntegerVariable]

        implicit final def compileConstIntExpr(expr: Expr): IntegerValue =
            getConst[IntegerValue](expr)

        implicit final def compileIntSetExpr(expr: Expr): IntegerSetVariable =
            compileAnyExpr(expr).asInstanceOf[IntegerSetVariable]

        implicit final def compileConstIntSetExpr(expr: Expr): IntegerSetValue =
            getConst[IntegerSetValue](expr)

        implicit final def compileBoolArray(expr: Expr): immutable.IndexedSeq[BooleanVariable] = {
            val xs = compileAnyArray(expr)
            xs.foreach(BooleanValueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[BooleanVariable]]
        }

        implicit final def compileIntArray(expr: Expr): immutable.IndexedSeq[IntegerVariable] = {
            val xs = compileAnyArray(expr)
            xs.foreach(IntegerValueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[IntegerVariable]]
        }

        implicit final def compileIntSetArray(expr: Expr): immutable.IndexedSeq[IntegerSetVariable] = {
            val xs = compileAnyArray(expr)
            xs.foreach(IntegerSetValueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[IntegerSetVariable]]
        }

    }

    protected trait LowPriorityImplicits extends LowestPriorityImplicits {

        implicit final def compileOrdExpr
            [V <: OrderedValue[V]]
            (expr: Expr)
            (using valueTraits: OrderedValueTraits[V]):
            OrderedVariable[V] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileOrdArray
            [V <: OrderedValue[V]]
            (expr: Expr)
            (using valueTraits: OrderedValueTraits[V]):
            immutable.IndexedSeq[OrderedVariable[V]] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(valueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[OrderedVariable[V]]]
        }

    }

    protected trait MediumPriorityImplicits extends LowPriorityImplicits {

        implicit final def compileNumExpr
            [V <: NumericalValue[V]]
            (expr: Expr)
            (using valueTraits: NumericalValueTraits[V]):
            NumericalVariable[V] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileNumArray
            [V <: NumericalValue[V]]
            (expr: Expr)
            (using valueTraits: NumericalValueTraits[V]):
            immutable.IndexedSeq[NumericalVariable[V]] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(valueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[NumericalVariable[V]]]
        }

    }

    protected object HighPriorityImplicits extends MediumPriorityImplicits {

        implicit final def compileExpr
            [V <: Value[V]]
            (expr: Expr)
            (using valueTraits: ValueTraits[V]):
            Variable[V] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileArray
            [V <: Value[V]]
            (expr: Expr)
            (using valueTraits: ValueTraits[V]):
            immutable.IndexedSeq[Variable[V]] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(valueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[Variable[V]]]
        }

    }

    implicit protected final def compileConstant(a: BooleanValue): BooleanVariable =
        HighPriorityImplicits.compileBoolExpr(BoolConst(a.truthValue))

    implicit protected final def compileConstant(a: IntegerValue): IntegerVariable =
        HighPriorityImplicits.compileIntExpr(IntConst(a.value))

    implicit protected final def compileConstant(a: IntegerDomain): IntegerSetVariable =
        if (a.isFinite && ! a.hasGaps) HighPriorityImplicits.compileIntSetExpr(IntSetConst(IntRange(a.lb.value, a.ub.value)))
        else IntegerSetValueTraits.createVariable(cc.space, a.toString, new SingletonIntegerSetDomain(a))

    protected final def createChannel
        [V <: Value[V]]
        ()
        (using valueTraits: ValueTraits[V]):
        Variable[V] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createOrdChannel
        [V <: OrderedValue[V]]
        ()
        (using valueTraits: OrderedValueTraits[V]):
        OrderedVariable[V] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createNumChannel
        [V <: NumericalValue[V]]
        ()
        (using valueTraits: NumericalValueTraits[V]):
        NumericalVariable[V] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createNonNegativeChannel
        [V <: NumericalValue[V]]
        ()
        (using valueTraits: NumericalValueTraits[V]):
        NumericalVariable[V] =
    {
        valueTraits.createVariable(cc.space, "", valueTraits.nonNegativeDomain)
    }

    protected final def createBoolChannel(): BooleanVariable = BooleanValueTraits.createChannel(cc.space)
    protected final def createIntChannel(): IntegerVariable = IntegerValueTraits.createChannel(cc.space)
    protected final def createNonNegativeIntChannel(): IntegerVariable = {
        val x = createIntChannel()
        x.pruneDomain(NonNegativeIntegerRange)
        x
    }
    protected final def createIntSetChannel(): IntegerSetVariable = IntegerSetValueTraits.createChannel(cc.space)

    protected abstract class CompilationHelper[V <: Value[V], Variable <: yuck.core.Variable[V]] {
        def compileExpr(expr: Expr): Variable
        def compileArray(expr: Expr): immutable.IndexedSeq[Variable]
        def createChannel(): Variable
    }

    protected abstract class OrderedCompilationHelper[V <: OrderedValue[V], Variable <: OrderedVariable[V]]
    extends CompilationHelper[V, Variable]

    protected abstract class NumericalCompilationHelper[V <: NumericalValue[V], Variable <: NumericalVariable[V]]
    extends OrderedCompilationHelper[V, Variable]

    protected given booleanCompilationHelper: OrderedCompilationHelper[BooleanValue, BooleanVariable] with {
        import HighPriorityImplicits.*
        override def compileExpr(expr: Expr) = compileBoolExpr(expr)
        override def compileArray(expr: Expr) = compileBoolArray(expr)
        override def createChannel() = createBoolChannel()
    }

    protected given integerCompilationHelper: NumericalCompilationHelper[IntegerValue, IntegerVariable] with {
        import HighPriorityImplicits.*
        override def compileExpr(expr: Expr) = compileIntExpr(expr)
        override def compileArray(expr: Expr) = compileIntArray(expr)
        override def createChannel() = createIntChannel()
    }

    protected given integerSetCompilationHelper: OrderedCompilationHelper[IntegerSetValue, IntegerSetVariable] with {
        import HighPriorityImplicits.*
        override def compileExpr(expr: Expr) = compileIntSetExpr(expr)
        override def compileArray(expr: Expr) = compileIntSetArray(expr)
        override def createChannel() = createIntSetChannel()
    }

    protected final def nextConstraintId(): Id[yuck.core.Constraint] =
        cc.space.nextConstraintId()

    implicit protected final def xs2axs
        [V <: NumericalValue[V]]
        (xs: immutable.IndexedSeq[NumericalVariable[V]])
        (using valueTraits: NumericalValueTraits[V]):
        immutable.IndexedSeq[AX[V]] =
    {
        for (x <- xs) yield new AX(valueTraits.one, x)
    }

}
