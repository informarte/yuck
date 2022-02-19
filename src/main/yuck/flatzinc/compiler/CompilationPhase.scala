package yuck.flatzinc.compiler

import scala.collection.*
import scala.language.implicitConversions

import yuck.core.*
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
        [V <: AnyValue]
        (a: Expr, b: V)
        (implicit valueTraits: ValueTraits[V]): Boolean =
    {
        val maybeC = tryGetAnyConst(a)
        (! maybeC.isEmpty && valueTraits.safeDowncast(maybeC.get) == b)
    }

    protected final def getConst
        [V <: AnyValue]
        (a: Expr)
        (implicit valueTraits: ValueTraits[V]): V =
    {
        tryGetConst(a).get
    }

    protected final def tryGetConst
        [V <: AnyValue]
        (a: Expr)
        (implicit valueTraits: ValueTraits[V]): Option[V] =
    {
        tryGetAnyConst(a).map(valueTraits.safeDowncast)
    }

    protected final def getAnyConst(a: Expr): AnyValue =
        tryGetAnyConst(a).get

    private final def tryGetAnyConst(a: Expr): Option[AnyValue] = {
        a match {
            case BoolConst(a) => Some(if (a) True else False)
            case IntConst(a) => Some(IntegerValue(a))
            case IntSetConst(IntRange(lb, ub)) => Some(new IntegerSetValue(createIntDomain(lb, ub)))
            case IntSetConst(IntSet(set)) => Some(new IntegerSetValue(createIntDomain(set)))
            case FloatConst(_) => throw new UnsupportedFlatZincTypeException(FloatType(None))
            case _ if cc.domains(a).isSingleton => Some(cc.domains(a).singleValue)
            case _ => None
        }
    }

    protected final def normalizeBool(a: Expr): Expr =
        tryGetConst[BooleanValue](a).map(_.truthValue).map(BoolConst.apply).getOrElse(a)

    protected final def normalizeInt(a: Expr): Expr =
        tryGetConst[IntegerValue](a).map(_.value).map(IntConst.apply).getOrElse(a)

    protected final def normalizeArray(a: Expr): Expr = a match {
        case ArrayConst(a) => ArrayConst(a)
        case a => ArrayConst(getArrayElems(a).toList)
    }

    protected final def boolDomain(a: Expr): BooleanDomain = a match {
        case BoolConst(false) => FalseDomain
        case BoolConst(true) => TrueDomain
        case _ => cc.domains(a).asInstanceOf[BooleanDomain]
    }

    protected final def intDomain(a: Expr): IntegerDomain = a match {
        case IntConst(a) => createIntDomain(a, a)
        case IntSetConst(IntRange(lb, ub)) => createIntDomain(lb, ub)
        case IntSetConst(IntSet(set)) => createIntDomain(set)
        case _ => cc.domains(a).asInstanceOf[IntegerDomain]
    }

    protected final def intSetDomain(a: Expr): IntegerSetDomain = a match {
        case IntSetConst(IntRange(lb, ub)) => new SingletonIntegerSetDomain(createIntDomain(lb, ub))
        case IntSetConst(IntSet(set)) => new SingletonIntegerSetDomain(createIntDomain(set))
        case _ => cc.domains(a).asInstanceOf[IntegerSetDomain]
    }

    protected final def getArrayElems(expr: Expr): immutable.Iterable[Expr] =
        cc.ast.getArrayElems(expr)

    protected final def createIntDomain(lb: Int, ub: Int): IntegerDomain =
        IntegerRange(lb, ub)

    protected final def createIntDomain(set: Set[Int]): IntegerDomain =
        IntegerDomain(set.map(IntegerValue.apply))

    protected final def compileAnyExpr(expr: Expr): AnyVariable = expr match {
        case _ if cc.consts.contains(expr) =>
            cc.consts(expr)
        case BoolConst(value) =>
            val domain = if (value) TrueDomain else FalseDomain
            val x = cc.space.createVariable(expr.toString, domain)
            cc.consts += expr -> x
            x
        case IntConst(value) =>
            val domain = createIntDomain(value, value)
            val x = cc.space.createVariable(expr.toString, domain)
            cc.consts += expr -> x
            x
        case IntSetConst(IntRange(lb, ub)) =>
            val domain = new SingletonIntegerSetDomain(createIntDomain(lb, ub))
            val x = cc.space.createVariable(expr.toString, domain)
            cc.consts += expr -> x
            x
        case IntSetConst(IntSet(set)) =>
            val domain = new SingletonIntegerSetDomain(createIntDomain(set))
            val x = cc.space.createVariable(expr.toString, domain)
            cc.consts += expr -> x
            x
        case FloatConst(_) =>
            throw new UnsupportedFlatZincTypeException(FloatType(None))
        case Term(id, Nil) =>
            cc.vars(expr)
        case ArrayAccess(id, IntConst(idx)) =>
            cc.arrays(Term(id, Nil))(idx - 1)
    }

    protected final def compileAnyArray(expr: Expr): immutable.IndexedSeq[AnyVariable] = expr match {
        case _ if cc.arrayConsts.contains(expr) =>
            cc.arrayConsts(expr)
        case Term(id, Nil) =>
            cc.arrays(expr)
        case ArrayConst(elems) =>
            val array = elems.iterator.map(elem => compileAnyExpr(elem)).to(immutable.ArraySeq)
            cc.arrayConsts += expr -> array
            array
    }

    protected trait LowestPriorityImplicits {

        implicit final def compileBoolExpr(expr: Expr): BooleanVariable =
            compileAnyExpr(expr).asInstanceOf[BooleanVariable]

        implicit final def compileIntExpr(expr: Expr): IntegerVariable =
            compileAnyExpr(expr).asInstanceOf[IntegerVariable]

        implicit final def compileIntSetExpr(expr: Expr): IntegerSetVariable =
            compileAnyExpr(expr).asInstanceOf[IntegerSetVariable]

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
            (implicit valueTraits: OrderedValueTraits[V]):
            OrderedVariable[V] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileOrdArray
            [V <: OrderedValue[V]]
            (expr: Expr)
            (implicit valueTraits: OrderedValueTraits[V]):
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
            (implicit valueTraits: NumericalValueTraits[V]):
            NumericalVariable[V] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileNumArray
            [V <: NumericalValue[V]]
            (expr: Expr)
            (implicit valueTraits: NumericalValueTraits[V]):
            immutable.IndexedSeq[NumericalVariable[V]] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(valueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[NumericalVariable[V]]]
        }

    }

    protected object HighPriorityImplicits extends MediumPriorityImplicits {

        implicit final def compileExpr
            [V <: AnyValue]
            (expr: Expr)
            (implicit valueTraits: ValueTraits[V]):
            Variable[V] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileArray
            [V <: AnyValue]
            (expr: Expr)
            (implicit valueTraits: ValueTraits[V]):
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
        [V <: AnyValue]
        (implicit valueTraits: ValueTraits[V]):
        Variable[V] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createOrdChannel
        [V <: OrderedValue[V]]
        (implicit valueTraits: OrderedValueTraits[V]):
        OrderedVariable[V] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createNumChannel
        [V <: NumericalValue[V]]
        (implicit valueTraits: NumericalValueTraits[V]):
        NumericalVariable[V] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createNonNegativeChannel
        [V <: NumericalValue[V]]
        (implicit valueTraits: NumericalValueTraits[V]):
        NumericalVariable[V] =
    {
        valueTraits.createVariable(cc.space, "", valueTraits.nonNegativeDomain)
    }

    protected final def createBoolChannel: BooleanVariable = BooleanValueTraits.createChannel(cc.space)
    protected final def createIntChannel: IntegerVariable = IntegerValueTraits.createChannel(cc.space)
    protected final def createNonNegativeIntChannel: IntegerVariable = {
        val x = createIntChannel
        x.pruneDomain(NonNegativeIntegerRange)
        x
    }
    protected final def createIntSetChannel: IntegerSetVariable = IntegerSetValueTraits.createChannel(cc.space)

    protected abstract class CompilationHelper[V <: AnyValue, Variable <: yuck.core.Variable[V]] {
        def compileExpr(expr: Expr): Variable
        def compileArray(expr: Expr): immutable.IndexedSeq[Variable]
        def createChannel: Variable
    }

    protected abstract class OrderedCompilationHelper[V <: OrderedValue[V], Variable <: OrderedVariable[V]]
    extends CompilationHelper[V, Variable]

    protected abstract class NumericalCompilationHelper[V <: NumericalValue[V], Variable <: NumericalVariable[V]]
    extends OrderedCompilationHelper[V, Variable]

    implicit protected object BooleanCompilationHelper extends OrderedCompilationHelper[BooleanValue, BooleanVariable] {
        import HighPriorityImplicits.*
        override def compileExpr(expr: Expr) = compileBoolExpr(expr)
        override def compileArray(expr: Expr) = compileBoolArray(expr)
        override def createChannel = createBoolChannel
    }

    implicit protected object IntegerCompilationHelper extends NumericalCompilationHelper[IntegerValue, IntegerVariable] {
        import HighPriorityImplicits.*
        override def compileExpr(expr: Expr) = compileIntExpr(expr)
        override def compileArray(expr: Expr) = compileIntArray(expr)
        override def createChannel = createIntChannel
    }

    implicit protected object IntegerSetCompilationHelper extends OrderedCompilationHelper[IntegerSetValue, IntegerSetVariable] {
        import HighPriorityImplicits.*
        override def compileExpr(expr: Expr) = compileIntSetExpr(expr)
        override def compileArray(expr: Expr) = compileIntSetArray(expr)
        override def createChannel = createIntSetChannel
    }

    protected final def nextConstraintId(): Id[yuck.core.Constraint] =
        cc.space.nextConstraintId()

    implicit protected final def xs2axs
        [V <: NumericalValue[V]]
        (xs: immutable.IndexedSeq[NumericalVariable[V]])
        (implicit valueTraits: NumericalValueTraits[V]):
        immutable.IndexedSeq[AX[V]] =
    {
        for (x <- xs) yield new AX(valueTraits.one, x)
    }

}
