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
abstract class CompilationPhase extends Runnable {

    protected val cc: CompilationContext

    protected final def compilesToConst(a: Expr): Boolean =
        a.isConst || cc.domains(a).isSingleton

    protected final def compilesToConst
        [Value <: AnyValue]
        (a: Expr, b: Value)
        (implicit valueTraits: ValueTraits[Value]): Boolean =
    {
        val maybeC = tryGetAnyConst(a)
        (! maybeC.isEmpty && valueTraits.safeDowncast(maybeC.get) == b)
    }

    protected final def getConst
        [Value <: AnyValue]
        (a: Expr)
        (implicit valueTraits: ValueTraits[Value]): Value =
    {
        tryGetConst(a).get
    }

    protected final def tryGetConst
        [Value <: AnyValue]
        (a: Expr)
        (implicit valueTraits: ValueTraits[Value]): Option[Value] =
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
        tryGetConst[BooleanValue](a).map(_.truthValue).map(BoolConst).getOrElse(a)

    protected final def normalizeInt(a: Expr): Expr =
        tryGetConst[IntegerValue](a).map(_.value).map(IntConst).getOrElse(a)

    protected final def normalizeArray(a: Expr): Expr = a match {
        case ArrayConst(a) => ArrayConst(a)
        case a => ArrayConst(getArrayElems(a).toList)
    }

    protected final def boolDomain(a: Expr): BooleanDecisionDomain = a match {
        case BoolConst(false) => FalseDomain
        case BoolConst(true) => TrueDomain
        case _ => cc.domains(a).asInstanceOf[BooleanDecisionDomain]
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
            cc.arrays.get(expr).get
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
            [Value <: OrderedValue[Value]]
            (expr: Expr)
            (implicit valueTraits: OrderedValueTraits[Value]):
            OrderedVariable[Value] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileOrdArray
            [Value <: OrderedValue[Value]]
            (expr: Expr)
            (implicit valueTraits: OrderedValueTraits[Value]):
            immutable.IndexedSeq[OrderedVariable[Value]] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(valueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[OrderedVariable[Value]]]
        }

    }

    protected trait MediumPriorityImplicits extends LowPriorityImplicits {

        implicit final def compileNumExpr
            [Value <: NumericalValue[Value]]
            (expr: Expr)
            (implicit valueTraits: NumericalValueTraits[Value]):
            NumericalVariable[Value] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileNumArray
            [Value <: NumericalValue[Value]]
            (expr: Expr)
            (implicit valueTraits: NumericalValueTraits[Value]):
            immutable.IndexedSeq[NumericalVariable[Value]] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(valueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[NumericalVariable[Value]]]
        }

    }

    protected object HighPriorityImplicits extends MediumPriorityImplicits {

        implicit final def compileExpr
            [Value <: AnyValue]
            (expr: Expr)
            (implicit valueTraits: ValueTraits[Value]):
            Variable[Value] =
        {
            valueTraits.safeDowncast(compileAnyExpr(expr))
        }

        implicit final def compileArray
            [Value <: AnyValue]
            (expr: Expr)
            (implicit valueTraits: ValueTraits[Value]):
            immutable.IndexedSeq[Variable[Value]] =
        {
            val xs = compileAnyArray(expr)
            xs.foreach(valueTraits.safeDowncast)
            xs.asInstanceOf[immutable.IndexedSeq[Variable[Value]]]
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
        [Value <: AnyValue]
        (implicit valueTraits: ValueTraits[Value]):
        Variable[Value] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createOrdChannel
        [Value <: OrderedValue[Value]]
        (implicit valueTraits: OrderedValueTraits[Value]):
        OrderedVariable[Value] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createNumChannel
        [Value <: NumericalValue[Value]]
        (implicit valueTraits: NumericalValueTraits[Value]):
        NumericalVariable[Value] =
    {
        valueTraits.createChannel(cc.space)
    }

    protected final def createNonNegativeChannel
        [Value <: NumericalValue[Value]]
        (implicit valueTraits: NumericalValueTraits[Value]):
        NumericalVariable[Value] =
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

    protected abstract class CompilationHelper[Value <: AnyValue, Variable <: yuck.core.Variable[Value]] {
        def compileExpr(expr: Expr): Variable
        def compileArray(expr: Expr): immutable.IndexedSeq[Variable]
        def createChannel: Variable
    }

    protected abstract class OrderedCompilationHelper[Value <: OrderedValue[Value], Variable <: OrderedVariable[Value]]
    extends CompilationHelper[Value, Variable]

    protected abstract class NumericalCompilationHelper[Value <: NumericalValue[Value], Variable <: NumericalVariable[Value]]
    extends OrderedCompilationHelper[Value, Variable]

    implicit protected object BooleanCompilationHelper extends OrderedCompilationHelper[BooleanValue, BooleanVariable] {
        import HighPriorityImplicits._
        override def compileExpr(expr: Expr) = compileBoolExpr(expr)
        override def compileArray(expr: Expr) = compileBoolArray(expr)
        override def createChannel = createBoolChannel
    }

    implicit protected object IntegerCompilationHelper extends NumericalCompilationHelper[IntegerValue, IntegerVariable] {
        import HighPriorityImplicits._
        override def compileExpr(expr: Expr) = compileIntExpr(expr)
        override def compileArray(expr: Expr) = compileIntArray(expr)
        override def createChannel = createIntChannel
    }

    implicit protected object IntegerSetCompilationHelper extends OrderedCompilationHelper[IntegerSetValue, IntegerSetVariable] {
        import HighPriorityImplicits._
        override def compileExpr(expr: Expr) = compileIntSetExpr(expr)
        override def compileArray(expr: Expr) = compileIntSetArray(expr)
        override def createChannel = createIntSetChannel
    }

    protected final def nextConstraintId: Id[yuck.core.Constraint] =
        cc.space.nextConstraintId

    implicit protected final def xs2axs
        [Value <: NumericalValue[Value]]
        (xs: immutable.IndexedSeq[NumericalVariable[Value]])
        (implicit valueTraits: NumericalValueTraits[Value]):
        immutable.IndexedSeq[AX[Value]] =
    {
        for (x <- xs) yield new AX(valueTraits.one, x)
    }

}
