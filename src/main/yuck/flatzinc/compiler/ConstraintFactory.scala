package yuck.flatzinc.compiler

import scala.collection.*
import scala.ref.WeakReference

import yuck.constraints.*
import yuck.core.*
import yuck.flatzinc.ast.*
import yuck.util.arm.{Sigint, scoped}
import yuck.util.logging.LogScope

/**
 * Compiles FlatZinc constraints to Yuck constraints.
 *
 * Skips FlatZinc constraints that were marked as redundant in an earlier phase.
 *
 * The implementation is based on pattern matching.
 *
 * In case a FlatZinc constraint gets rewritten to another FlatZinc constraint,
 * this step will be visible from the log.
 *
 * Many reified FlatZinc constraints receive special treatment but there is a
 * final case (matching all constraint names ending with "_reif") that implements
 * reification in a generic way such that ALL FlatZinc constraints (including global
 * ones) can be reified.
 *
 * Potential functional dependencies (declared by defines_var annotations) are
 * exploited as far as possible; only those annotations are ignored the processing
 * of which would entail a cyclic constraint graph.
 *
 * @author Michael Marte
 */
final class ConstraintFactory
    (override protected val cc: CompilationContext, sigint: Sigint)
    extends CompilationPhase
{

    private val space = cc.space
    private val impliedConstraints = cc.impliedConstraints
    private val logger = cc.logger

    private def definedVars(annotation: Annotation): Seq[AnyVariable] =
        annotation.term match {
            case Term("defines_var", List(a)) => List(compileAnyExpr(a))
            case Term("defines_vars", List(a)) => compileAnyArray(a)
            case _ => Nil
        }

    // Checks whether the given constraint is annotated with defines_var(a) where a compiles to out.
    private def definesVar(constraint: yuck.flatzinc.ast.Constraint, out: AnyVariable): Boolean =
        constraint.annotations.iterator.flatMap(definedVars).contains(out)

    // Checks whether the given constraint is annotated with defines_var(out).
    private def definesVar(constraint: yuck.flatzinc.ast.Constraint, out: Expr): Boolean =
        definesVar(constraint, compileAnyExpr(out))

    // Computes the set of variables defined by the given constraint according to its annotations.
    private def definedVars(constraint: yuck.flatzinc.ast.Constraint): Set[AnyVariable] =
        constraint.annotations.iterator.flatMap(definedVars).toSet

    private val fakeConstraintId = new Id[yuck.core.Constraint](-1)

    // Checks whether a constraint from in to out could be posted.
    // Notice that this method may be quite expensive!
    private def isViableConstraint(in: Iterable[AnyVariable], out: AnyVariable): Boolean =
        (! out.domain.isSingleton) &&
        (! cc.searchVars.contains(out)) &&
        space.maybeDefiningConstraint(out).isEmpty &&
        (! space.wouldIntroduceCycle(new DummyConstraint(fakeConstraintId, in, List(out))))

    // Checks whether there is a functional dependency that could be exploited without introducing a cycle.
    // Notice that this method may be quite expensive!
    private def definesVar
        (constraint: yuck.flatzinc.ast.Constraint, in: Iterable[AnyVariable], out: AnyVariable): Boolean =
    {
        (! out.domain.isSingleton) &&
        (definesVar(constraint, out) || ! cc.definedVars.contains(out)) &&
        isViableConstraint(in, out)
    }

    private def forcesImplicitSolving(annotation: Annotation): Boolean =
        annotation.term match {
            case Term("implicit", Nil) => true
            case _=> false
        }

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         in: Iterable[AnyVariable],
         out: Iterable[AnyVariable],
         functionalCase: => Iterable[BooleanVariable],
         generalCase: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
    {
        val definableVars = definedVars(constraint)
        if (out.forall(x =>
                ! x.domain.isSingleton &&
                ! cc.searchVars.contains(x) &&
                space.maybeDefiningConstraint(x).isEmpty &&
                (definableVars.contains(x) || ! cc.definedVars.contains(x))))
        {
            try {
                functionalCase
            }
            catch {
                case _: CyclicConstraintNetworkException => generalCase
                case _: IllegalArgumentException => generalCase
            }
        } else {
            generalCase
        }
    }

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         in: Seq[Expr],
         out: Expr,
         functionalCase: => Iterable[BooleanVariable],
         generalCase: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
        compileConstraint(constraint, in.map(compileAnyExpr), List(compileAnyExpr(out)), functionalCase, generalCase)

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         in: Expr,
         out: Expr,
         functionalCase: => Iterable[BooleanVariable],
         generalCase: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
        compileConstraint(constraint, compileAnyArray(in), List(compileAnyExpr(out)), functionalCase, generalCase)

    override def run() = {
        cc.costVars ++=
            cc.ast.constraints
            .iterator
            .flatMap(constraint =>
                compileConstraint(if (cc.cfg.attachGoals) Some(new FlatZincGoal(constraint)) else None, constraint))
    }

    private val IntLin = "int_lin_(.*)".r
    private val Reif = "(.*)_reif".r

    // In Yuck, True < False, but in FlatZinc, false < true.
    private val booleanOrdering = BooleanValueTraits.valueOrdering.reverse
    private val booleanSequenceOrdering = createLexicographicOrderingForIterable(booleanOrdering)

    private def compileConstraint
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint):
        Iterable[BooleanVariable] =
    {
        if (sigint.isSet) {
            throw new FlatZincCompilerInterruptedException
        }
        if (impliedConstraints.contains(constraint)) {
            logger.logg("Skipping %s".format(constraint))
            Nil
        } else {
            logger.logg("Compiling %s".format(constraint))
            scoped(new LogScope(logger)) {
                // toList enforces constraint generation in this log scope
                compileNonImplicitConstraint(maybeGoal, constraint).toList
            }
        }
    }

    import HighPriorityImplicits.*

    private def compileNonImplicitConstraint
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint):
        Iterable[BooleanVariable] =
        (constraint: @unchecked) match
    {
        // TODO Implement other direction!?
        case Constraint("bool2int", List(a, b), _) =>
            val x = compileBoolExpr(a)
            val y = compileIntExpr(b)

            def functionalCase = {
                space.post(new Bool2Int1(nextConstraintId(), maybeGoal, x, y))
                Nil
            }

            def generalCase = {
                val costs = createBoolChannel
                space.post(new Bool2Int2(nextConstraintId(), maybeGoal, x, y, costs))
                List(costs)
            }

            compileConstraint(constraint, List(a), b, functionalCase, generalCase)
        case Constraint("bool2costs", List(a, b), _) =>
            val x = compileBoolExpr(a)
            val y = compileIntExpr(b)

            def functionalCase = {
                space.post(new Bool2Costs1(nextConstraintId(), maybeGoal, x, y))
                Nil
            }

            def generalCase = {
                val costs = createBoolChannel
                space.post(new Bool2Costs2(nextConstraintId(), maybeGoal, x, y, costs))
                List(costs)
            }

            compileConstraint(constraint, List(a), b, functionalCase, generalCase)
        case Constraint("bool_not", List(a, b), _) =>
            def functionalCase = {
                space.post(new Not(nextConstraintId(), maybeGoal, a, b))
                Nil
            }
            def generalCase = {
                val costs = createBoolChannel
                space.post(new Ne[BooleanValue](nextConstraintId(), maybeGoal, a, b, costs))
                List(costs)
            }
            compileConstraint(constraint, List(a), b, functionalCase, generalCase)
        case Constraint("bool_eq", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Eq[BooleanValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("bool_eq_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Eq[BooleanValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("bool_lt", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Lt[BooleanValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("bool_lt_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Lt[BooleanValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("bool_le", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Le[BooleanValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("bool_le_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Le[BooleanValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("bool_and", _, _) =>
            compileTernaryBoolConstraint(new And(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("bool_or", _, _) =>
            compileTernaryBoolConstraint(new Or(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("bool_xor", _, _) =>
            compileTernaryBoolConstraint(new Ne[BooleanValue](_, _, _, _, _), maybeGoal, constraint)
        case Constraint("array_bool_and", List(as, b), _) =>
            val as1 = ArrayConst(getArrayElems(as).iterator.filter(a => ! compilesToConst(a, True)).toList)
            val as2 = if (as1.value.isEmpty) ArrayConst(List(BoolConst(true))) else as1
            val xs = compileBoolArray(as2)
            val y = compileBoolExpr(b)
            def post(y: BooleanVariable): BooleanVariable = {
                if (xs.size == 2) {
                    space.post(new And(nextConstraintId(), maybeGoal, xs(0), xs(1), y))
                } else {
                    space.post(new Conjunction(nextConstraintId(), maybeGoal, xs, y))
                }
                y
            }
            def functionalCase = {
                post(y)
                Nil
            }
            def generalCase = {
                val costs = if (xs.size == 1) xs(0) else post(createBoolChannel)
                if (compilesToConst(b, True)) {
                    List(costs)
                } else {
                    val result = createBoolChannel
                    space.post(new Eq[BooleanValue](nextConstraintId(), maybeGoal, costs, y, result))
                    List(result)
                }
            }
            compileConstraint(constraint, xs, List(y), functionalCase, generalCase)
        case Constraint("array_bool_or", List(as, b), _) =>
            val as1 = ArrayConst(getArrayElems(as).iterator.filter(a => ! compilesToConst(a, False)).toList)
            val as2 = if (as1.value.isEmpty) ArrayConst(List(BoolConst(false))) else as1
            val xs = compileBoolArray(as2)
            val y = compileBoolExpr(b)
            def post(y: BooleanVariable): BooleanVariable = {
                if (xs.size == 2) {
                    space.post(new Or(nextConstraintId(), maybeGoal, xs(0), xs(1), y))
                } else {
                    space.post(new Disjunction(nextConstraintId(), maybeGoal, xs, y))
                }
                y
            }
            def functionalCase = {
                post(y)
                Nil
            }
            def generalCase = {
                val costs = if (xs.size == 1) xs(0) else post(createBoolChannel)
                if (compilesToConst(b, True)) {
                    List(costs)
                } else {
                    val result = createBoolChannel
                    space.post(new Eq[BooleanValue](nextConstraintId(), maybeGoal, costs, y, result))
                    List(result)
                }
            }
            compileConstraint(constraint, xs, List(y), functionalCase, generalCase)
        case Constraint("array_bool_xor", List(as), _) =>
            val xs = compileBoolArray(as)
            val maybeY =
                xs
                .filter(y => definesVar(constraint, xs.filter(_ != y), y))
                .sortWith((x, y) => definesVar(constraint, x) && ! definesVar(constraint, y))
                .headOption
            if (maybeY.isDefined) {
                val y = maybeY.get
                val trueCount = createNonNegativeIntChannel
                space.post(new CountConst[BooleanValue](nextConstraintId(), maybeGoal, xs.filter(_ != y), True, trueCount))
                space.post(new Even(nextConstraintId(), maybeGoal, trueCount, y))
                Nil
            } else {
                val trueCount = createNonNegativeIntChannel
                space.post(new CountConst[BooleanValue](nextConstraintId(), maybeGoal, xs, True, trueCount))
                val costs = createBoolChannel
                space.post(new Uneven[IntegerValue](nextConstraintId(), maybeGoal, trueCount, costs))
                List(costs)
            }
        case Constraint("bool_clause", List(ArrayConst(List(a)), ArrayConst(List(b))), _) =>
            compileConstraint(maybeGoal, Constraint("bool_le", List(b, a), Nil))
        case Constraint("bool_clause", List(as, bs), _) =>
            // as are positive literals, bs are negative literals
            (getArrayElems(as).iterator.filter(a => ! compilesToConst(a, False)).toList,
             getArrayElems(bs).iterator.filter(b => ! compilesToConst(b, True)).toList) match {
                case (Nil, Nil) => throw new InconsistentConstraintException(constraint)
                case (Nil, _) => compileConstraint(maybeGoal, Constraint("array_bool_and", List(bs, BoolConst(false)), Nil))
                case (_, Nil) => compileConstraint(maybeGoal, Constraint("array_bool_or", List(as, BoolConst(true)), Nil))
                case _ =>
                    val List(costs0) = compileConstraint(maybeGoal, Constraint("array_bool_or", List(as, BoolConst(true)), Nil)).toList
                    val List(costs1) = compileConstraint(maybeGoal, Constraint("array_bool_and", List(bs, BoolConst(true)), Nil)).toList
                    val costs = createBoolChannel
                    space.post(new Le[BooleanValue](nextConstraintId(), maybeGoal, costs1, costs0, costs))
                    List(costs)
            }
        case Constraint("int_eq", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Eq[IntegerValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("int_eq_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Eq[IntegerValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("int_ne", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Ne[IntegerValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("int_ne_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Ne[IntegerValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("int_lt", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Lt[IntegerValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("int_lt_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Lt[IntegerValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("int_le", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Le[IntegerValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("int_le_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Le[IntegerValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("int_min", _, _) =>
            compileTernaryIntConstraint(new Min(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("int_max", _, _) =>
            compileTernaryIntConstraint(new Max(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("int_plus", List(a, b, c), annotations) =>
            compileConstraint(
                maybeGoal,
                Constraint("int_lin_eq", List(ArrayConst(List(IntConst(1), IntConst(1))), ArrayConst(List(a, b)), c), annotations))
        case Constraint("int_minus", List(a, b, c), annotations) =>
            compileConstraint(
                maybeGoal,
                Constraint("int_lin_eq", List(ArrayConst(List(IntConst(1), IntConst(-1))), ArrayConst(List(a, b)), c), annotations))
        case Constraint("int_times", List(a, b, c), _) =>
            compileTernaryIntConstraint(new Times(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("int_div", List(a, b, c), _) =>
            compileTernaryIntConstraint(new Div(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("int_mod", List(a, b, c), _) =>
            compileTernaryIntConstraint(new Mod(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("int_pow", List(a, b, c), _) =>
            compileTernaryIntConstraint(new Power(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("int_abs", List(a, b), _) =>
            compileBinaryConstraint[IntegerValue, IntegerVariable, IntegerValue, IntegerVariable](new Abs(_, _, _, _), maybeGoal, constraint)
       // expansion of terms in parameters
        case Constraint(IntLin(name), (as @ Term(_, _)) :: t, _) =>
            compileConstraint(maybeGoal, constraint.copy(params = ArrayConst(getArrayElems(as).toList) :: t))
        case Constraint(IntLin(name), as :: (bs @ Term(_, _)) :: t, _) =>
            compileConstraint(maybeGoal, constraint.copy(params = as :: ArrayConst(getArrayElems(bs).toList) :: t))
        case Constraint(IntLin(name), as :: bs :: c :: t, _) if ! c.isConst && compilesToConst(c) =>
            compileConstraint(maybeGoal, constraint.copy(params = as :: bs :: IntConst(getConst[IntegerValue](c).value) :: t))
        // -1 * x <op> c -> 1 * x <op> -c where op in {==, !=}
        case Constraint(IntLin(name), ArrayConst(List(IntConst(-1))) :: bs :: IntConst(c) :: t, _)
        if name == "eq" || name == "ne" =>
            compileConstraint(maybeGoal, constraint.copy(params = ArrayConst(List(IntConst(1))) :: bs :: IntConst(-c) :: t))
        // 1 * x <op> c -> x <op> c
        case Constraint(IntLin(name), ArrayConst(List(IntConst(1))) :: ArrayConst(bs) :: c :: t, annotations) =>
            compileConstraint(maybeGoal, Constraint("int_" + name, bs.head :: c :: t, annotations))
        // -1 * x + 1 * y <op> 0 -> 1 * y + -1 * x <op> 0
        case Constraint(
            IntLin(name),
            ArrayConst(List(IntConst(-1), IntConst(1))) :: ArrayConst(List(x, y)) :: (c @ IntConst(0)) :: t, annotations) =>
            compileConstraint(
                maybeGoal,
                constraint.copy(params = ArrayConst(List(IntConst(1), IntConst(-1))) :: ArrayConst(List(y, x)) :: c :: t))
        // 1 * x + -1 * y <op> 0 -> x <op> y
        case Constraint(
            IntLin(name),
            ArrayConst(List(IntConst(1), IntConst(-1))) :: ArrayConst(List(x, y)) :: IntConst(0) :: t, annotations) =>
            compileConstraint(maybeGoal, Constraint("int_" + name, x :: y :: t, annotations))
        case Constraint(
            "int_lin_eq",
            List(ArrayConst(as), ArrayConst(bs), c), annotations)
            if (! definesVar(constraint, c) &&
                as.iterator.zip(bs.iterator).exists{
                    case ((IntConst(a), b)) => (a == -1 || a == 1) && definesVar(constraint, b)}) =>
            val abs = as.zip(bs)
            val (a, b) = abs.find{case ((IntConst(a), b)) => (a == -1 || a == 1) && definesVar(constraint, b)}.get
            a match {
                case IntConst(1) =>
                    // b1 + a2 b2 + ... = c
                    // b1               = c - a2 b2 - ...
                    val (as1, bs1) = (for ((IntConst(a), b1) <- abs if b1 != b) yield (IntConst(-a), b1)).unzip
                    compileConstraint(
                        maybeGoal,
                        Constraint("int_lin_eq", List(ArrayConst(IntConst(1) :: as1), ArrayConst(c :: bs1), b), annotations))
                case IntConst(-1) =>
                    // -1 b1 + a2 b2 + ... =    c
                    // -1 b1               =    c - a2 b2 - ...
                    //    b1               = -1 c + a2 b2 + ...
                    val bs1 = for (b1 <- bs) yield if (b1 == b) c else b1
                    compileConstraint(maybeGoal, Constraint("int_lin_eq", List(ArrayConst(as), ArrayConst(bs1), b), annotations))
            }
        case Constraint("int_lin_eq", List(as, bs, c), _) =>
            def functionalCase = {
                compileLinearCombination[IntegerValue](maybeGoal, as, bs, Some(c))
                Nil
            }
            def generalCase = {
                List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, EqRelation, c))
            }
            compileConstraint(constraint, bs, c, functionalCase, generalCase)
        case Constraint("int_lin_eq_reif", List(as, bs, c, r), _) =>
            def functionalCase = {
                compileLinearConstraint[IntegerValue](maybeGoal, as, bs, EqRelation, c, Some(r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, bs, r, functionalCase, generalCase)
        case Constraint("int_lin_ne", List(as, bs, c), _) =>
            List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, NeRelation, c))
        case Constraint("int_lin_ne_reif", List(as, bs, c, r), _) =>
            def functionalCase = {
                compileLinearConstraint[IntegerValue](maybeGoal, as, bs, NeRelation, c, Some(r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, bs, r, functionalCase, generalCase)
        case Constraint("int_lin_le", List(as, bs, c), _) =>
            List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, LeRelation, c))
        case Constraint("int_lin_le_reif", List(as, bs, c, r), _) =>
            def functionalCase = {
                compileLinearConstraint[IntegerValue](maybeGoal, as, bs, LeRelation, c, Some(r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, bs, r, functionalCase, generalCase)
        case Constraint("array_int_maximum", List(b, as), _) =>
            val xs = compileIntArray(as)
            val y = compileIntExpr(b)
            def functionalCase = {
                space.post(new Maximum[IntegerValue](nextConstraintId(), maybeGoal, xs, y))
                Nil
            }
            def generalCase = {
                val max = createIntChannel
                space.post(new Maximum[IntegerValue](nextConstraintId(), maybeGoal, xs, max))
                val costs = createBoolChannel
                space.post(new Eq[IntegerValue](nextConstraintId(), maybeGoal, max, y, costs))
                List(costs)
            }
            compileConstraint(constraint, xs, List(y), functionalCase, generalCase)
        case Constraint("array_int_minimum", List(b, as), _) =>
            val xs = compileIntArray(as)
            val y = compileIntExpr(b)
            def functionalCase = {
                space.post(new Minimum[IntegerValue](nextConstraintId(), maybeGoal, xs, y))
                Nil
            }
            def generalCase = {
                val max = createIntChannel
                space.post(new Minimum[IntegerValue](nextConstraintId(), maybeGoal, xs, max))
                val costs = createBoolChannel
                space.post(new Eq[IntegerValue](nextConstraintId(), maybeGoal, max, y, costs))
                List(costs)
            }
            compileConstraint(constraint, xs, List(y), functionalCase, generalCase)
        case Constraint("array_var_bool_element" | "array_bool_element" | "yuck_array_bool_element" , _, _) =>
            compileElementConstraint[BooleanValue](maybeGoal, constraint)
        case Constraint("array_var_int_element" | "array_int_element" | "yuck_array_int_element" , _, _) =>
            compileElementConstraint[IntegerValue](maybeGoal, constraint)
        case Constraint("array_var_set_element" | "array_set_element" | "yuck_array_set_element" , _, _) =>
            compileElementConstraint[IntegerSetValue](maybeGoal, constraint)
        case Constraint("yuck_if_then_else_var_bool" | "yuck_if_then_else_bool", _, _) =>
            compileIfThenElseConstraint[BooleanValue](maybeGoal, constraint)
        case Constraint("yuck_if_then_else_var_int" | "yuck_if_then_else_int", _, _) =>
            compileIfThenElseConstraint[IntegerValue](maybeGoal, constraint)
        case Constraint("yuck_if_then_else_var_set" | "yuck_if_then_else_set", _, _) =>
            compileIfThenElseConstraint[IntegerSetValue](maybeGoal, constraint)
        case Constraint("set_eq", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Eq[IntegerSetValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_eq_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Eq[IntegerSetValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("set_ne", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Ne[IntegerSetValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_ne_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Ne[IntegerSetValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("set_lt", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Lt[IntegerSetValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_lt_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Lt[IntegerSetValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("set_le", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Le[IntegerSetValue](nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_le_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Le[IntegerSetValue](nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("set_card", List(a, b), _) =>
            compileBinaryConstraint[IntegerSetValue, IntegerSetVariable, IntegerValue, IntegerVariable](new SetCardinality(_, _, _, _), maybeGoal, constraint)
        case Constraint("set_in", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Contains(nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_in_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Contains(nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("set_subset", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Subset(nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_subset_reif", List(a, b, r), _) =>
            def functionalCase = {
                space.post(new Subset(nextConstraintId(), maybeGoal, a, b, r))
                Nil
            }
            def generalCase = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, functionalCase, generalCase)
        case Constraint("set_intersect", List(a, b, c), _) =>
            compileTernaryIntSetConstraint(new SetIntersection(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("set_union", List(a, b, c), _) =>
            compileTernaryIntSetConstraint(new SetUnion(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("set_diff", List(a, b, c), _) =>
            compileTernaryIntSetConstraint(new SetDifference(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("set_symdiff", List(a, b, c), _) =>
            compileTernaryIntSetConstraint(
                new SymmetricalSetDifference(_, _, _, _, _), maybeGoal, constraint)
        case Constraint("fzn_all_different_int", List(as), _) =>
            val xs = compileIntArray(as)
            val costs = createBoolChannel
            space.post(new Alldistinct(nextConstraintId(), maybeGoal, xs, costs))
            List(costs)
        case Constraint("fzn_all_different_set", List(as), _) =>
            val xs = compileIntSetArray(as)
            val costs = createBoolChannel
            space.post(new Alldistinct(nextConstraintId(), maybeGoal, xs, costs))
            List(costs)
        case Constraint("fzn_alldifferent_except_0", List(as), _) =>
            val xs = compileIntArray(as)
            val costs = createBoolChannel
            space.post(new AlldistinctExceptZero(nextConstraintId(), maybeGoal, xs, costs))
            List(costs)
        case Constraint("fzn_nvalue", List(n0, as), _) =>
            val xs = compileIntArray(as)
            val n = compileIntExpr(n0)
            def functionalCase = {
                space.post(new NumberOfDistinctValues[IntegerValue](nextConstraintId(), maybeGoal, xs, n))
                Nil
            }
            def generalCase = {
                val m = createNonNegativeIntChannel
                space.post(new NumberOfDistinctValues[IntegerValue](nextConstraintId(), maybeGoal, xs, m))
                val costs = createBoolChannel
                space.post(new Eq[IntegerValue](nextConstraintId(), maybeGoal, m, n, costs))
                List(costs)
            }
            compileConstraint(constraint, xs, List(n), functionalCase, generalCase)
        case Constraint("fzn_count_eq", _, _) =>
            compileCountConstraint[IntegerValue](maybeGoal, constraint, new Eq[IntegerValue](_, _, _, _, _))
        case Constraint("fzn_member_bool", _, _) =>
            compileMemberConstraint[BooleanValue](maybeGoal, constraint)
        case Constraint("fzn_member_int", _, _) =>
            compileMemberConstraint[IntegerValue](maybeGoal, constraint)
        case Constraint("fzn_member_set", _, _) =>
            compileMemberConstraint[IntegerSetValue](maybeGoal, constraint)
        case Constraint("fzn_cumulative", List(s, d, r, b), _) =>
            val xs = compileIntArray(s)
            val ys = compileIntArray(d)
            val zs = compileIntArray(r)
            assert(xs.size == ys.size)
            assert(ys.size == zs.size)
            val tasks = for (((x, y), z) <- xs.zip(ys).zip(zs)) yield new CumulativeTask(x, y, z)
            val costs = createBoolChannel
            space.post(new Cumulative(nextConstraintId(), maybeGoal, tasks, b, costs))
            List(costs)
        case Constraint("yuck_disjunctive", List(x, w, BoolConst(strict)), _) =>
            val xs = compileIntArray(x)
            val ws = compileIntArray(w)
            assert(xs.size == ws.size)
            val y = compileConstant(Zero)
            val h = compileConstant(One)
            val rects = for (i <- xs.indices) yield new Disjoint2Rect(xs(i), y, ws(i), h)
            val costs = createBoolChannel
            space.post(new Disjoint2(nextConstraintId(), maybeGoal, rects.to(immutable.ArraySeq), strict, costs))
            List(costs)
        case Constraint("yuck_diffn", List(x, y, w, h, BoolConst(strict)), _) =>
            val xs = compileIntArray(x)
            val ys = compileIntArray(y)
            val ws = compileIntArray(w)
            val hs = compileIntArray(h)
            assert(xs.size == ys.size)
            assert(xs.size == ws.size)
            assert(xs.size == hs.size)
            val rects = for (i <- xs.indices) yield new Disjoint2Rect(xs(i), ys(i), ws(i), hs(i))
            val costs = createBoolChannel
            space.post(new Disjoint2(nextConstraintId(), maybeGoal, rects.to(immutable.ArraySeq), strict, costs))
            List(costs)
        case Constraint("fzn_table_bool", List(as, flatTable), _) =>
            val xs = compileBoolArray(as)
            val rows = compileBoolArray(flatTable).map(_.domain.singleValue).grouped(xs.size).to(immutable.ArraySeq)
            val costs = createBoolChannel
            val forceImplicitSolving = constraint.annotations.exists(forcesImplicitSolving)
            space.post(new Table(nextConstraintId(), maybeGoal, xs, rows, costs, forceImplicitSolving))
            List(costs)
        case Constraint("fzn_table_int", List(as, flatTable), _) =>
            val xs = compileIntArray(as)
            val rows = compileIntArray(flatTable).map(_.domain.singleValue).grouped(xs.size).to(immutable.ArraySeq)
            val costs = createBoolChannel
            val forceImplicitSolving = constraint.annotations.exists(forcesImplicitSolving)
            space.post(new Table(nextConstraintId(), maybeGoal, xs, rows, costs, forceImplicitSolving))
            List(costs)
        case Constraint("fzn_regular", List(xs, IntConst(q), IntConst(s), flatDelta, IntConst(q0), f), _) =>
            val Q = q
            val S = s
            val delta = compileIntArray(flatDelta).map(_.domain.singleValue.value).grouped(s).to(immutable.ArraySeq)
            val F = compileIntSetExpr(f).domain.singleValue.set
            val costs = createBoolChannel
            space.post(new Regular(nextConstraintId(), maybeGoal, xs, Q, S, delta, q0, F, costs))
            List(costs)
        case Constraint("yuck_circuit", List(succ, IntConst(offset)), _) =>
            val costs = createBoolChannel
            space.post(new Circuit(nextConstraintId(), maybeGoal, succ, offset, costs))
            List(costs)
        case Constraint("yuck_delivery", _, _) =>
            compileDeliveryConstraint[IntegerValue](maybeGoal, constraint)
        case Constraint("yuck_inverse", List(f, IntConst(fOffset), g, IntConst(gOffset)), _) =>
            val costs = createBoolChannel
            val constraint = new Inverse(nextConstraintId(), maybeGoal, new InverseFunction(f, fOffset), new InverseFunction(g, gOffset), costs)
            val constraints = constraint.decompose(space)
            constraints.foreach(space.post)
            constraints.view.flatMap(_.outVariables).map(_.asInstanceOf[BooleanVariable])
        case Constraint("yuck_bin_packing_load", List(loads0, bins0, weights0, IntConst(minLoadIndex)), _) =>
            val bins = compileIntArray(bins0)
            val weights = getArrayElems(weights0).map(getConst[IntegerValue](_))
            require(bins.size == weights.size)
            val itemGenerator =
                for ((bin, weight) <- bins.iterator.zip(weights.iterator)) yield
                    new BinPackingItem(bin, weight)
            val items = itemGenerator.toIndexedSeq
            val loads1 = compileIntArray(loads0)
            val loads = (minLoadIndex until minLoadIndex + loads1.size).iterator.zip(loads1.iterator).toMap
            compileBinPackingConstraint(maybeGoal, constraint, items, loads)
        case Constraint("fzn_global_cardinality", List(xs0, cover0, counts0), _) =>
            val xs = compileIntArray(xs0)
            val items = xs.map(new BinPackingItem(_, One))
            val cover = getArrayElems(cover0).map(getConst[IntegerValue](_).value)
            val counts = compileIntArray(counts0)
            require(cover.size == counts.size)
            val loads = cover.iterator.zip(counts.iterator).toMap
            compileBinPackingConstraint(maybeGoal, constraint, items, loads)
        case Constraint("fzn_lex_less_int", List(as, bs), _) =>
            val xs = compileIntArray(as)
            val ys = compileIntArray(bs)
            val costs = createBoolChannel
            space.post(new LexLess(nextConstraintId(), maybeGoal, xs, ys, costs))
            List(costs)
        case Constraint("fzn_lex_less_bool", List(as, bs), _) =>
            val xs = compileBoolArray(as)
            val ys = compileBoolArray(bs)
            val costs = createBoolChannel
            space.post(new LexLess(nextConstraintId(), maybeGoal, xs, ys, costs)(booleanOrdering))
            List(costs)
        case Constraint("fzn_lex_less_set", List(as, bs), _) =>
            val xs = compileIntSetArray(as)
            val ys = compileIntSetArray(bs)
            val costs = createBoolChannel
            space.post(new LexLess(nextConstraintId(), maybeGoal, xs, ys, costs))
            List(costs)
        case Constraint("fzn_lex_lesseq_int", List(as, bs), _) =>
            val xs = compileIntArray(as)
            val ys = compileIntArray(bs)
            val costs = createBoolChannel
            space.post(new LexLessEq(nextConstraintId(), maybeGoal, xs, ys, costs))
            List(costs)
        case Constraint("fzn_lex_lesseq_bool", List(as, bs), _) =>
            val xs = compileBoolArray(as)
            val ys = compileBoolArray(bs)
            val costs = createBoolChannel
            space.post(new LexLessEq(nextConstraintId(), maybeGoal, xs, ys, costs)(booleanOrdering))
            List(costs)
        case Constraint("fzn_lex_lesseq_set", List(as, bs), _) =>
            val xs = compileIntSetArray(as)
            val ys = compileIntSetArray(bs)
            val costs = createBoolChannel
            space.post(new LexLessEq(nextConstraintId(), maybeGoal, xs, ys, costs))
            List(costs)
        case Constraint(Reif(name), _, _) =>
            compileReifiedConstraint(maybeGoal, constraint)
    }

    private type BinaryConstraintFactory
        [InVariable <: AnyVariable, OutVariable <: AnyVariable] =
        (Id[yuck.core.Constraint], Option[Goal], InVariable, OutVariable) => yuck.core.Constraint

    private def compileBinaryConstraint
        [InValue <: AnyValue, InVariable <: Variable[InValue],
         OutValue <: OrderedValue[OutValue], OutVariable <: OrderedVariable[OutValue]]
        (constraintFactory: BinaryConstraintFactory[InVariable, OutVariable],
         maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (implicit
         inHelper: CompilationHelper[InValue, InVariable],
         inValueTraits: ValueTraits[InValue],
         outHelper: OrderedCompilationHelper[OutValue, OutVariable],
         outValueTraits: OrderedValueTraits[OutValue]):
        Iterable[BooleanVariable] =
    {
        val List(a, b) = constraint.params
        val x = inHelper.compileExpr(a)
        val y = outHelper.compileExpr(b)
        def functionalCase = {
            space.post(constraintFactory(nextConstraintId(), maybeGoal, x, y))
            Nil
        }
        def generalCase = {
            val channel = outHelper.createChannel
            space.post(constraintFactory(nextConstraintId(), maybeGoal, x, channel))
            val costs = createBoolChannel
            space.post(new Eq[OutValue](nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, List(a), b, functionalCase, generalCase)
    }

    private val compileBinaryIntConstraint =
        compileBinaryConstraint[IntegerValue, IntegerVariable, IntegerValue, IntegerVariable] _

    private type TernaryConstraintFactory
        [In1Variable <: AnyVariable, In2Variable <: AnyVariable, OutVariable <: AnyVariable] =
        (Id[yuck.core.Constraint], Option[Goal], In1Variable, In2Variable, OutVariable) => yuck.core.Constraint

    private def compileTernaryConstraint
        [In1Value <: AnyValue, In1Variable <: Variable[In1Value],
         In2Value <: AnyValue, In2Variable <: Variable[In2Value],
         OutValue <: OrderedValue[OutValue], OutVariable <: OrderedVariable[OutValue]]
        (constraintFactory: TernaryConstraintFactory[In1Variable, In2Variable, OutVariable],
         maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (implicit
         in1Helper: CompilationHelper[In1Value, In1Variable],
         in2Helper: CompilationHelper[In2Value, In2Variable],
         outHelper: OrderedCompilationHelper[OutValue, OutVariable],
         outValueTraits: OrderedValueTraits[OutValue]):
        Iterable[BooleanVariable] =
    {
        val List(a, b, c) = constraint.params
        val x = in1Helper.compileExpr(a)
        val y = in2Helper.compileExpr(b)
        val z = outHelper.compileExpr(c)
        def functionalCase = {
            space.post(constraintFactory(nextConstraintId(), maybeGoal, x, y, z))
            Nil
        }
        def generalCase = {
            val channel = outHelper.createChannel
            space.post(constraintFactory(nextConstraintId(), maybeGoal, x, y, channel))
            val costs = createBoolChannel
            space.post(new Eq[OutValue](nextConstraintId(), maybeGoal, channel, z, costs))
            List(costs)
        }
        compileConstraint(constraint, List(a, b), c, functionalCase, generalCase)
    }

    private val compileTernaryBoolConstraint =
        compileTernaryConstraint[BooleanValue, BooleanVariable, BooleanValue, BooleanVariable, BooleanValue, BooleanVariable] _

    private val compileTernaryIntConstraint =
        compileTernaryConstraint[IntegerValue, IntegerVariable, IntegerValue, IntegerVariable, IntegerValue, IntegerVariable] _

    private val compileTernaryIntSetConstraint =
        compileTernaryConstraint[IntegerSetValue, IntegerSetVariable, IntegerSetValue, IntegerSetVariable, IntegerSetValue, IntegerSetVariable] _

    private def compileBinPackingConstraint
        [Load <: NumericalValue[Load]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         items: immutable.Seq[BinPackingItem[Load]],
         loads: immutable.Map[Int, NumericalVariable[Load]]) // bin -> load
        (implicit loadTraits: NumericalValueTraits[Load]):
        Iterable[BooleanVariable] =
    {
        require(items.forall(_.weight >= loadTraits.zero))
        val items1: immutable.IndexedSeq[BinPackingItem[Load]] =
            items
            .foldLeft(immutable.Map.empty[IntegerVariable, Load]){
                case (map, item) => map.updated(item.bin, map.getOrElse(item.bin, loadTraits.zero) + item.weight)
            }
            .iterator
            .filter{case (_, weight) => weight > loadTraits.zero}
            .map{case (bin, weight) => new BinPackingItem(bin, weight)}
            .toIndexedSeq
        val bins = items1.map(_.bin)
        def functionalCase = {
            space.post(new BinPacking[Load](nextConstraintId(), maybeGoal, items1, loads))
            Nil
        }
        def generalCase = {
            val loads1: immutable.Map[Int, NumericalVariable[Load]] = {
                val definedVars = new mutable.HashSet[NumericalVariable[Load]]
                for ((bin, load) <- loads) yield
                    if (! definedVars.contains(load) && definesVar(constraint, bins, loads(bin))) {
                        definedVars += load
                        bin -> load
                    }
                    else bin -> createNonNegativeChannel[Load]
            }
            space.post(new BinPacking[Load](nextConstraintId(), maybeGoal, items1, loads1))
            val deltas: immutable.Iterable[BooleanVariable] =
                for ((bin, load) <- loads if load != loads1(bin)) yield {
                    val delta = createBoolChannel
                    space.post(new Eq[Load](nextConstraintId(), maybeGoal, load, loads1(bin), delta))
                    delta
                }
            deltas
        }
        compileConstraint(constraint, bins, loads.values, functionalCase, generalCase)
    }

    private def compileDeliveryConstraint
        [Time <: NumericalValue[Time]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (implicit timeTraits: NumericalValueTraits[Time]):
        Iterable[BooleanVariable] =
    {
        val List(startNodes0, endNodes0, succ0, IntConst(offset), arrivalTimes0, serviceTimes0, travelTimes0,
                    BoolConst(withWaiting), totalTravelTime0) =
            constraint.params
        val startNodes = compileIntSetExpr(startNodes0).domain.singleValue.set
        val endNodes = compileIntSetExpr(endNodes0).domain.singleValue.set
        val succ = compileIntArray(succ0)
        val nodes = IntegerRange(offset, offset + succ.size - 1)
        val arrivalTimes = compileNumArray[Time](arrivalTimes0)
        val serviceTimes1 = compileNumArray[Time](serviceTimes0).map(_.domain.singleValue)
        require(serviceTimes1.isEmpty || serviceTimes1.size == nodes.size)
        val serviceTimes: Int => Time =
            if (serviceTimes1.isEmpty) _ => timeTraits.zero else i => serviceTimes1(i)
        val travelTimes1 =
            compileNumArray[Time](travelTimes0).map(_.domain.singleValue).grouped(arrivalTimes.size)
                 .to(immutable.ArraySeq)
        require(travelTimes1.isEmpty || (travelTimes1.size == nodes.size && travelTimes1.forall(_.size == nodes.size)))
        val travelTimesAreSymmetric =
            travelTimes1.isEmpty ||
            Range(0, nodes.size).forall(
                i => Range(i + 1, nodes.size).forall(j => travelTimes1(i)(j) == travelTimes1(j)(i)))
        val travelTimes2 =
            if (! travelTimes1.isEmpty && travelTimesAreSymmetric)
                (for (i <- 0 until nodes.size) yield travelTimes1(i).drop(i)).to(immutable.ArraySeq)
            else travelTimes1
        val travelTimes: (Int, Int) => Time =
            if (travelTimes2.isEmpty) (_, _) => timeTraits.zero
            else if (travelTimesAreSymmetric) (i, j) => if (i <= j) travelTimes2(i)(j - i) else travelTimes2(j)(i - j)
            else (i, j) => travelTimes2(i)(j)
        val totalTravelTime1 = compileNumExpr[Time](totalTravelTime0)
        val totalTravelTime = {
            if (travelTimes2.isEmpty) {
                // avoid an Eq constraint
                totalTravelTime1.pruneDomain(timeTraits.createDomain(Set(timeTraits.zero)))
                createNumChannel[Time]
            } else {
                totalTravelTime1
            }
        }
        val costs = createBoolChannel
        def functionalCase = {
            val delivery =
                new Delivery[Time](
                    WeakReference(space), nextConstraintId(), maybeGoal,
                    startNodes, endNodes, succ, offset, arrivalTimes, serviceTimes, travelTimes,
                    withWaiting, totalTravelTime, costs)
            space.post(delivery)
            List(costs)
        }
        def generalCase = {
            val nodes = IntegerRange(offset, offset + succ.size - 1)
            val arrivalTimes1: immutable.IndexedSeq[NumericalVariable[Time]] = {
                val definableVars = this.definedVars(constraint)
                val definedVars = new mutable.HashSet[NumericalVariable[Time]]
                for (i <- nodes.values.toIndexedSeq) yield {
                    val arrivalTime = arrivalTimes(i.value - offset)
                    if (startNodes.contains(i)) {
                        arrivalTime
                    }
                    else if (definableVars.contains(arrivalTime) && ! definedVars.contains(arrivalTime) &&
                             isViableConstraint(succ, arrivalTime))
                    {
                        definedVars += arrivalTime
                        arrivalTime
                    }
                    else timeTraits.createVariable(space, "", arrivalTime.domain)
                }
            }
            val totalTravelTime1: NumericalVariable[Time] =
                if (isViableConstraint(succ, totalTravelTime)) totalTravelTime
                else timeTraits.createVariable(space, "", totalTravelTime.domain)
            space.post(
                new Delivery[Time](
                    WeakReference(space), nextConstraintId(), maybeGoal,
                    startNodes, endNodes, succ, offset, arrivalTimes1, serviceTimes, travelTimes,
                    withWaiting, totalTravelTime1, costs))
            val pairs = (arrivalTimes :+ totalTravelTime).zip(arrivalTimes1 :+ totalTravelTime1)
            val deltas =
                for ((x, x1) <- pairs if x != x1) yield {
                    val delta = createBoolChannel
                    space.post(new Eq[Time](nextConstraintId(), maybeGoal, x, x1, delta))
                    delta
                }
            deltas.view :+ costs
        }
        compileConstraint(
            constraint,
            startNodes.values.view.map(i => arrivalTimes(i.value - offset)) ++ succ,
            nodes.diff(startNodes).values.view.map(i => arrivalTimes(i.value - offset)) ++ Seq(totalTravelTime),
            functionalCase, generalCase)
    }

    private def compileLinearCombination
        [V <: NumericalValue[V]]
        (maybeGoal: Option[Goal],
         as0: Expr, bs: Expr,
         maybeChannel: Option[NumericalVariable[V]] = None)
        (implicit valueTraits: NumericalValueTraits[V]):
        NumericalVariable[V] =
    {
        val zero = valueTraits.zero
        val one = valueTraits.one
        val minusOne = one.negated
        val as = compileNumArray[V](as0)
        val xs = compileNumArray[V](bs)
        require(as.size == xs.size)
        val axs =
            AX.normalize(
                for ((x, y) <- as.view.zip(xs.view)
                     if x.domain.singleValue != zero && (! y.domain.isSingleton || y.domain.singleValue != zero))
                    yield new AX[V](x.domain.singleValue, y))
        axs match {
            case List(AX(`one`, x)) if maybeChannel.isEmpty =>
                x
            case List(AX(`one`, x), AX(`minusOne`, y)) =>
                val channel = maybeChannel.getOrElse(createNumChannel[V])
                space.post(new Minus[V](nextConstraintId(), maybeGoal, x, y, channel))
                channel
            case List(AX(`minusOne`, x), AX(`one`, y)) =>
                val channel = maybeChannel.getOrElse(createNumChannel[V])
                space.post(new Minus[V](nextConstraintId(), maybeGoal, y, x, channel))
                channel
            case _ =>
                val channel = maybeChannel.getOrElse(createNumChannel[V])
                if (axs.forall(_.a == one)) {
                    if (axs.size == 2) {
                        val List(AX(_, x), AX(_, y)) = axs
                        space.post(new Plus(nextConstraintId(), maybeGoal, x, y, channel))
                    } else {
                        val xs = axs.iterator.map(_.x).toIndexedSeq
                        space.post(new Sum(nextConstraintId(), maybeGoal, xs , channel))
                    }
                } else {
                   space.post(new LinearCombination(nextConstraintId(), maybeGoal, axs.toIndexedSeq, channel))
                }
                channel
        }
    }

    private def compileLinearConstraint
        [V <: NumericalValue[V]]
        (maybeGoal: Option[Goal],
         as0: Expr, bs: Expr, relation: OrderingRelation, c: Expr,
         maybeCosts: Option[BooleanVariable] = None)
        (implicit valueTraits: NumericalValueTraits[V]):
        BooleanVariable =
    {
        val zero = valueTraits.zero
        val as = compileNumArray[V](as0)
        val xs = compileNumArray[V](bs)
        require(as.size == xs.size)
        val axs =
            AX.normalize(
                for ((x, y) <- as.view.zip(xs.view)
                     if x.domain.singleValue != zero && (! y.domain.isSingleton || y.domain.singleValue != zero))
                    yield new AX[V](x.domain.singleValue, y))
        val y = createNumChannel[V]
        val z = compileNumExpr[V](c)
        val costs = maybeCosts.getOrElse(createBoolChannel)
        if (axs.forall(_.a == valueTraits.one)) {
            space.post(new SumConstraint(nextConstraintId(), maybeGoal, axs.map(_.x).to(immutable.ArraySeq), y, relation, z, costs))
        } else {
            space.post(new LinearConstraint(nextConstraintId(), maybeGoal, axs.to(immutable.ArraySeq), y, relation, z, costs))
        }
        costs
    }

    private def compileCountConstraint
        [V <: AnyValue]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         comparatorFactory: TernaryConstraintFactory[IntegerVariable, IntegerVariable, BooleanVariable])
        (implicit valueTraits: ValueTraits[V]):
        Iterable[BooleanVariable] =
    {
        val Constraint(_, List(as, a, b), _) = constraint
        val xs0 = compileArray[V](as)
        val y = compileExpr[V](a)
        // If xs(j) does not play a role (because its domain is disjoint from y.domain and hence
        // its values will never be counted), we omit xs(j) from the constraint and hence an
        // useless arc from the constraint network.
        val xs = xs0.filter(_.domain.intersects(y.domain))
        val m = compileIntExpr(b)
        if (compilesToConst(a)) {
            def functionalCase = {
                space.post(new CountConst[V](nextConstraintId(), maybeGoal, xs, y.domain.singleValue, m))
                Nil
            }
            def generalCase = {
                val n = createNonNegativeIntChannel
                space.post(new CountConst[V](nextConstraintId(), maybeGoal, xs, y.domain.singleValue, n))
                val costs = createBoolChannel
                space.post(comparatorFactory(nextConstraintId(), maybeGoal, n, m, costs))
                List(costs)
            }
            compileConstraint(constraint, xs, List(m), functionalCase, generalCase)
        } else {
            def functionalCase = {
                space.post(new CountVar[V](nextConstraintId(), maybeGoal, xs, y, m))
                Nil
            }
            def generalCase = {
                val n = createNonNegativeIntChannel
                space.post(new CountVar[V](nextConstraintId(), maybeGoal, xs, y, n))
                val costs = createBoolChannel
                space.post(comparatorFactory(nextConstraintId(), maybeGoal, n, m, costs))
                List(costs)
            }
            compileConstraint(constraint, xs :+ y, List(m), functionalCase, generalCase)
        }
    }

    private def compileMemberConstraint
        [V <: AnyValue]
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (implicit valueTraits: ValueTraits[V]):
        Iterable[BooleanVariable] =
        compileCountConstraint(
            maybeGoal,
            constraint.copy(id = "fzn_count_leq", params = constraint.params ++ List(IntConst(1))),
            new Eq[IntegerValue](_, _, _, _, _))

    private def compileElementConstraint
        [V <: OrderedValue[V]]
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (implicit valueTraits: OrderedValueTraits[V]):
        Iterable[BooleanVariable] =
    {
        val List(IntConst(offset0), b, as, c) =
            if (constraint.params.size == 4) constraint.params
            else IntConst(1) :: constraint.params
        val i = compileIntExpr(b)
        val xs0 = compileArray[V](as)
        val y = compileOrdExpr[V](c)
        val indexRange0 = createIntDomain(offset0, offset0 + xs0.size - 1)
        if (! i.domain.intersects(indexRange0)) {
            throw new InconsistentConstraintException(constraint)
        }
        // If xs0(j) does not play a role (because j is not in i.domain), then there is no
        // need to monitor xs0(j) and we either drop it or replace it by some xs0(j') with j' in i.domain
        // to omit a useless arc from the constraint network.
        val indexRange = indexRange0.intersect(i.domain.hull)
        val offset = indexRange.lb.value
        val xs1 = xs0.drop(max(0, i.domain.lb.value - indexRange0.lb.value)).take(indexRange.size)
        val xs =
            (for (j <- indexRange.values) yield xs1((if (i.domain.contains(j)) j else i.domain.lb).value - offset))
                .toIndexedSeq
        def post(y: OrderedVariable[V]): OrderedVariable[V] = {
            if (xs.forall(_.domain.isSingleton)) {
                val as = xs.map(_.domain.singleValue)
                space.post(new ElementConst[V](nextConstraintId(), maybeGoal, as, i, y, offset))
            } else {
                space.post(new ElementVar[V](nextConstraintId(), maybeGoal, xs, i, y, offset))
            }
            y
        }
        def functionalCase = {
            post(y)
            Nil
        }
        def generalCase = {
            val channel = post(createOrdChannel[V])
            val costs = createBoolChannel
            space.post(new Eq[V](nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, xs :+ i, List(y), functionalCase, generalCase)
    }

    private def compileIfThenElseConstraint
        [V <: OrderedValue[V]]
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (implicit valueTraits: OrderedValueTraits[V]):
        Iterable[BooleanVariable] =
    {
        val cs = compileBoolArray(constraint.params(0))
        val xs = compileArray[V](constraint.params(1))
        val y = compileOrdExpr[V](constraint.params(2))
        require(cs.size == xs.size)
        require(cs.size >= 2)
        require(cs.last.domain.isSingleton)
        require(cs.last.domain.singleValue == True)
        def post(y: OrderedVariable[V]): OrderedVariable[V] = {
            space.post(new IfThenElse[V](nextConstraintId(), maybeGoal, cs, xs, y))
            y
        }
        def functionalCase = {
            post(y)
            Nil
        }
        def generalCase = {
            val channel = post(createOrdChannel[V])
            val costs = createBoolChannel
            space.post(new Eq[V](nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, cs.view ++ xs, List(y), functionalCase, generalCase)
    }

    private def compileReifiedConstraint
        (maybeGoal: Option[Goal], reifiedConstraint: yuck.flatzinc.ast.Constraint):
        Iterable[BooleanVariable] =
    {
        val Constraint(Reif(name), params, annotations) = reifiedConstraint
        val constraint = Constraint(name, params.take(params.size - 1), annotations)
        val satisfied = compileBoolExpr(params.last)
        if (compilesToConst(params.last, True)) {
            if (impliedConstraints.contains(constraint)) Nil
            else compileConstraint(maybeGoal, constraint)
        } else if (impliedConstraints.contains(constraint)) {
            def functionalCase = {
                space.post(new Conjunction(nextConstraintId(), maybeGoal, Nil, satisfied))
                Nil
            }
            def generalCase = {
                List(satisfied)
            }
            compileConstraint(reifiedConstraint, Nil, List(satisfied), functionalCase, generalCase)
        } else {
            val costs0 = compileConstraint(maybeGoal, constraint).toIndexedSeq
            def functionalCase = {
                space.post(new Conjunction(nextConstraintId(), maybeGoal, costs0, satisfied))
                Nil
            }
            def generalCase = {
                val costs = createBoolChannel
                if (costs0.size == 1) {
                    space.post(new Eq(nextConstraintId(), maybeGoal, costs0.head, satisfied, costs))
                } else {
                    val costs1 = createBoolChannel
                    space.post(new Conjunction(nextConstraintId(), maybeGoal, costs0, costs1))
                    space.post(new Eq(nextConstraintId(), maybeGoal, costs1, satisfied, costs))
                }
                List(costs)
            }
            compileConstraint(reifiedConstraint, costs0, List(satisfied), functionalCase, generalCase)
        }
    }

}
