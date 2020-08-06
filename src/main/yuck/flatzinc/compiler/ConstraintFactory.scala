package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints._
import yuck.core._
import yuck.flatzinc.ast._
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
    (cc: CompilationContext, randomGenerator: RandomGenerator, sigint: Sigint)
    extends CompilationPhase(cc, randomGenerator)
{

    private val space = cc.space
    private val impliedConstraints = cc.impliedConstraints
    private val logger = cc.logger

    // Checks whether the given constraint is annotated with defined_var(a)
    // where a compiles to out.
    private def definesVar(
        constraint: yuck.flatzinc.ast.Constraint, out: AnyVariable): Boolean =
    {
        constraint
        .annotations
        .iterator
        .flatMap(_.term match {
            case Term("defines_var", List(a)) => List(compileAnyExpr(a))
            case Term("defines_vars", List(a)) => compileAnyArray(a)
            case _ => Nil
        })
        .contains(out)
    }

    private val fakeConstraintId = new Id[yuck.core.Constraint](-1)

    // Checks whether there is a functional dependency that could be exploited without
    // introducing a cycle.
    // Notice that this method may be quite expensive!
    private def definesVar(
        constraint: yuck.flatzinc.ast.Constraint, in: Iterable[AnyVariable], out: AnyVariable): Boolean =
    {
        ! out.domain.isSingleton &&
        ! cc.searchVars.contains(out) &&
        (definesVar(constraint, out) || ! cc.definedVars.contains(out)) &&
        space.maybeDefiningConstraint(out).isEmpty &&
        ! space.wouldIntroduceCycle(new DummyConstraint(fakeConstraintId, in, List(out)))
    }

    private def definesVar(
        constraint: yuck.flatzinc.ast.Constraint, out: Expr): Boolean =
        definesVar(constraint, compileAnyExpr(out))

    private def definesVar(
        constraint: yuck.flatzinc.ast.Constraint, in: Seq[Expr], out: Expr): Boolean =
        definesVar(constraint, in.map(compileAnyExpr), compileAnyExpr(out))

    private def definesVar(
        constraint: yuck.flatzinc.ast.Constraint, in: Expr, out: Expr): Boolean =
        definesVar(constraint, compileAnyArray(in), compileAnyExpr(out))

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         in: Seq[AnyVariable],
         out: AnyVariable,
         withFunctionalDependency: => Iterable[BooleanVariable],
         withoutFunctionalDependency: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
    {
        if (! out.domain.isSingleton &&
            ! cc.searchVars.contains(out) &&
            (definesVar(constraint, out) || ! cc.definedVars.contains(out)) &&
            space.maybeDefiningConstraint(out).isEmpty)
        {
            try {
                withFunctionalDependency
            }
            catch {
                case _: CyclicConstraintNetworkException => withoutFunctionalDependency
            }
        } else {
            withoutFunctionalDependency
        }
    }

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         in: Seq[Expr],
         out: Expr,
         withFunctionalDependency: => Iterable[BooleanVariable],
         withoutFunctionalDependency: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
        compileConstraint(constraint, in.map(compileAnyExpr), compileAnyExpr(out), withFunctionalDependency, withoutFunctionalDependency)

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         in: Expr,
         out: Expr,
         withFunctionalDependency: => Iterable[BooleanVariable],
         withoutFunctionalDependency: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
        compileConstraint(constraint, compileAnyArray(in), compileAnyExpr(out), withFunctionalDependency, withoutFunctionalDependency)

    override def run = {
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

    import HighPriorityImplicits._

    private def compileNonImplicitConstraint
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint):
        Iterable[BooleanVariable] =
        constraint match
    {
        // TODO Implement other direction!?
        case Constraint("bool2int", List(a, b), _) => {
            val x = compileBoolExpr(a)
            val y = compileIntExpr(b)
            def withFunctionalDependency = {
                space.post(new Bool2Int1(nextConstraintId, maybeGoal, x, y))
                Nil
            }
            def withoutFunctionalDependency = {
                val costs = createBoolChannel
                space.post(new Bool2Int2(nextConstraintId, maybeGoal, x, y, costs))
                List(costs)
            }
            compileConstraint(constraint, List(a), b, withFunctionalDependency, withoutFunctionalDependency)
        }
        case Constraint("bool2costs", List(a, b), _) => {
            val x = compileBoolExpr(a)
            val y = compileIntExpr(b)
            def withFunctionalDependency = {
                space.post(new Bool2Costs1(nextConstraintId, maybeGoal, x, y))
                Nil
            }
            def withoutFunctionalDependency = {
                val costs = createBoolChannel
                space.post(new Bool2Costs2(nextConstraintId, maybeGoal, x, y, costs))
                List(costs)
            }
            compileConstraint(constraint, List(a), b, withFunctionalDependency, withoutFunctionalDependency)
        }
        case Constraint("bool_not", List(a, b), _) =>
            def withFunctionalDependency = {
                space.post(new Not(nextConstraintId, maybeGoal, a, b))
                Nil
            }
            def withoutFunctionalDependency = {
                val costs = createBoolChannel
                space.post(new Ne[BooleanValue](nextConstraintId, maybeGoal, a, b, costs))
                List(costs)
            }
            compileConstraint(constraint, List(a), b, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("bool_eq", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Eq[BooleanValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("bool_eq_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Eq[BooleanValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("bool_lt", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Lt[BooleanValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("bool_lt_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Lt[BooleanValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("bool_le", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Le[BooleanValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("bool_le_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Le[BooleanValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("bool_xor", _, _) =>
            compileTernaryBoolConstraint(new Ne[BooleanValue](_, _, _, _, _), maybeGoal, constraint)
        case Constraint("array_bool_and", List(as, b), _) =>
            val as1 = ArrayConst(getArrayElems(as).iterator.filter(a => ! compilesToConst(a, True)).toList)
            val as2 = if (as1.value.isEmpty) ArrayConst(List(BoolConst(true))) else as1
            val xs = compileBoolArray(as2)
            val y = compileBoolExpr(b)
            def post(y: BooleanVariable): BooleanVariable = {
                if (xs.size == 2) {
                    space.post(new And(nextConstraintId, maybeGoal, xs(0), xs(1), y))
                } else if (xs.size > 2) {
                    space.post(new Conjunction(nextConstraintId, maybeGoal, xs, y))
                }
                y
            }
            def withFunctionalDependency = {
                post(y)
                Nil
            }
            def withoutFunctionalDependency = {
                val costs = if (xs.size == 1) xs(0) else post(createBoolChannel)
                if (compilesToConst(b, True)) {
                    // exists clause
                    List(costs)
                } else {
                    val result = createBoolChannel
                    space.post(new Eq[BooleanValue](nextConstraintId, maybeGoal, costs, y, result))
                    List(result)
                }
            }
            compileConstraint(constraint, xs, y, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("array_bool_or", List(as, b), _) =>
            val as1 = ArrayConst(getArrayElems(as).iterator.filter(a => ! compilesToConst(a, False)).toList)
            val as2 = if (as1.value.isEmpty) ArrayConst(List(BoolConst(false))) else as1
            val xs = compileBoolArray(as2)
            val y = compileBoolExpr(b)
            def post(y: BooleanVariable): BooleanVariable = {
                if (xs.size == 2) {
                    space.post(new Or(nextConstraintId, maybeGoal, xs(0), xs(1), y))
                } else if (xs.size > 2) {
                    space.post(new Disjunction(nextConstraintId, maybeGoal, xs, y))
                }
                y
            }
            def withFunctionalDependency = {
                post(y)
                Nil
            }
            def withoutFunctionalDependency = {
                val costs = if (xs.size == 1) xs(0) else post(createBoolChannel)
                if (compilesToConst(b, True)) {
                    // exists clause
                    List(costs)
                } else {
                    val result = createBoolChannel
                    space.post(new Eq[BooleanValue](nextConstraintId, maybeGoal, costs, y, result))
                    List(result)
                }
            }
            compileConstraint(constraint, xs, y, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("array_bool_xor", List(as), _) =>
            val xs = compileBoolArray(as)
            val maybeY =
                xs
                .sortWith((x, y) => definesVar(constraint, x) && ! definesVar(constraint, y))
                .filter(y => definesVar(constraint, xs.filter(_ != y), y))
                .headOption
            if (maybeY.isDefined) {
                val y = maybeY.get
                val trueCount = createNonNegativeIntChannel
                space.post(new CountConst[BooleanValue](nextConstraintId, maybeGoal, xs.filter(_ != y), True, trueCount))
                space.post(new Even(nextConstraintId, maybeGoal, trueCount, y))
                Nil
            } else {
                val trueCount = createNonNegativeIntChannel
                space.post(new CountConst[BooleanValue](nextConstraintId, maybeGoal, xs, True, trueCount))
                val costs = createBoolChannel
                space.post(new Uneven[IntegerValue](nextConstraintId, maybeGoal, trueCount, costs))
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
                    val List(costs0) = compileConstraint(maybeGoal, Constraint("array_bool_or", List(as, BoolConst(true)), Nil))
                    val List(costs1) = compileConstraint(maybeGoal, Constraint("array_bool_and", List(bs, BoolConst(true)), Nil))
                    val costs = createBoolChannel
                    space.post(new Le[BooleanValue](nextConstraintId, maybeGoal, costs1, costs0, costs))
                    List(costs)
            }
        case Constraint("int_eq", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Eq[IntegerValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("int_eq_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Eq[IntegerValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("int_ne", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Ne[IntegerValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("int_ne_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Ne[IntegerValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("int_lt", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Lt[IntegerValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("int_lt_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Lt[IntegerValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("int_le", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Le[IntegerValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("int_le_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Le[IntegerValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
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
            def withFunctionalDependency = {
                compileLinearCombination[IntegerValue](maybeGoal, as, bs, Some(c))
                Nil
            }
            def withoutFunctionalDependency = {
                List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, EqRelation, c))
            }
            compileConstraint(constraint, bs, c, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("int_lin_eq_reif", List(as, bs, c, r), _) =>
            def withFunctionalDependency = {
                compileLinearConstraint[IntegerValue](maybeGoal, as, bs, EqRelation, c, Some(r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, bs, r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("int_lin_ne", List(as, bs, c), _) =>
            List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, NeRelation, c))
        case Constraint("int_lin_ne_reif", List(as, bs, c, r), _) =>
            def withFunctionalDependency = {
                compileLinearConstraint[IntegerValue](maybeGoal, as, bs, NeRelation, c, Some(r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, bs, r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("int_lin_le", List(as, bs, c), _) =>
            List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, LeRelation, c))
        case Constraint("int_lin_le_reif", List(as, bs, c, r), _) =>
            def withFunctionalDependency = {
                compileLinearConstraint[IntegerValue](maybeGoal, as, bs, LeRelation, c, Some(r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, bs, r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("array_int_maximum", List(b, as), _) =>
            val xs = compileIntArray(as)
            val y = compileIntExpr(b)
            def withFunctionalDependency = {
                space.post(new Maximum[IntegerValue](nextConstraintId, maybeGoal, xs, y))
                Nil
            }
            def withoutFunctionalDependency = {
                val max = createIntChannel
                space.post(new Maximum[IntegerValue](nextConstraintId, maybeGoal, xs, max))
                val costs = createBoolChannel
                space.post(new Eq[IntegerValue](nextConstraintId, maybeGoal, max, y, costs))
                List(costs)
            }
            compileConstraint(constraint, xs, y, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("array_int_minimum", List(b, as), _) =>
            val xs = compileIntArray(as)
            val y = compileIntExpr(b)
            def withFunctionalDependency = {
                space.post(new Minimum[IntegerValue](nextConstraintId, maybeGoal, xs, y))
                Nil
            }
            def withoutFunctionalDependency = {
                val max = createIntChannel
                space.post(new Minimum[IntegerValue](nextConstraintId, maybeGoal, xs, max))
                val costs = createBoolChannel
                space.post(new Eq[IntegerValue](nextConstraintId, maybeGoal, max, y, costs))
                List(costs)
            }
            compileConstraint(constraint, xs, y, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("array_var_bool_element" | "array_bool_element" | "yuck_array_bool_element" , _, _) =>
            compileElementConstraint[BooleanValue](maybeGoal, constraint)
        case Constraint("array_var_int_element" | "array_int_element" | "yuck_array_int_element" , _, _) =>
            compileElementConstraint[IntegerValue](maybeGoal, constraint)
        case Constraint("array_var_set_element" | "array_set_element" | "yuck_array_set_element" , _, _) =>
            compileElementConstraint[IntegerSetValue](maybeGoal, constraint)
        case Constraint("set_eq", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Eq[IntegerSetValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_eq_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Eq[IntegerSetValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("set_ne", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Ne[IntegerSetValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_ne_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Ne[IntegerSetValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("set_lt", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Lt[IntegerSetValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_lt_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Lt[IntegerSetValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("set_le", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Le[IntegerSetValue](nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_le_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Le[IntegerSetValue](nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("set_card", List(a, b), _) =>
            compileBinaryConstraint[IntegerSetValue, IntegerSetVariable, IntegerValue, IntegerVariable](new SetCardinality(_, _, _, _), maybeGoal, constraint)
        case Constraint("set_in", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Contains(nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_in_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Contains(nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
        case Constraint("set_subset", List(a, b), _) =>
            val costs = createBoolChannel
            space.post(new Subset(nextConstraintId, maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_subset_reif", List(a, b, r), _) =>
            def withFunctionalDependency = {
                space.post(new Subset(nextConstraintId, maybeGoal, a, b, r))
                Nil
            }
            def withoutFunctionalDependency = {
                compileReifiedConstraint(maybeGoal, constraint)
            }
            compileConstraint(constraint, List(a, b), r, withFunctionalDependency, withoutFunctionalDependency)
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
            space.post(new Alldistinct(nextConstraintId, maybeGoal, xs, costs))
            List(costs)
        case Constraint("fzn_alldifferent_except_0", List(as), _) =>
            val xs = compileIntArray(as)
            val costs = createBoolChannel
            space.post(new AlldistinctExceptZero(nextConstraintId, maybeGoal, xs, costs))
            List(costs)
        case Constraint("fzn_nvalue", List(n0, as), _) =>
            val xs = compileIntArray(as)
            val n = compileIntExpr(n0)
            def withFunctionalDependency = {
                space.post(new NumberOfDistinctValues[IntegerValue](nextConstraintId, maybeGoal, xs, n))
                Nil
            }
            def withoutFunctionalDependency = {
                val m = createNonNegativeIntChannel
                space.post(new NumberOfDistinctValues[IntegerValue](nextConstraintId, maybeGoal, xs, m))
                val costs = createBoolChannel
                space.post(new Eq[IntegerValue](nextConstraintId, maybeGoal, m, n, costs))
                List(costs)
            }
            compileConstraint(constraint, xs, n, withFunctionalDependency, withoutFunctionalDependency)
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
            space.post(new Cumulative(nextConstraintId, maybeGoal, tasks, b, costs))
            List(costs)
        case Constraint("yuck_disjunctive", List(x, w, BoolConst(strict)), _) =>
            val xs = compileIntArray(x)
            val ws = compileIntArray(w)
            assert(xs.size == ws.size)
            val y = compileConstant(Zero)
            val h = compileConstant(One)
            val rects = for (i <- 0 until xs.size) yield new Disjoint2Rect(xs(i), y, ws(i), h)
            val costs = createBoolChannel
            space.post(new Disjoint2(nextConstraintId, maybeGoal, rects.to(immutable.ArraySeq), strict, costs))
            List(costs)
        case Constraint("yuck_diffn", List(x, y, w, h, BoolConst(strict)), _) =>
            val xs = compileIntArray(x)
            val ys = compileIntArray(y)
            val ws = compileIntArray(w)
            val hs = compileIntArray(h)
            assert(xs.size == ys.size)
            assert(xs.size == ws.size)
            assert(xs.size == hs.size)
            val rects = for (i <- 0 until xs.size) yield new Disjoint2Rect(xs(i), ys(i), ws(i), hs(i))
            val costs = createBoolChannel
            space.post(new Disjoint2(nextConstraintId, maybeGoal, rects.to(immutable.ArraySeq), strict, costs))
            List(costs)
        case Constraint("fzn_table_bool", List(as, flatTable), _) =>
            val xs = compileBoolArray(as)
            val rows = compileBoolArray(flatTable).map(_.domain.singleValue).grouped(xs.size).to(immutable.ArraySeq)
            val costs = createBoolChannel
            space.post(new BooleanTable(nextConstraintId, maybeGoal, xs, rows, costs))
            List(costs)
        case Constraint("fzn_table_int", List(as, flatTable), _) =>
            val xs = compileIntArray(as)
            val rows = compileIntArray(flatTable).map(_.domain.singleValue).grouped(xs.size).to(immutable.ArraySeq)
            val costs = createBoolChannel
            space.post(new IntegerTable(nextConstraintId, maybeGoal, xs, rows, costs))
            List(costs)
        case Constraint("fzn_regular", List(xs, IntConst(q), IntConst(s), flatDelta, IntConst(q0), f), _) =>
            val Q = q
            val S = s
            val delta = compileIntArray(flatDelta).map(_.domain.singleValue.value).grouped(s).to(immutable.ArraySeq)
            val F = compileIntSetExpr(f).domain.singleValue.set
            val costs = createBoolChannel
            space.post(new Regular(nextConstraintId, maybeGoal, xs, Q, S, delta, q0, F, costs))
            List(costs)
        case Constraint("yuck_inverse", List(f, IntConst(fOffset), g, IntConst(gOffset)), _) =>
            val costs = createBoolChannel
            val constraint = new Inverse(nextConstraintId, maybeGoal, new InverseFunction(f, fOffset), new InverseFunction(g, gOffset), costs)
            val constraints = constraint.decompose(space)
            constraints.foreach(space.post)
            constraints.view.flatMap(_.outVariables)
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
            space.post(new LexLess(nextConstraintId, maybeGoal, xs, ys, costs))
            List(costs)
        case Constraint("fzn_lex_less_bool", List(as, bs), _) =>
            val xs = compileBoolArray(as)
            val ys = compileBoolArray(bs)
            val costs = createBoolChannel
            space.post(new LexLess(nextConstraintId, maybeGoal, xs, ys, costs)(booleanOrdering))
            List(costs)
        case Constraint("fzn_lex_less_set", List(as, bs), _) =>
            val xs = compileIntSetArray(as)
            val ys = compileIntSetArray(bs)
            val costs = createBoolChannel
            space.post(new LexLess(nextConstraintId, maybeGoal, xs, ys, costs))
            List(costs)
        case Constraint("fzn_lex_lesseq_int", List(as, bs), _) =>
            val xs = compileIntArray(as)
            val ys = compileIntArray(bs)
            val costs = createBoolChannel
            space.post(new LexLessEq(nextConstraintId, maybeGoal, xs, ys, costs))
            List(costs)
        case Constraint("fzn_lex_lesseq_bool", List(as, bs), _) =>
            val xs = compileBoolArray(as)
            val ys = compileBoolArray(bs)
            val costs = createBoolChannel
            space.post(new LexLessEq(nextConstraintId, maybeGoal, xs, ys, costs)(booleanOrdering))
            List(costs)
        case Constraint("fzn_lex_lesseq_set", List(as, bs), _) =>
            val xs = compileIntSetArray(as)
            val ys = compileIntSetArray(bs)
            val costs = createBoolChannel
            space.post(new LexLessEq(nextConstraintId, maybeGoal, xs, ys, costs))
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
        def withFunctionalDependency = {
            space.post(constraintFactory(nextConstraintId, maybeGoal, x, y))
            Nil
        }
        def withoutFunctionalDependency = {
            val channel = outHelper.createChannel
            space.post(constraintFactory(nextConstraintId, maybeGoal, x, channel))
            val costs = createBoolChannel
            space.post(new Eq[OutValue](nextConstraintId, maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, List(a), b, withFunctionalDependency, withoutFunctionalDependency)
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
        def withFunctionalDependency = {
            space.post(constraintFactory(nextConstraintId, maybeGoal, x, y, z))
            Nil
        }
        def withoutFunctionalDependency = {
            val channel = outHelper.createChannel
            space.post(constraintFactory(nextConstraintId, maybeGoal, x, y, channel))
            val costs = createBoolChannel
            space.post(new Eq[OutValue](nextConstraintId, maybeGoal, channel, z, costs))
            List(costs)
        }
        compileConstraint(constraint, List(a, b), c, withFunctionalDependency, withoutFunctionalDependency)
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
        (implicit valueTraits: NumericalValueTraits[Load]):
        Iterable[BooleanVariable] =
    {
        require(items.forall(_.weight >= valueTraits.zero))
        val items1: immutable.IndexedSeq[BinPackingItem[Load]] =
            items
            .foldLeft(immutable.Map.empty[IntegerVariable, Load]){
                case (map, item) => map.updated(item.bin, map.getOrElse(item.bin, valueTraits.zero) + item.weight)
            }
            .iterator
            .filter{case (_, weight) => weight > valueTraits.zero}
            .map{case (bin, weight) => new BinPackingItem(bin, weight)}
            .toIndexedSeq
        val loads1: immutable.Map[Int, NumericalVariable[Load]] = {
            val bins = items1.map(_.bin)
            val definedVars = new mutable.HashSet[NumericalVariable[Load]]
            for ((bin, load) <- loads) yield
                if (! definedVars.contains(load) && definesVar(constraint, bins, loads(bin))) {
                    definedVars += load
                    bin -> load
                }
                else bin -> createNonNegativeChannel[Load]
        }
        space.post(new BinPacking[Load](nextConstraintId, maybeGoal, items1, loads1))
        val deltas: immutable.Iterable[BooleanVariable] =
            for ((bin, load) <- loads if load != loads1(bin)) yield {
                val delta = createBoolChannel
                space.post(new Eq[Load](nextConstraintId, maybeGoal, load, loads1(bin), delta))
                delta
            }
        deltas
    }

    private def compileLinearCombination
        [Value <: NumericalValue[Value]]
        (maybeGoal: Option[Goal],
         as0: Expr, bs: Expr,
         maybeChannel: Option[NumericalVariable[Value]] = None)
        (implicit valueTraits: NumericalValueTraits[Value]):
        NumericalVariable[Value] =
    {
        val zero = valueTraits.zero
        val one = valueTraits.one
        val minusOne = one.negate
        val as = compileNumArray[Value](as0)
        val xs = compileNumArray[Value](bs)
        require(as.size == xs.size)
        val axs =
            AX.compact(
                for ((x, y) <- as.view.zip(xs.view)
                     if x.domain.singleValue != zero && (! y.domain.isSingleton || y.domain.singleValue != zero))
                    yield new AX[Value](x.domain.singleValue, y))
        axs match {
            case List(AX(`one`, x)) if maybeChannel.isEmpty =>
                x
            case List(AX(`one`, x), AX(`minusOne`, y)) =>
                val channel = maybeChannel.getOrElse(createNumChannel[Value])
                space.post(new Minus[Value](nextConstraintId, maybeGoal, x, y, channel))
                channel
            case List(AX(`minusOne`, x), AX(`one`, y)) =>
                val channel = maybeChannel.getOrElse(createNumChannel[Value])
                space.post(new Minus[Value](nextConstraintId, maybeGoal, y, x, channel))
                channel
            case _ =>
                val channel = maybeChannel.getOrElse(createNumChannel[Value])
                if (axs.forall(_.a == one)) {
                    if (axs.size == 2) {
                        val List(AX(_, x), AX(_, y)) = axs
                        space.post(new Plus(nextConstraintId, maybeGoal, x, y, channel))
                    } else {
                        val xs = axs.iterator.map(_.x).toIndexedSeq
                        space.post(new Sum(nextConstraintId, maybeGoal, xs , channel))
                    }
                } else {
                   space.post(new LinearCombination(nextConstraintId, maybeGoal, axs.toIndexedSeq, channel))
                }
                channel
        }
    }

    private def compileLinearConstraint
        [Value <: NumericalValue[Value]]
        (maybeGoal: Option[Goal],
         as0: Expr, bs: Expr, relation: OrderingRelation, c: Expr,
         maybeCosts: Option[BooleanVariable] = None)
        (implicit valueTraits: NumericalValueTraits[Value]):
        BooleanVariable =
    {
        val zero = valueTraits.zero
        val as = compileNumArray[Value](as0)
        val xs = compileNumArray[Value](bs)
        require(as.size == xs.size)
        val axs =
            AX.compact(
                for ((x, y) <- as.view.zip(xs.view)
                     if x.domain.singleValue != zero && (! y.domain.isSingleton || y.domain.singleValue != zero))
                    yield new AX[Value](x.domain.singleValue, y))
        val y = createNumChannel[Value]
        val z = compileNumExpr[Value](c)
        val costs = maybeCosts.getOrElse(createBoolChannel)
        if (axs.forall(_.a == valueTraits.one)) {
            space.post(new SumConstraint(nextConstraintId, maybeGoal, axs.map(_.x).to(immutable.ArraySeq), y, relation, z, costs))
        } else {
            space.post(new LinearConstraint(nextConstraintId, maybeGoal, axs.to(immutable.ArraySeq), y, relation, z, costs))
        }
        costs
    }

    private def compileCountConstraint
        [Value <: AnyValue]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         comparatorFactory: TernaryConstraintFactory[IntegerVariable, IntegerVariable, BooleanVariable])
        (implicit valueTraits: ValueTraits[Value]):
        Iterable[BooleanVariable] =
    {
        val Constraint(_, List(as, a, b), _) = constraint
        val xs = compileArray[Value](as)
        val m = compileIntExpr(b)
        if (compilesToConst(a)) {
            val y = compileExpr[Value](a).domain.singleValue
            def withFunctionalDependency = {
                space.post(new CountConst[Value](nextConstraintId, maybeGoal, xs, y, m))
                Nil
            }
            def withoutFunctionalDependency = {
                val n = createNonNegativeIntChannel
                space.post(new CountConst[Value](nextConstraintId, maybeGoal, xs, y, n))
                val costs = createBoolChannel
                space.post(comparatorFactory(nextConstraintId, maybeGoal, n, m, costs))
                List(costs)
            }
            compileConstraint(constraint, xs, m, withFunctionalDependency, withoutFunctionalDependency)
        } else {
            val y = compileExpr[Value](a)
            def withFunctionalDependency = {
                space.post(new CountVar[Value](nextConstraintId, maybeGoal, xs, y, m))
                Nil
            }
            def withoutFunctionalDependency = {
                val n = createNonNegativeIntChannel
                space.post(new CountVar[Value](nextConstraintId, maybeGoal, xs, y, n))
                val costs = createBoolChannel
                space.post(comparatorFactory(nextConstraintId, maybeGoal, n, m, costs))
                List(costs)
            }
            compileConstraint(constraint, xs :+ y, m, withFunctionalDependency, withoutFunctionalDependency)
        }
    }

    private def compileMemberConstraint
        [Value <: AnyValue]
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (implicit valueTraits: ValueTraits[Value]):
        Iterable[BooleanVariable] =
        compileCountConstraint(
            maybeGoal,
            constraint.copy(id = "fzn_count_leq", params = constraint.params ++ List(IntConst(1))),
            new Eq[IntegerValue](_, _, _, _, _))

    private def compileElementConstraint
        [Value <: OrderedValue[Value]]
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (implicit valueTraits: OrderedValueTraits[Value]):
        Iterable[BooleanVariable] =
    {
        val List(IntConst(offset), b, as, c) =
            if (constraint.params.size == 4) constraint.params
            else IntConst(1) :: constraint.params
        val i = compileIntExpr(b)
        val xs = compileArray[Value](as)
        val y = compileOrdExpr[Value](c)
        val indexRange = createIntDomain(offset, offset + xs.size - 1)
        val result = mutable.ArrayBuffer[BooleanVariable]()
        if (b.isConst) {
            if (! indexRange.contains(getConst[IntegerValue](b))) {
                throw new InconsistentConstraintException(constraint)
            }
        } else if (! i.domain.isSubsetOf(indexRange)) {
            // The index may be out of range, so we have to add a check, as required by the FlatZinc spec.
            val delta = createBoolChannel
            space.post(new Contains(nextConstraintId, maybeGoal, i, indexRange, delta))
            result += delta
        }
        def post(y: OrderedVariable[Value]): OrderedVariable[Value] = {
            if (xs.forall(_.domain.isSingleton)) {
                val as = xs.map(_.domain.singleValue)
                space.post(new ElementConst[Value](nextConstraintId, maybeGoal, as, i, y, offset))
            } else {
                space.post(new ElementVar[Value](nextConstraintId, maybeGoal, xs, i, y, offset))
            }
            y
        }
        def withFunctionalDependency = {
            post(y)
            result
        }
        def withoutFunctionalDependency = {
            val channel = post(createOrdChannel[Value])
            val costs = createBoolChannel
            space.post(new Eq[Value](nextConstraintId, maybeGoal, channel, y, costs))
            result += costs
            result
        }
        compileConstraint(constraint, xs :+ i, y, withFunctionalDependency, withoutFunctionalDependency)
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
            def withFunctionalDependency = {
                space.post(new Conjunction(nextConstraintId, maybeGoal, Nil, satisfied))
                Nil
            }
            def withoutFunctionalDependency = {
                List(satisfied)
            }
            compileConstraint(reifiedConstraint, Nil, satisfied, withFunctionalDependency, withoutFunctionalDependency)
        } else {
            val costs0 = compileConstraint(maybeGoal, constraint).toIndexedSeq
            def withFunctionalDependency = {
                space.post(new Conjunction(nextConstraintId, maybeGoal, costs0, satisfied))
                Nil
            }
            def withoutFunctionalDependency = {
                val costs = createBoolChannel
                if (costs0.size == 1) {
                    space.post(new Eq(nextConstraintId, maybeGoal, costs0.head, satisfied, costs))
                } else {
                    val costs1 = createBoolChannel
                    space.post(new Conjunction(nextConstraintId, maybeGoal, costs0, costs1))
                    space.post(new Eq(nextConstraintId, maybeGoal, costs1, satisfied, costs))
                }
                List(costs)
            }
            compileConstraint(reifiedConstraint, costs0, satisfied, withFunctionalDependency, withoutFunctionalDependency)
        }
    }

}
