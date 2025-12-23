package yuck.flatzinc.compiler

import scala.collection.*
import scala.ref.WeakReference

import yuck.constraints.*
import yuck.constraints.OrderingRelation.*
import yuck.core.*
import yuck.flatzinc.ast.*
import yuck.util.arm.scoped
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
 * Reification is dealt with in a generic way such that ALL constraints (including global
 * ones) can be reified.
 *
 * Potential functional dependencies (e.g. those pointed out by defines_var annotations)
 * are exploited as far as possible; only those annotations are ignored the processing
 * of which would entail a cyclic constraint graph.
 */
final class ConstraintFactory
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    import ConstraintFactory.*

    private def definedVars(annotation: Annotation): Seq[AnyVariable] =
        annotation.term match {
            case Term("defines_var", Seq(a)) => List(compileAnyExpr(a))
            case Term("yuck_defines_bool_vars" | "yuck_defines_int_vars" | "yuck_defines_set_vars", Seq(a)) => compileAnyArray(a)
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
        cc.space.maybeDefiningConstraint(out).isEmpty &&
        (! cc.space.wouldIntroduceCycle(new DummyConstraint(fakeConstraintId, in, List(out))))

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

    private def createGoal(constraint: yuck.flatzinc.ast.Constraint): Option[Goal] =
        val userDefinedGoals =
            constraint.annotations.flatMap {
                case Annotation(Term("goal", Seq(StringConst(name)))) => List(name)
                case _ => Nil
            }
        if ! userDefinedGoals.isEmpty
        then Option(UserDefinedGoal(userDefinedGoals.head))
        else if cc.cfg.attachGoals
        then Some(FlatZincGoal(constraint))
        else None

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         out: Iterable[AnyVariable],
         functionalCase: => Iterable[BooleanVariable],
         generalCase: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
    {
        val definableVars = definedVars(constraint)
        if out.forall(x =>
                ! x.domain.isSingleton &&
                ! cc.searchVars.contains(x) &&
                cc.space.maybeDefiningConstraint(x).isEmpty &&
                (definableVars.contains(x) || ! cc.definedVars.contains(x))) then
        {
            try {
                functionalCase
            }
            catch {
                case _: IllegalArgumentException => generalCase
            }
        } else {
            generalCase
        }
    }

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         out: Expr,
         functionalCase: => Iterable[BooleanVariable],
         generalCase: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
        compileConstraint(constraint, List(compileAnyExpr(out)), functionalCase, generalCase)

    override def run() = {
        cc.costVars ++=
            cc.ast.constraints.iterator
                .flatMap(constraint => compileConstraint(createGoal(constraint), constraint, None))
        optimizeIntDomainEnforcement()
    }

    // maybeCosts may contain the cost variable the caller would like to be used.
    // compileConstraint is free to ignore the given cost variable.
    // If compileConstraint considers the cost variable, it must return a singleton sequence containing the variable.
    // Otherwise, the caller must deal with the result by use of a Conjunction constraint.
    // (maybeCosts is for use by compileReifiedConstraint.)
    private def compileConstraint
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint, maybeCosts: Option[BooleanVariable] = None):
        Iterable[BooleanVariable] =
    {
        if cc.sigint.isSet then {
            throw new FlatZincCompilerInterruptedException
        }
        if cc.impliedConstraints.contains(constraint) then {
            cc.logger.log("Skipping %s".format(constraint))
            Nil
        } else {
            cc.logger.log("Compiling %s".format(constraint))
            scoped(new LogScope(cc.logger)) {
                // toList enforces constraint generation in this log scope
                compileNonImplicitConstraint(maybeGoal, constraint, maybeCosts).toList
            }
        }
    }

    import HighPriorityImplicits.*

    private def compileNonImplicitConstraint
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint, maybeCosts: Option[BooleanVariable]):
        Iterable[BooleanVariable] =
        (constraint: @unchecked) match
    {
        case Constraint(Reif(_), _, _) =>
            require(maybeCosts.isEmpty)
            compileReifiedConstraint(maybeGoal, constraint)
        case Constraint("bool2int", Seq(a, b), _) =>
            val x = compileBoolExpr(a)
            val y = compileIntExpr(b)
            def functionalCase = {
                cc.post(new Bool2Int1(nextConstraintId(), maybeGoal, x, y))
                if IntegerRange(0, 1).diff(y.domain).isEmpty
                then Nil
                else enforceIntDomain(y)
            }
            def generalCase = {
                val costs = createBoolChannel()
                cc.post(new Bool2Int2(nextConstraintId(), maybeGoal, x, y, costs))
                List(costs)
            }
            compileConstraint(constraint, List(y), functionalCase, generalCase)
        case Constraint("bool2costs", Seq(a, b), _) =>
            def functionalCase = {
                cc.post(new Bool2Costs1(nextConstraintId(), maybeGoal, a, b))
                enforceIntDomain(b)
            }
            def generalCase = {
                val costs = createBoolChannel()
                cc.post(new Bool2Costs2(nextConstraintId(), maybeGoal, a, b, costs))
                List(costs)
            }
            compileConstraint(constraint, b, functionalCase, generalCase)
        case Constraint("bool_not", Seq(a, b), _) =>
            def functionalCase = {
                cc.post(new Not(nextConstraintId(), maybeGoal, a, b))
                enforceBoolDomain(b)
            }
            def generalCase = {
                val costs = createBoolChannel()
                cc.post(new Ne[BooleanValue, BooleanDomain, BooleanVariable](nextConstraintId(), maybeGoal, a, b, costs))
                List(costs)
            }
            compileConstraint(constraint, b, functionalCase, generalCase)
        case Constraint("bool_eq", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, EqRelation, maybeCosts)(using BooleanCompilationHelper)
        case Constraint("bool_lt", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, LtRelation, maybeCosts)(using BooleanCompilationHelper)
        case Constraint("bool_le", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, LeRelation, maybeCosts)(using BooleanCompilationHelper)
        case Constraint("bool_and", _, _) =>
            compileTernaryBoolConstraint(new And(_, _, _, _, _), (_, _, z) => enforceBoolDomain(z), maybeGoal, constraint)
        case Constraint("bool_or", _, _) =>
            compileTernaryBoolConstraint(new Or(_, _, _, _, _), (_, _, z) => enforceBoolDomain(z), maybeGoal, constraint)
        case Constraint("bool_xor", _, _) =>
            compileTernaryBoolConstraint(new Ne(_, _, _, _, _), (_, _, z) => enforceBoolDomain(z), maybeGoal, constraint)
        case Constraint("array_bool_and", Seq(as, b), _) =>
            val xs = compileBoolArray(as)
            val y = compileBoolExpr(b)
            def functionalCase = {
                postConjunction(maybeGoal, xs, Some(y))
                enforceBoolDomain(y)
            }
            def generalCase = {
                if y.domain == TrueDomain then {
                    List(postConjunction(maybeGoal, xs))
                } else {
                    val costs0 = postConjunction(maybeGoal, xs)
                    val costs = createBoolChannel()
                    cc.post(new Eq(nextConstraintId(), maybeGoal, costs0, y, costs))
                    List(costs)
                }
            }
            compileConstraint(constraint, List(y), functionalCase, generalCase)
        case Constraint("array_bool_or", Seq(as, b), _) =>
            val xs = compileBoolArray(as)
            val y = compileBoolExpr(b)
            def functionalCase = {
                postDisjunction(maybeGoal, xs, Some(y))
                enforceBoolDomain(y)
            }
            def generalCase = {
                if y.domain == TrueDomain then {
                    List(postDisjunction(maybeGoal, xs))
                } else {
                    val costs0 = postDisjunction(maybeGoal, xs)
                    val costs = createBoolChannel()
                    cc.post(new Eq(nextConstraintId(), maybeGoal, costs0, y, costs))
                    List(costs)
                }
            }
            compileConstraint(constraint, List(y), functionalCase, generalCase)
        case Constraint("array_bool_xor", Seq(as), _) =>
            val xs = compileBoolArray(as)
            val maybeY =
                xs
                .filter(y => definesVar(constraint, xs.filter(_ != y), y))
                .sortWith((x, y) => definesVar(constraint, x) && ! definesVar(constraint, y))
                .headOption
            if maybeY.isDefined then {
                val y = maybeY.get
                val trueCount = createNonNegativeIntChannel()
                cc.post(new CountConst(nextConstraintId(), maybeGoal, xs.filter(_ != y), True, trueCount))
                cc.post(new Even(nextConstraintId(), maybeGoal, trueCount, y))
                Nil
            } else {
                val trueCount = createNonNegativeIntChannel()
                cc.post(new CountConst(nextConstraintId(), maybeGoal, xs, True, trueCount))
                val costs = createBoolChannel()
                cc.post(new Uneven(nextConstraintId(), maybeGoal, trueCount, costs))
                List(costs)
            }
        case Constraint("bool_clause", Seq(ArrayConst(IndexedSeq(a)), ArrayConst(IndexedSeq(b))), _) =>
            compileConstraint(maybeGoal, Constraint("bool_le", List(b, a), Nil), maybeCosts)
        case Constraint("bool_clause", Seq(as, bs), _) =>
            // as are positive literals, bs are negative literals
            val xs = compileBoolArray(as).iterator.filterNot(_.domain == FalseDomain).toList
            val ys = compileBoolArray(bs).iterator.filterNot(_.domain == TrueDomain).toList
            (xs, ys) match {
                case (Nil, Nil) =>
                    List(compileBoolExpr(BoolConst(false)))
                case (Nil, _) =>
                    val costs0 = postConjunction(maybeGoal, ys)
                    val costs = maybeCosts.getOrElse(createBoolChannel())
                    cc.post(new Not(nextConstraintId(), maybeGoal, costs0, costs))
                    List(costs)
                case (_, Nil) =>
                    List(postDisjunction(maybeGoal, xs, maybeCosts))
                case _ =>
                    val costs0 = postDisjunction(maybeGoal, xs)
                    val costs1 = postConjunction(maybeGoal, ys)
                    val costs = maybeCosts.getOrElse(createBoolChannel())
                    cc.post(new Le(nextConstraintId(), maybeGoal, costs1, costs0, costs))
                    List(costs)
            }
        case Constraint("int_eq", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, EqRelation, maybeCosts)(using IntegerCompilationHelper)
        case Constraint("int_ne", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, NeRelation, maybeCosts)(using IntegerCompilationHelper)
        case Constraint("int_lt", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, LtRelation, maybeCosts)(using IntegerCompilationHelper)
        case Constraint("int_le", _, _) =>
             compileOrderingConstraint(maybeGoal, constraint, LeRelation, maybeCosts)(using IntegerCompilationHelper)
        case Constraint("int_min", _, _) =>
            compileTernaryIntConstraint(
                new Min(_, _, _, _, _),
                (x, y, z) =>
                    if IntegerDomainPruner.minRule(List(x.domain, y.domain), CompleteIntegerRange)._2.isSubsetOf(z.domain)
                    then Nil
                    else enforceIntDomain(z),
                maybeGoal,
                constraint)
        case Constraint("int_max", _, _) =>
            compileTernaryIntConstraint(
                new Max(_, _, _, _, _),
                (x, y, z) =>
                    if IntegerDomainPruner.maxRule(List(x.domain, y.domain), CompleteIntegerRange)._2.isSubsetOf(z.domain)
                    then Nil
                    else enforceIntDomain(z),
                maybeGoal,
                constraint)
        case Constraint("int_plus", Seq(a, b, c), annotations) =>
            compileConstraint(
                maybeGoal,
                Constraint("int_lin_eq", List(ArrayConst(Vector(IntConst(1), IntConst(1))), ArrayConst(Vector(a, b)), c), annotations))
        case Constraint("int_minus", Seq(a, b, c), annotations) =>
            compileConstraint(
                maybeGoal,
                Constraint("int_lin_eq", List(ArrayConst(Vector(IntConst(1), IntConst(-1))), ArrayConst(Vector(a, b)), c), annotations))
        case Constraint("int_times", _, _) =>
            compileTernaryIntConstraint(
                new Times(_, _, _, _, _),
                (x, y, z) =>
                    if IntegerDomainPruner.timesRule(x.domain, y.domain, CompleteIntegerRange)._2.isSubsetOf(z.domain)
                    then Nil
                    else enforceIntDomain(z),
                maybeGoal,
                constraint)
        case Constraint("int_div", _, _) =>
            compileTernaryIntConstraint(new Div(_, _, _, _, _), (_, _, z) => enforceIntDomain(z), maybeGoal, constraint)
        case Constraint("int_mod", _, _) =>
            compileTernaryIntConstraint(new Mod(_, _, _, _, _), (_, _, z) => enforceIntDomain(z), maybeGoal, constraint)
        case Constraint("int_pow", _, _) =>
            compileTernaryIntConstraint(new Power(_, _, _, _, _), (_, _, z) => enforceIntDomain(z), maybeGoal, constraint)
        case Constraint("int_abs", _, _) =>
            compileBinaryConstraint1
                [IntegerValue, IntegerDomain, IntegerVariable, IntegerValue, IntegerDomain, IntegerVariable]
                (new Abs(_, _, _, _),
                 (x, y) =>
                     if IntegerDomainPruner.absRule(x.domain, CompleteIntegerRange)._2.isSubsetOf(y.domain)
                     then Nil
                     else enforceIntDomain(y),
                 maybeGoal,
                 constraint)
        // expansion of terms in parameters
        case Constraint(IntLin(_), (as@Term(_, _)) :: t, _) =>
            compileConstraint(maybeGoal, constraint.copy(params = ArrayConst(getArrayElems(as)) :: t), maybeCosts)
        case Constraint(IntLin(_), as :: (bs@Term(_, _)) :: t, _) =>
            compileConstraint(maybeGoal, constraint.copy(params = as :: ArrayConst(getArrayElems(bs)) :: t), maybeCosts)
        case Constraint(IntLin(_), as :: bs :: c :: t, _) if !c.isConst && compilesToConst(c) =>
            compileConstraint(
                maybeGoal,
                constraint.copy(params = as :: bs :: IntConst(compileConstIntExpr(c).value) :: t),
                maybeCosts)
        // -1 * x <op> c -> 1 * x <op> -c where op in {==, !=}
        case Constraint(IntLin(name), ArrayConst(List(IntConst(-1))) :: bs :: IntConst(c) :: t, _)
            if name.startsWith("eq") || name.startsWith("ne") =>
            compileConstraint(
                maybeGoal,
                constraint.copy(params = ArrayConst(Vector(IntConst(1))) :: bs :: IntConst(-c) :: t),
                maybeCosts)
        // 1 * x <op> c -> x <op> c
        case Constraint(IntLin(name), ArrayConst(List(IntConst(1))) :: ArrayConst(bs) :: c :: t, annotations) =>
            compileConstraint(maybeGoal, Constraint("int_" + name, bs.head :: c :: t, annotations), maybeCosts)
        // -1 * x <op> c -> -c <op> x
        case Constraint(IntLin(name), ArrayConst(List(IntConst(-1))) :: ArrayConst(bs) :: IntConst(c) :: t, annotations) =>
            compileConstraint(maybeGoal, Constraint("int_" + name, IntConst(-c) :: bs.head :: t, annotations), maybeCosts)
        // -1 * x + 1 * y <op> c -> 1 * y + -1 * x <op> c
        case Constraint(IntLin(_), ArrayConst(Seq(IntConst(-1), IntConst(1))) :: ArrayConst(Seq(x, y)) :: c :: t, _) =>
            compileConstraint(
                maybeGoal,
                constraint.copy(params = ArrayConst(Vector(IntConst(1), IntConst(-1))) :: ArrayConst(Vector(y, x)) :: c :: t),
                maybeCosts)
        // 1 * x + -1 * y <op> 0 -> x <op> y
        case Constraint(
            IntLin(name),
            ArrayConst(Seq(IntConst(1), IntConst(-1))) :: ArrayConst(Seq(x, y)) :: IntConst(0) :: t, annotations) =>
            compileConstraint(maybeGoal, Constraint("int_" + name, x :: y :: t, annotations), maybeCosts)
        // 1 * x + -1 * y <= -1 -> x < y
        case Constraint(
            IntLin(name),
            ArrayConst(Seq(IntConst(1), IntConst(-1))) :: ArrayConst(Seq(x, y)) :: IntConst(-1) :: t, annotations)
            if name.startsWith("le") =>
            compileConstraint(
                maybeGoal,
                Constraint("int_" + name.replace("le", "lt"), x :: y :: t, annotations),
                maybeCosts)
        case Constraint("int_lin_eq", Seq(ArrayConst(as), ArrayConst(bs), c), annotations)
            if ! definesVar(constraint, c) && as.iterator.zip(bs.iterator).exists(
                (_: @unchecked) match {
                    case (IntConst(a), b) => (a == -1 || a == 1) && definesVar(constraint, b)
                }
            ) =>
            val abs = as.zip(bs)
            val (a, b) = abs.find(
                (_: @unchecked) match {
                    case (IntConst(a), b) => (a == -1 || a == 1) && definesVar(constraint, b)
                }
            ).get
            a match {
                case IntConst(1) =>
                    // b1 + a2 b2 + ... = c
                    // b1               = c - a2 b2 - ...
                    val (as1, bs1) = (for case (IntConst(a), b1) <- abs if b1 != b yield (IntConst(-a), b1)).unzip
                    compileConstraint(
                        maybeGoal,
                        Constraint("int_lin_eq", List(ArrayConst(IntConst(1) +: as1), ArrayConst(c +: bs1), b), annotations),
                        maybeCosts)
                case IntConst(-1) =>
                    // -1 b1 + a2 b2 + ... =    c
                    // -1 b1               =    c - a2 b2 - ...
                    //    b1               = -1 c + a2 b2 + ...
                    val bs1 = for b1 <- bs yield if b1 == b then c else b1
                    compileConstraint(
                        maybeGoal,
                        Constraint("int_lin_eq", List(ArrayConst(as), ArrayConst(bs1), b), annotations),
                        maybeCosts)
            }
        case Constraint("int_lin_eq", Seq(as, bs, c), _) =>
            def functionalCase = {
                val y = compileIntExpr(c)
                compileLinearCombination(maybeGoal, as, bs, Some(y))
                val lhs = compileIntArray(as).view.map(_.domain.singleValue).zip(compileIntArray(bs).view.map(_.domain))
                if IntegerDomainPruner.linEqRule(lhs, CompleteIntegerRange)._2.isSubsetOf(y.domain)
                then Nil
                else enforceIntDomain(y)
            }
            def generalCase = {
                List(compileLinearConstraint(maybeGoal, as, bs, EqRelation, c, maybeCosts)(using IntegerCompilationHelper))
            }
            compileConstraint(constraint, c, functionalCase, generalCase)
        case Constraint("int_lin_ne", Seq(as, bs, c), _) =>
            List(compileLinearConstraint(maybeGoal, as, bs, NeRelation, c, maybeCosts)(using IntegerCompilationHelper))
        case Constraint("int_lin_le", Seq(as, bs, c), _) =>
            List(compileLinearConstraint(maybeGoal, as, bs, LeRelation, c, maybeCosts)(using IntegerCompilationHelper))
        case Constraint("array_int_maximum", _, _) =>
            compileBinaryIntConstraint2(
                new Maximum(_, _, _, _),
                (xs, y) =>
                    if IntegerDomainPruner.maxRule(xs.view.map(_.domain), CompleteIntegerRange)._2.isSubsetOf(y.domain)
                    then Nil
                    else enforceIntDomain(y),
                maybeGoal,
                constraint)
        case Constraint("array_int_minimum", _, _) =>
            compileBinaryIntConstraint2(
                new Minimum(_, _, _, _),
                (xs, y) =>
                    if IntegerDomainPruner.minRule(xs.view.map(_.domain), CompleteIntegerRange)._2.isSubsetOf(y.domain)
                    then Nil
                    else enforceIntDomain(y),
                maybeGoal,
                constraint)
        case Constraint("array_var_bool_element" | "array_bool_element" | "yuck_array_bool_element" , _, _) =>
            compileElementConstraint(maybeGoal, constraint)(using BooleanCompilationHelper)
        case Constraint("array_var_int_element" | "array_int_element" | "yuck_array_int_element" , _, _) =>
            compileElementConstraint(maybeGoal, constraint)(using IntegerCompilationHelper)
        case Constraint("array_var_set_element" | "array_set_element" | "yuck_array_set_element" , _, _) =>
            compileElementConstraint(maybeGoal, constraint)(using IntegerSetCompilationHelper)
        case Constraint("yuck_if_then_else_var_bool" | "yuck_if_then_else_bool", _, _) =>
            compileIfThenElseConstraint(maybeGoal, constraint)(using BooleanCompilationHelper)
        case Constraint("yuck_if_then_else_var_int" | "yuck_if_then_else_int", _, _) =>
            compileIfThenElseConstraint(maybeGoal, constraint)(using IntegerCompilationHelper)
        case Constraint("yuck_if_then_else_var_set" | "yuck_if_then_else_set", _, _) =>
            compileIfThenElseConstraint(maybeGoal, constraint)(using IntegerSetCompilationHelper)
        case Constraint("set_eq", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, EqRelation, maybeCosts)(using IntegerSetCompilationHelper)
        case Constraint("set_ne", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, NeRelation, maybeCosts)(using IntegerSetCompilationHelper)
        case Constraint("set_lt", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, LtRelation, maybeCosts)(using IntegerSetCompilationHelper)
        case Constraint("set_le", _, _) =>
            compileOrderingConstraint(maybeGoal, constraint, LeRelation, maybeCosts)(using IntegerSetCompilationHelper)
        case Constraint("set_card", _, _) =>
            def enforceDomain(x: IntegerSetVariable, y: IntegerVariable) = {
                val (minCard, maxCard) = x.domain match {
                    case dx: SingletonIntegerSetDomain => (dx.base.size, dx.base.size)
                    case dx: IntegerPowerSetDomain => (0, dx.base.size)
                }
                if IntegerRange(minCard, maxCard).isSubsetOf(y.domain) then Nil else enforceIntDomain(y)
            }
            compileBinaryConstraint1
                [IntegerSetValue, IntegerSetDomain, IntegerSetVariable, IntegerValue, IntegerDomain, IntegerVariable]
                (new SetCardinality(_, _, _, _), enforceDomain, maybeGoal, constraint)
        case Constraint("set_in", Seq(a, b), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new Contains(nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_subset", Seq(a, b), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new Subset(nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_intersect", _, _) =>
            compileTernaryIntSetConstraint(
                new SetIntersection(_, _, _, _, _),
                (x, y, z) => if x.domain.intersect(y.domain).isSubsetOf(z.domain) then Nil else enforceIntSetDomain(z),
                maybeGoal,
                constraint)
        case Constraint("set_union", _, _) =>
            compileTernaryIntSetConstraint(
                new SetUnion(_, _, _, _, _),
                (x, y, z) =>
                    if x.domain.isSubsetOf(z.domain) && y.domain.isSubsetOf(z.domain)
                    then Nil
                    else enforceIntSetDomain(z),
                maybeGoal,
                constraint)
        case Constraint("set_diff", _, _) =>
            compileTernaryIntSetConstraint(
                new SetDifference(_, _, _, _, _),
                (x, _, z) => if x.domain.isSubsetOf(z.domain) then Nil else enforceIntSetDomain(z),
                maybeGoal,
                constraint)
        case Constraint("set_symdiff", _, _) =>
            compileTernaryIntSetConstraint(
                new SymmetricalSetDifference(_, _, _, _, _),
                (x, y, z) =>
                    if x.domain.isSubsetOf(z.domain) && y.domain.isSubsetOf(z.domain)
                    then Nil
                    else enforceIntSetDomain(z),
                maybeGoal,
                constraint)
        case Constraint("fzn_all_different_int", Seq(as), _) =>
            val xs = compileIntArray(as)
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new AllDifferent(nextConstraintId(), maybeGoal, xs, immutable.Set(), costs, cc.logger)(using IntegerTypeTraits))
            List(costs)
        case Constraint("fzn_all_different_set", Seq(as), _) =>
            val xs = compileIntSetArray(as)
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new AllDifferent(nextConstraintId(), maybeGoal, xs, immutable.Set(), costs, cc.logger)(using IntegerSetTypeTraits))
            List(costs)
        case Constraint("fzn_alldifferent_except", Seq(as, s), _) =>
            val xs = compileIntArray(as)
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new AllDifferent(nextConstraintId(), maybeGoal, xs, s.set.values.toSet, costs, cc.logger)(using IntegerTypeTraits))
            List(costs)
        case Constraint("fzn_alldifferent_except_0", Seq(as), _) =>
            val xs = compileIntArray(as)
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new AllDifferent(nextConstraintId(), maybeGoal, xs, immutable.Set(Zero), costs, cc.logger)(using IntegerTypeTraits))
            List(costs)
        case Constraint("fzn_increasing_bool", Seq(as), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new BooleanIncreasing(nextConstraintId(), maybeGoal, as, costs))
            List(costs)
        case Constraint("yuck_increasing_int", Seq(as, BoolConst(strict)), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new IntegerIncreasing(nextConstraintId(), maybeGoal, as, strict, costs))
            List(costs)
        case Constraint("fzn_nvalue", _, _) =>
            compileBinaryIntConstraint2(
                new NumberOfDistinctValues(_, _, _, _),
                (xs, y) => {
                    val countDomainApproximation = IntegerRange(
                        if xs.isEmpty then 0 else 1,
                        min(xs.size, xs.foldLeft(IntegerTypeTraits.emptyDomain)((u, x) => u.union(x.domain)).size))
                    if countDomainApproximation.isSubsetOf(y.domain) then Nil else enforceIntDomain(y)
                },
                maybeGoal,
                constraint)
        case Constraint(Count(_, "bool"), _, _) =>
            compileCountConstraint(maybeGoal, constraint, maybeCosts)(using BooleanCompilationHelper)
        case Constraint(Count(_, "int"), _, _) =>
            compileCountConstraint(maybeGoal, constraint, maybeCosts)(using IntegerCompilationHelper)
        case Constraint(Count(_, "set"), _, _) =>
            compileCountConstraint(maybeGoal, constraint, maybeCosts)(using IntegerSetCompilationHelper)
        case Constraint("fzn_cumulative", Seq(s, d, r, b), _) =>
            val xs = compileIntArray(s)
            val ys = compileIntArray(d)
            val zs = compileIntArray(r)
            assert(xs.size == ys.size)
            assert(ys.size == zs.size)
            val tasks = for ((x, y), z) <- xs.zip(ys).zip(zs) yield new CumulativeTask(x, y, z)
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new Cumulative(nextConstraintId(), maybeGoal, tasks, b, costs))
            List(costs)
        case Constraint("yuck_disjunctive", Seq(x, w, BoolConst(strict)), _) =>
            val xs = compileIntArray(x)
            val ws = compileIntArray(w)
            assert(xs.size == ws.size)
            val y = compileConstant(Zero)
            val h = compileConstant(One)
            val rects = Vector.tabulate(xs.size)(i => new Disjoint2Rect(xs(i), y, ws(i), h))
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new Disjoint2(nextConstraintId(), maybeGoal, rects, strict, costs))
            List(costs)
        case Constraint("yuck_diffn", Seq(x, y, w, h, BoolConst(strict)), _) =>
            val xs = compileIntArray(x)
            val ys = compileIntArray(y)
            val ws = compileIntArray(w)
            val hs = compileIntArray(h)
            assert(xs.size == ys.size)
            assert(xs.size == ws.size)
            assert(xs.size == hs.size)
            val rects = Vector.tabulate(xs.size)(i => new Disjoint2Rect(xs(i), ys(i), ws(i), hs(i)))
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new Disjoint2(nextConstraintId(), maybeGoal, rects, strict, costs))
            List(costs)
        case Constraint("yuck_table_bool", Seq(as, flatTable), _) =>
            val xs = compileBoolArray(as)
            val rows = compileBoolArray(flatTable).map(_.domain.singleValue).grouped(xs.size).toVector
            val costs = maybeCosts.getOrElse(createBoolChannel())
            val forceImplicitSolving = constraint.annotations.exists(forcesImplicitSolving)
            cc.post(new Table(nextConstraintId(), maybeGoal, xs, rows, costs, forceImplicitSolving))
            List(costs)
        case Constraint("yuck_table_int", Seq(as, flatTable), _) =>
            val xs = compileIntArray(as)
            val rows = compileIntArray(flatTable).map(_.domain.singleValue).grouped(xs.size).toVector
            val costs = maybeCosts.getOrElse(createBoolChannel())
            val forceImplicitSolving = constraint.annotations.exists(forcesImplicitSolving)
            cc.post(new Table(nextConstraintId(), maybeGoal, xs, rows, costs, forceImplicitSolving))
            List(costs)
        case Constraint("yuck_regular", Seq(xs, q, s, flatDelta, q0, f), _) =>
            val delta = compileIntArray(flatDelta).map(_.domain.singleValue.toInt).grouped(s.toInt).toVector
            val costs = maybeCosts.getOrElse(createBoolChannel())
            val dfa = new RegularDfa(xs, q.toInt, s.toInt, delta, q0.toInt, f.set)
            cc.post(new Regular(nextConstraintId(), maybeGoal, dfa, costs, cc.logger))
            List(costs)
        case Constraint("yuck_circuit", Seq(succ, IntConst(offset)), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.post(new Circuit(nextConstraintId(), maybeGoal, succ, safeToInt(offset), costs, cc.logger, cc.sigint))
            List(costs)
        case Constraint("yuck_delivery", _, _) =>
            compileDeliveryConstraint(maybeGoal, constraint)(using IntegerCompilationHelper)
        case Constraint("yuck_inverse", Seq(f, IntConst(fOffset), g, IntConst(gOffset)), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            val constraint = new Inverse(nextConstraintId(), maybeGoal, new InverseFunction(f, safeToInt(fOffset)), new InverseFunction(g, safeToInt(gOffset)), costs, cc.logger)
            val constraints = constraint.decompose(cc.space)
            constraints.foreach(cc.post)
            constraints.view.flatMap(_.outVariables).map(_.asInstanceOf[BooleanVariable])
        case Constraint("yuck_bin_packing_load", Seq(loads0, bins0, weights0, IntConst(minLoadIndex0)), _) =>
            val bins = compileIntArray(bins0)
            val weights = getArrayElems(weights0).map(compileConstIntExpr)
            val minLoadIndex = safeToInt(minLoadIndex0)
            require(bins.size == weights.size)
            val itemGenerator =
                for (bin, weight) <- bins.iterator.zip(weights.iterator) yield
                    new BinPackingItem(bin, weight)
            val items = itemGenerator.toVector
            val loads1 = compileIntArray(loads0)
            val loads = (minLoadIndex until minLoadIndex + loads1.size).iterator.map(IntegerValue.apply).zip(loads1.iterator).toMap
            compileBinPackingConstraint(maybeGoal, constraint, items, loads)
        case Constraint("fzn_global_cardinality", Seq(xs0, cover0, counts0), _) =>
            val xs = compileIntArray(xs0)
            val items = xs.map(new BinPackingItem(_, One))
            val cover = getArrayElems(cover0).map(compileConstIntExpr(_).toInt)
            val counts = compileIntArray(counts0)
            require(cover.size == counts.size)
            val loads = cover.iterator.map(IntegerValue.apply).zip(counts.iterator).toMap
            compileBinPackingConstraint(maybeGoal, constraint, items, loads)
        case Constraint("fzn_lex_less_int", Seq(as, bs), _) =>
            compileLexLessConstraint(maybeGoal, constraint, LtRelation, maybeCosts)(using IntegerCompilationHelper)
        case Constraint("fzn_lex_less_bool", Seq(as, bs), _) =>
            compileLexLessConstraint(maybeGoal, constraint, LtRelation, maybeCosts)(using BooleanCompilationHelper, FlatZincBooleanValueOrdering)
        case Constraint("fzn_lex_less_set", Seq(as, bs), _) =>
            compileLexLessConstraint(maybeGoal, constraint, LtRelation, maybeCosts)(using IntegerSetCompilationHelper)
        case Constraint("fzn_lex_lesseq_int", Seq(as, bs), _) =>
            compileLexLessConstraint(maybeGoal, constraint, LeRelation, maybeCosts)(using IntegerCompilationHelper)
        case Constraint("fzn_lex_lesseq_bool", Seq(as, bs), _) =>
            compileLexLessConstraint(maybeGoal, constraint, LeRelation, maybeCosts)(using BooleanCompilationHelper, FlatZincBooleanValueOrdering)
        case Constraint("fzn_lex_lesseq_set", Seq(as, bs), _) =>
            compileLexLessConstraint(maybeGoal, constraint, LeRelation, maybeCosts)(using IntegerSetCompilationHelper)
        case Constraint("redundant_constraint", Seq(b), _) =>
            cc.costVarsFromRedundantConstraints += b
            Nil
    }

    private def compileOrderingConstraint
        [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         relation: OrderingRelation,
         maybeCosts: Option[BooleanVariable] = None)
        (using helper: OrderedCompilationHelper[A, D, X]):
        Iterable[BooleanVariable] =
    {
        given typeTraits: OrderedTypeTraits[A, D, X] = helper.typeTraits
        val Constraint(_, Seq(a, b), _) = constraint: @unchecked
        val x = helper.compileExpr(a)
        val y = helper.compileExpr(b)
        val costs = maybeCosts.getOrElse(createBoolChannel())
        relation match {
            case EqRelation => cc.post(new Eq(nextConstraintId(), maybeGoal, x, y, costs))
            case NeRelation => cc.post(new Ne(nextConstraintId(), maybeGoal, x, y, costs))
            case LtRelation => cc.post(new Lt(nextConstraintId(), maybeGoal, x, y, costs))
            case LeRelation => cc.post(new Le(nextConstraintId(), maybeGoal, x, y, costs))
        }
        List(costs)
    }

    private def compileBinaryConstraint1
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X],
         B <: Value[B], E <: Domain[B, E], Y <: Variable[B, E, Y]]
        (createConstraint: (Id[yuck.core.Constraint], Option[Goal], X, Y) => yuck.core.Constraint,
         enforceDomain: (X, Y) => Iterable[BooleanVariable],
         maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using
         aHelper: CompilationHelper[A, D, X],
         bHelper: CompilationHelper[B, E, Y]):
        Iterable[BooleanVariable] =
    {
        given bTraits: TypeTraits[B, E, Y] = bHelper.typeTraits
        val Seq(a, b) = constraint.params: @unchecked
        val x = aHelper.compileExpr(a)
        val y = bHelper.compileExpr(b)
        def functionalCase = {
            cc.post(createConstraint(nextConstraintId(), maybeGoal, x, y))
            enforceDomain(x, y)
        }
        def generalCase = {
            val channel = bHelper.createChannel()
            cc.post(createConstraint(nextConstraintId(), maybeGoal, x, channel))
            val costs = createBoolChannel()
            cc.post(new Eq(nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, b, functionalCase, generalCase)
    }

    private def compileBinaryConstraint2
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X],
         B <: Value[B], E <: Domain[B, E], Y <: Variable[B, E, Y]]
        (createConstraint: (Id[yuck.core.Constraint], Option[Goal], immutable.IndexedSeq[X], Y) => yuck.core.Constraint,
         enforceDomain: (Seq[X], Y) => Iterable[BooleanVariable],
         maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using
         aHelper: CompilationHelper[A, D, X],
         bHelper: CompilationHelper[B, E, Y]):
        Iterable[BooleanVariable] =
    {
        given bTraits: TypeTraits[B, E, Y] = bHelper.typeTraits
        val Seq(b, as) = constraint.params: @unchecked
        val xs = aHelper.compileArray(as)
        val y = bHelper.compileExpr(b)
        def functionalCase = {
            cc.post(createConstraint(nextConstraintId(), maybeGoal, xs, y))
            enforceDomain(xs, y)
        }
        def generalCase = {
            val channel = bHelper.createChannel()
            cc.post(createConstraint(nextConstraintId(), maybeGoal, xs, channel))
            val costs = createBoolChannel()
            cc.post(new Eq(nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, b, functionalCase, generalCase)
    }

    private val compileBinaryIntConstraint2 =
        compileBinaryConstraint2[
            IntegerValue, IntegerDomain, IntegerVariable,
            IntegerValue, IntegerDomain, IntegerVariable]

    private def compileTernaryConstraint
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X],
         B <: Value[B], E <: Domain[B, E], Y <: Variable[B, E, Y],
         C <: Value[C], F <: Domain[C, F], Z <: Variable[C, F, Z]]
        (createConstraint: (Id[yuck.core.Constraint], Option[Goal], X, Y, Z) => yuck.core.Constraint,
         enforceDomain: (X, Y, Z) => Iterable[BooleanVariable],
         maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using
         aHelper: CompilationHelper[A, D, X],
         bHelper: CompilationHelper[B, E, Y],
         cHelper: CompilationHelper[C, F, Z]):
        Iterable[BooleanVariable] =
    {
        given cTraits: TypeTraits[C, F, Z] = cHelper.typeTraits
        val Seq(a, b, c) = constraint.params: @unchecked
        val x = aHelper.compileExpr(a)
        val y = bHelper.compileExpr(b)
        val z = cHelper.compileExpr(c)
        def functionalCase = {
            cc.post(createConstraint(nextConstraintId(), maybeGoal, x, y, z))
            enforceDomain(x, y, z)
        }
        def generalCase = {
            if z.isInstanceOf[BooleanVariable] && z.domain == TrueDomain then {
                val costs = cHelper.createChannel()
                cc.post(createConstraint(nextConstraintId(), maybeGoal, x, y, costs))
                List(costs.asInstanceOf[BooleanVariable])
            } else {
                val channel = cHelper.createChannel()
                cc.post(createConstraint(nextConstraintId(), maybeGoal, x, y, channel))
                val costs = createBoolChannel()
                cc.post(new Eq(nextConstraintId(), maybeGoal, channel, z, costs))
                List(costs)
            }
        }
        compileConstraint(constraint, c, functionalCase, generalCase)
    }

    private val compileTernaryBoolConstraint =
        compileTernaryConstraint[
            BooleanValue, BooleanDomain, BooleanVariable,
            BooleanValue, BooleanDomain, BooleanVariable,
            BooleanValue, BooleanDomain, BooleanVariable]

    private val compileTernaryIntConstraint =
        compileTernaryConstraint[
            IntegerValue, IntegerDomain, IntegerVariable,
            IntegerValue, IntegerDomain, IntegerVariable,
            IntegerValue, IntegerDomain, IntegerVariable]

    private val compileTernaryIntSetConstraint =
        compileTernaryConstraint[
            IntegerSetValue, IntegerSetDomain, IntegerSetVariable,
            IntegerSetValue, IntegerSetDomain, IntegerSetVariable,
            IntegerSetValue, IntegerSetDomain, IntegerSetVariable]

    private def compileBinPackingConstraint
        [Load <: NumericalValue[Load],
         LoadDomain <: NumericalDomain[Load, LoadDomain],
         LoadVariable <: NumericalVariable[Load, LoadDomain, LoadVariable]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         items: immutable.Seq[BinPackingItem[Load]],
         loads: immutable.Map[IntegerValue, LoadVariable]) // bin -> load
        (using loadTraits: NumericalTypeTraits[Load, LoadDomain, LoadVariable]):
        Iterable[BooleanVariable] =
    {
        require(items.forall(_.weight >= loadTraits.zero))
        val items1: immutable.IndexedSeq[BinPackingItem[Load]] =
            items
                .groupBy(_.bin)
                .view
                .mapValues(_.foldLeft(loadTraits.zero)((load, item) => load + item.weight))
                .filter((_, weight) => weight > loadTraits.zero)
                .map((bin, weight) => new BinPackingItem(bin, weight))
                .toVector
        val bins = items1.map(_.bin)
        val maxLoad = items.map(_.weight).sum(using loadTraits.numericalOperations)
        val loadDomainApproximation = loadTraits.createDomain(loadTraits.zero, maxLoad)
        def hasRedundantDomain(load: LoadVariable) =
            loadDomainApproximation.isSubsetOf(load.domain)
        def functionalCase = {
            cc.post(new BinPacking(nextConstraintId(), maybeGoal, items1, loads)(using loadTraits))
            loads.values.filterNot(hasRedundantDomain).flatMap(enforceDomain)
        }
        def generalCase = {
            val loads1: immutable.Map[IntegerValue, LoadVariable] = {
                val definedVars = new mutable.HashSet[LoadVariable]
                for (bin, load) <- loads yield
                    if ! definedVars.contains(load) && definesVar(constraint, bins, loads(bin)) then {
                        definedVars += load
                        bin -> load
                    }
                    else bin -> createNonNegativeChannel()(using loadTraits)
            }
            cc.post(new BinPacking(nextConstraintId(), maybeGoal, items1, loads1)(using loadTraits))
            val deltas: Iterable[BooleanVariable] =
                loads.flatMap((bin, load) =>
                    if load == loads1(bin)
                    then if hasRedundantDomain(load) then Nil else enforceDomain(load)
                    else {
                        val delta = createBoolChannel()
                        cc.post(new Eq(nextConstraintId(), maybeGoal, load, loads1(bin), delta))
                        List(delta)
                    }
                )
            deltas
        }
        compileConstraint(constraint, loads.values, functionalCase, generalCase)
    }

    private def compileDeliveryConstraint
        [Time <: NumericalValue[Time],
         TimeDomain <: NumericalDomain[Time, TimeDomain],
         TimeVariable <: NumericalVariable[Time, TimeDomain, TimeVariable]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using timeHelper: NumericalCompilationHelper[Time, TimeDomain, TimeVariable]):
        Iterable[BooleanVariable] =
    {
        given timeTraits: NumericalTypeTraits[Time, TimeDomain, TimeVariable] = timeHelper.typeTraits
        val Seq(startNodes0, endNodes0, succ0, IntConst(offset), arrivalTimes0, serviceTimes0, travelTimes0,
                    BoolConst(withWaiting), totalTravelTime0) =
            constraint.params: @unchecked
        val startNodes = compileIntSetExpr(startNodes0).domain.singleValue.set
        val endNodes = compileIntSetExpr(endNodes0).domain.singleValue.set
        val succ = compileIntArray(succ0)
        val nodes = IntegerRange(offset, offset + succ.size - 1)
        val arrivalTimes = timeHelper.compileArray(arrivalTimes0)
        val serviceTimes1 = timeHelper.compileArray(serviceTimes0).map(_.domain.singleValue)
        require(serviceTimes1.isEmpty || serviceTimes1.size == nodes.size)
        val serviceTimes: Int => Time =
            if serviceTimes1.isEmpty then _ => timeTraits.zero else i => serviceTimes1(i)
        val travelTimes1 =
            timeHelper.compileArray(travelTimes0)
                .map(_.domain.singleValue)
                .grouped(arrivalTimes.size)
                .toVector
        require(travelTimes1.isEmpty || (travelTimes1.size == nodes.size && travelTimes1.forall(_.size == nodes.size)))
        val travelTimesAreSymmetric =
            travelTimes1.isEmpty ||
            Range(0, nodes.size).forall(
                i => Range(i + 1, nodes.size).forall(j => travelTimes1(i)(j) == travelTimes1(j)(i)))
        val travelTimes2 =
            if ! travelTimes1.isEmpty && travelTimesAreSymmetric
            then Vector.tabulate(nodes.size)(i => travelTimes1(i).drop(i))
            else travelTimes1
        val travelTimes: (Int, Int) => Time =
            if travelTimes2.isEmpty
            then (_, _) => timeTraits.zero
            else if travelTimesAreSymmetric
            then (i, j) => if i <= j then travelTimes2(i)(j - i) else travelTimes2(j)(i - j)
            else (i, j) => travelTimes2(i)(j)
        val totalTravelTime1 = timeHelper.compileExpr(totalTravelTime0)
        val totalTravelTime = {
            if travelTimes2.isEmpty then {
                // avoid an Eq constraint
                totalTravelTime1.pruneDomain(timeTraits.createDomain(Set(timeTraits.zero)))
                timeHelper.createChannel()
            } else {
                totalTravelTime1
            }
        }
        val costs = createBoolChannel()
        def functionalCase = {
            val delivery =
                new Delivery
                    [Time, TimeDomain, TimeVariable]
                    (WeakReference(cc.space), nextConstraintId(), maybeGoal,
                     startNodes, endNodes, succ, safeToInt(offset), arrivalTimes, serviceTimes, travelTimes,
                     withWaiting, totalTravelTime, costs)
            cc.post(delivery)
            List(costs)
        }
        def generalCase = {
            val nodes = IntegerRange(offset, offset + succ.size - 1)
            val arrivalTimes1: immutable.IndexedSeq[TimeVariable] = {
                val definableVars = this.definedVars(constraint)
                val definedVars = new mutable.HashSet[TimeVariable]
                nodes.valuesIterator.map(i =>
                    val arrivalTime = arrivalTimes(safeToInt(safeSub(i.value, offset)))
                    if startNodes.contains(i) then {
                        arrivalTime
                    }
                    else if definableVars.contains(arrivalTime) && ! definedVars.contains(arrivalTime) &&
                             isViableConstraint(succ, arrivalTime) then
                    {
                        definedVars += arrivalTime
                        arrivalTime
                    }
                    else timeTraits.createVariable(cc.space, "", arrivalTime.domain)
                ).toVector
            }
            val totalTravelTime1: TimeVariable =
                if isViableConstraint(succ, totalTravelTime)
                then totalTravelTime
                else timeTraits.createVariable(cc.space, "", totalTravelTime.domain)
            val delivery =
                new Delivery
                    [Time, TimeDomain, TimeVariable]
                    (WeakReference(cc.space), nextConstraintId(), maybeGoal,
                     startNodes, endNodes, succ, safeToInt(offset), arrivalTimes1, serviceTimes, travelTimes,
                     withWaiting, totalTravelTime1, costs)
            cc.space.post(delivery)
            val pairs = (arrivalTimes :+ totalTravelTime).zip(arrivalTimes1 :+ totalTravelTime1)
            val deltas: Iterable[BooleanVariable] =
                pairs.flatMap((x, x1) =>
                    if x == x1 then {
                        enforceDomain(x)
                    } else {
                        val delta = createBoolChannel()
                        cc.post(new Eq(nextConstraintId(), maybeGoal, x, x1, delta))
                        List(delta)
                    }
                )
            deltas.view.concat(List(costs))
        }
        compileConstraint(
            constraint,
            nodes.diff(startNodes).values.view.map(i => arrivalTimes(safeToInt(safeSub(i.value, offset)))) ++ Seq(totalTravelTime),
            functionalCase, generalCase)
    }

    private def compileLinearCombination
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (maybeGoal: Option[Goal],
         as0: Expr, bs: Expr,
         maybeChannel: Option[X] = None)
        (using helper: NumericalCompilationHelper[A, D, X]):
        X =
    {
        given typeTraits: NumericalTypeTraits[A, D, X] = helper.typeTraits
        val zero = typeTraits.zero
        val one = typeTraits.one
        val minusOne = one.negated
        val as = helper.compileArray(as0)
        val xs = helper.compileArray(bs)
        require(as.size == xs.size)
        val axs =
            AX.normalize(
                for (x, y) <- as.view.zip(xs.view)
                    if x.domain.singleValue != zero && (! y.domain.isSingleton || y.domain.singleValue != zero)
                yield
                    new AX(x.domain.singleValue, y))
        axs match {
            case List(AX(`one`, x)) if maybeChannel.isEmpty =>
                x
            case List(AX(`one`, x), AX(`minusOne`, y)) =>
                val channel = maybeChannel.getOrElse(helper.createChannel())
                cc.post(new Minus(nextConstraintId(), maybeGoal, x, y, channel))
                channel
            case List(AX(`minusOne`, x), AX(`one`, y)) =>
                val channel = maybeChannel.getOrElse(helper.createChannel())
                cc.post(new Minus(nextConstraintId(), maybeGoal, y, x, channel))
                channel
            case _ =>
                val channel = maybeChannel.getOrElse(helper.createChannel())
                if axs.forall(_.a == one) then {
                    if axs.size == 2 then {
                        val List(AX(_, x), AX(_, y)) = axs
                        cc.post(new Plus(nextConstraintId(), maybeGoal, x, y, channel))
                    } else {
                        val xs = axs.iterator.map(_.x).toVector
                        cc.post(new Sum(nextConstraintId(), maybeGoal, xs , channel))
                    }
                } else {
                   cc.post(new LinearCombination(nextConstraintId(), maybeGoal, axs.toVector, channel))
                }
                channel
        }
    }

    private def compileLinearConstraint
        [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
        (maybeGoal: Option[Goal],
         as0: Expr, bs: Expr, relation: OrderingRelation, c: Expr,
         maybeCosts: Option[BooleanVariable] = None)
        (using helper: NumericalCompilationHelper[A, D, X]):
        BooleanVariable =
    {
        given typeTraits: NumericalTypeTraits[A, D, X] = helper.typeTraits
        val zero = typeTraits.zero
        val as = helper.compileArray(as0)
        val xs = helper.compileArray(bs)
        require(as.size == xs.size)
        val axs =
            AX.normalize(
                for (x, y) <- as.view.zip(xs.view)
                    if x.domain.singleValue != zero && (! y.domain.isSingleton || y.domain.singleValue != zero)
                yield
                    new AX(x.domain.singleValue, y))
        val y = helper.createChannel()
        val z = helper.compileExpr(c)
        val costs = maybeCosts.getOrElse(createBoolChannel())
        if axs.forall(_.a == typeTraits.one) then {
            cc.post(new SumConstraint(nextConstraintId(), maybeGoal, axs.map(_.x).toVector, y, relation, z, costs))
        } else {
            cc.post(new LinearConstraint(nextConstraintId(), maybeGoal, axs.toVector, y, relation, z, costs))
        }
        costs
    }

    private def compileCountConstraint
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         maybeCosts: Option[BooleanVariable] = None)
        (using helper: CompilationHelper[A, D, X]):
        Iterable[BooleanVariable] =
    {
        given typeTraits: TypeTraits[A, D, X] = helper.typeTraits
        val Constraint(Count(relation, _), Seq(as, a, b), _) = constraint: @unchecked
        val y = helper.compileExpr(a)
        // If xs(j) does not play a role (because its domain is disjoint from y.domain and hence
        // its values will never be counted), we omit xs(j) from the constraint and hence an
        // useless arc from the constraint network.
        val xs = helper.compileArray(as).filter(_.domain.intersects(y.domain))
        val m = compileIntExpr(b)
        def functionalCase = {
            if y.domain.isSingleton then {
                cc.post(new CountConst(nextConstraintId(), maybeGoal, xs, y.domain.singleValue, m))
                val minCount = xs.count(_.domain == y.domain)
                val maxCount = xs.count(_.domain.intersects(y.domain))
                if IntegerRange(minCount, maxCount).isSubsetOf(m.domain) then Nil else enforceIntDomain(m)
            } else {
                cc.post(new CountVar(nextConstraintId(), maybeGoal, xs, y, m))
                if IntegerRange(0, xs.size).isSubsetOf(m.domain) then Nil else enforceIntDomain(m)
            }
        }
        def generalCase = {
            val n = createNonNegativeIntChannel()
            if y.domain.isSingleton then {
                cc.post(new CountConst(nextConstraintId(), maybeGoal, xs, y.domain.singleValue, n))
            } else {
                cc.post(new CountVar(nextConstraintId(), maybeGoal, xs, y, n))
            }
            val costs = maybeCosts.getOrElse(createBoolChannel())
            relation match {
                case "eq" => cc.post(new Eq(nextConstraintId(), maybeGoal, m, n, costs))
                case "neq" => cc.post(new Ne(nextConstraintId(), maybeGoal, m, n, costs))
                case "leq" => cc.post(new Le(nextConstraintId(), maybeGoal, m, n, costs))
                case "lt" => cc.post(new Lt(nextConstraintId(), maybeGoal, m, n, costs))
                case "geq" => cc.post(new Le(nextConstraintId(), maybeGoal, n, m, costs))
                case "gt" => cc.post(new Lt(nextConstraintId(), maybeGoal, n, m, costs))
             }
            List(costs)
        }
        if relation == "eq" && maybeCosts.isEmpty then {
            compileConstraint(constraint, List(m), functionalCase, generalCase)
        } else {
            generalCase
        }
    }

    private def compileElementConstraint
        [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (using helper: CompilationHelper[A, D, X]):
        Iterable[BooleanVariable] =
    {
        given typeTraits: TypeTraits[A, D, X] = helper.typeTraits
        val Seq(IntConst(offset0), b, as, c) =
            if constraint.params.size == 4
            then constraint.params: @unchecked
            else IntConst(1) +: constraint.params: @unchecked
        val i = compileIntExpr(b)
        val xs = helper.compileArray(as)
        val y = helper.compileExpr(c)
        val indexRange = IntegerRange(offset0, offset0 + xs.size - 1)
        val offset = indexRange.lb.toInt
        if ! i.domain.intersects(indexRange) then {
            throw new InconsistentConstraintException(constraint)
        }
        def post(y: X): X = {
            if xs.forall(_.domain.isSingleton) then {
                val as = xs.map(_.domain.singleValue)
                cc.post(new ElementConst(nextConstraintId(), maybeGoal, as, i, y, offset))
            } else {
                cc.post(new ElementVar(nextConstraintId(), maybeGoal, xs, i, y, offset))
            }
            y
        }
        def functionalCase = {
            post(y)
            if xs.forall(_.domain.isSubsetOf(y.domain)) then Nil else enforceDomain(y)
        }
        def generalCase = {
            val channel = post(helper.createChannel())
            val costs = createBoolChannel()
            cc.post(new Eq(nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, List(y), functionalCase, generalCase)
    }

    private def compileIfThenElseConstraint
        [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using helper: OrderedCompilationHelper[A, D, X]):
        Iterable[BooleanVariable] =
    {
        given typeTraits: OrderedTypeTraits[A, D, X] = helper.typeTraits
        val cs = compileBoolArray(constraint.params(0))
        val xs = helper.compileArray(constraint.params(1))
        val y = helper.compileExpr(constraint.params(2))
        require(cs.size == xs.size)
        require(cs.size >= 2)
        require(cs.last.domain.isSingleton)
        require(cs.last.domain.singleValue == True)
        def post(y: X): X = {
            cc.post(new IfThenElse(nextConstraintId(), maybeGoal, cs, xs, y))
            y
        }
        def functionalCase = {
            post(y)
            if xs.forall(_.domain.isSubsetOf(y.domain)) then Nil else enforceDomain(y)
        }
        def generalCase = {
            val channel = post(createOrdChannel()(using typeTraits))
            val costs = createBoolChannel()
            cc.post(new Eq(nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, List(y), functionalCase, generalCase)
    }

    private def compileLexLessConstraint
        [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         relation: OrderingRelation,
         maybeCosts: Option[BooleanVariable] = None)
        (using helper: OrderedCompilationHelper[A, D, X], ord: Ordering[A]):
        Iterable[BooleanVariable] =
    {
        given typeTraits: OrderedTypeTraits[A, D, X] = helper.typeTraits
        val xs = helper.compileArray(constraint.params(0))
        val ys = helper.compileArray(constraint.params(1))
        val costs = maybeCosts.getOrElse(createBoolChannel())
        (relation: @unchecked) match {
            case LtRelation => cc.post(new LexLess(nextConstraintId(), maybeGoal, xs, ys, costs))
            case LeRelation => cc.post(new LexLessEq(nextConstraintId(), maybeGoal, xs, ys, costs))
        }
        List(costs)
    }

    private def compileReifiedConstraint
        (maybeGoal: Option[Goal],
         reifiedConstraint: yuck.flatzinc.ast.Constraint,
         maybeCosts: Option[BooleanVariable] = None):
        Iterable[BooleanVariable] =
    {
        val Constraint(Reif(name), params, annotations) = reifiedConstraint: @unchecked
        val constraint = Constraint(name, params.take(params.size - 1), annotations)
        val satisfied = compileBoolExpr(params.last)
        if compilesToConst(params.last, True) then {
            if cc.impliedConstraints.contains(constraint)
            then Nil
            else compileConstraint(maybeGoal, constraint)
        } else if cc.impliedConstraints.contains(constraint) then {
            def functionalCase = {
                postConjunction(maybeGoal, Nil, Some(satisfied))
                enforceBoolDomain(satisfied)
            }
            def generalCase = {
                List(satisfied)
            }
            compileConstraint(reifiedConstraint, List(satisfied), functionalCase, generalCase)
        } else {
            def functionalCase = {
                val costs0 = compileConstraint(maybeGoal, constraint, Some(satisfied)).toVector
                if costs0.size != 1 || costs0.head != satisfied then {
                    postConjunction(maybeGoal, costs0, Some(satisfied))
                }
                enforceBoolDomain(satisfied)
            }
            def generalCase = {
                val costs0 = compileConstraint(maybeGoal, constraint, None).toVector
                val costs = createBoolChannel()
                if costs0.size == 1 then {
                    cc.post(new Eq(nextConstraintId(), maybeGoal, costs0.head, satisfied, costs))
                } else {
                    val costs1 = postConjunction(maybeGoal, costs0)
                    cc.post(new Eq(nextConstraintId(), maybeGoal, costs1, satisfied, costs))
                }
                List(costs)
            }
            compileConstraint(reifiedConstraint, List(satisfied), functionalCase, generalCase)
        }
    }

    private def postDisjunction
        (maybeGoal: Option[Goal], xs0: Seq[BooleanVariable], maybeY: Option[BooleanVariable] = None):
        BooleanVariable =
    {
        val xs = xs0.iterator.filterNot(_.domain == FalseDomain).toSet.toVector
        if xs.size == 1 && maybeY.isEmpty then {
            xs(0)
        } else {
            val y = maybeY.getOrElse(createBoolChannel())
            if xs.size == 2 then {
               cc.post(new Or(nextConstraintId(), maybeGoal, xs(0), xs(1), y))
            } else {
                cc.post(new Disjunction(nextConstraintId(), maybeGoal, xs, y))
            }
            y
        }
    }

    private def postConjunction
        (maybeGoal: Option[Goal], xs0: Seq[BooleanVariable], maybeY: Option[BooleanVariable] = None):
        BooleanVariable =
    {
        val xs = xs0.iterator.filterNot(_.domain == TrueDomain).toSet.toVector
        if xs.size == 1 && maybeY.isEmpty then {
            xs(0)
        } else {
            val y = maybeY.getOrElse(createBoolChannel())
            if xs.size == 2 then {
                cc.post(new And(nextConstraintId(), maybeGoal, xs(0), xs(1), y))
            } else {
                cc.post(new Conjunction(nextConstraintId(), maybeGoal, xs, y))
            }
            y
        }
    }

    private object DomainEnforcementGoal extends Goal {
        override def toString = "Domain enforcement"
    }

    private def enforceDomain(x: AnyVariable): List[BooleanVariable] = {
        x match {
            case x: BooleanVariable => enforceBoolDomain(x)
            case x: IntegerVariable => enforceIntDomain(x)
            case x: IntegerSetVariable => enforceIntSetDomain(x)
        }
    }

    private def enforceBoolDomain(x: BooleanVariable): List[BooleanVariable] = {
        val dx = x.domain
        if dx.isSingleton then {
            if cc.space.isChannelVariable(x) then {
                if dx.singleValue.truthValue then {
                    List(x)
                } else {
                    val costs = createBoolChannel()
                    cc.post(new Not(nextConstraintId(), Some(DomainEnforcementGoal), x, costs))
                    List(costs)
                }
            } else {
                cc.space.setValue(x, dx.singleValue)
                Nil
            }
        } else {
            Nil
        }
    }

    private def enforceIntDomain(x: IntegerVariable): List[BooleanVariable] = {
        val dx = x.domain
        if dx.isBounded then {
            if cc.space.isChannelVariable(x) then {
                val costs = createBoolChannel()
                cc.post(new Contains(nextConstraintId(), Some(DomainEnforcementGoal), x, x.domain, costs))
                List(costs)
            } else if dx.isSingleton then {
                cc.space.setValue(x, dx.singleValue)
                Nil
            } else {
                Nil
            }
        } else {
            Nil
        }
    }

    private def enforceIntSetDomain(x: IntegerSetVariable): List[BooleanVariable] = {
        val dx = x.domain
        if dx.isBounded then {
            if cc.space.isChannelVariable(x) then {
                val costs = createBoolChannel()
                dx match {
                    case dx: IntegerPowerSetDomain =>
                        cc.post(new Subset(nextConstraintId(), Some(DomainEnforcementGoal), x, dx.base, costs))
                    case dx: SingletonIntegerSetDomain =>
                        cc.post(new Eq(nextConstraintId(), Some(DomainEnforcementGoal), x, dx.base, costs))
                }
                List(costs)
            } else if dx.isSingleton then {
                cc.space.setValue(x, dx.singleValue)
                Nil
            } else {
                Nil
            }
        } else {
            Nil
        }
    }

    private def optimizeIntDomainEnforcement(): Unit = {
        val intDomainEnforcementConstraints =
            cc.costVars.iterator
                .map(cc.space.maybeDefiningConstraint)
                .filter(_.isDefined)
                .map(_.get)
                .filter(_.isInstanceOf[Contains])
                .filter(_.maybeGoal.contains(DomainEnforcementGoal))
                .toVector
        if intDomainEnforcementConstraints.size > 32 then {
            // When there are many integer channels, we enforce their domains using a single InDomain constraint.
            // This way we speed up neighbourhood generation and reduce the overhead of goal tracking.
            intDomainEnforcementConstraints.foreach(cc.space.retract)
            cc.costVars --= intDomainEnforcementConstraints.iterator.map(_.outVariables.head.asInstanceOf[BooleanVariable])
            val xs = intDomainEnforcementConstraints.map(_.inVariables.head.asInstanceOf[IntegerVariable])
            val costs = createBoolChannel()
            cc.post(new InDomain(nextConstraintId(), Some(DomainEnforcementGoal), xs, costs))
            cc.costVars += costs
        } else {
            // InDomain comes with more overhead than Contains and, when there are only a few integer channels,
            // a set of Contains constraints is faster than a single InDomain constraint.
        }
    }

}

object ConstraintFactory {

    private val Count = "yuck_count_(.*)_(.*)".r
    private val IntLin = "int_lin_(.*)".r
    private val Reif = "(.*)_reif".r

    // In Yuck, True < False, but in FlatZinc, false < true.
    private val FlatZincBooleanValueOrdering = BooleanValueOrdering.reverse

}
