package yuck.flatzinc.compiler

import scala.collection.*
import scala.ref.WeakReference

import yuck.constraints.*
import yuck.core.{given, *}
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
 *
 * @author Michael Marte
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

    private def compileConstraint
        (constraint: yuck.flatzinc.ast.Constraint,
         out: Iterable[AnyVariable],
         functionalCase: => Iterable[BooleanVariable],
         generalCase: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
    {
        val definableVars = definedVars(constraint)
        if (out.forall(x =>
                ! x.domain.isSingleton &&
                ! cc.searchVars.contains(x) &&
                cc.space.maybeDefiningConstraint(x).isEmpty &&
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
         out: Expr,
         functionalCase: => Iterable[BooleanVariable],
         generalCase: => Iterable[BooleanVariable]):
        Iterable[BooleanVariable] =
        compileConstraint(constraint, List(compileAnyExpr(out)), functionalCase, generalCase)

    override def run() = {
        cc.costVars ++=
            cc.ast.constraints
            .iterator
            .flatMap(constraint =>
                compileConstraint(if (cc.cfg.attachGoals) Some(new FlatZincGoal(constraint)) else None,
                constraint,
                None))
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
        if (cc.sigint.isSet) {
            throw new FlatZincCompilerInterruptedException
        }
        if (cc.impliedConstraints.contains(constraint)) {
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
                cc.space.post(new Bool2Int1(nextConstraintId(), maybeGoal, x, y))
                if IntegerRange(0, 1).diff(y.domain).isEmpty
                then Nil
                else enforceIntDomain(y)
            }
            def generalCase = {
                val costs = createBoolChannel()
                cc.space.post(new Bool2Int2(nextConstraintId(), maybeGoal, x, y, costs))
                List(costs)
            }
            compileConstraint(constraint, List(y), functionalCase, generalCase)
        case Constraint("bool2costs", Seq(a, b), _) =>
            def functionalCase = {
                cc.space.post(new Bool2Costs1(nextConstraintId(), maybeGoal, a, b))
                enforceIntDomain(b)
            }
            def generalCase = {
                val costs = createBoolChannel()
                cc.space.post(new Bool2Costs2(nextConstraintId(), maybeGoal, a, b, costs))
                List(costs)
            }
            compileConstraint(constraint, b, functionalCase, generalCase)
        case Constraint("bool_not", Seq(a, b), _) =>
            def functionalCase = {
                cc.space.post(new Not(nextConstraintId(), maybeGoal, a, b))
                enforceBoolDomain(b)
            }
            def generalCase = {
                val costs = createBoolChannel()
                cc.space.post(new Ne[BooleanValue](nextConstraintId(), maybeGoal, a, b, costs))
                List(costs)
            }
            compileConstraint(constraint, b, functionalCase, generalCase)
        case Constraint("bool_eq", _, _) =>
            compileOrderingConstraint[BooleanValue](maybeGoal, constraint, EqRelation, maybeCosts)
        case Constraint("bool_lt", _, _) =>
            compileOrderingConstraint[BooleanValue](maybeGoal, constraint, LtRelation, maybeCosts)
        case Constraint("bool_le", _, _) =>
            compileOrderingConstraint[BooleanValue](maybeGoal, constraint, LeRelation, maybeCosts)
        case Constraint("bool_and", _, _) =>
            compileTernaryBoolConstraint(new And(_, _, _, _, _), (_, _, z) => enforceBoolDomain(z), maybeGoal, constraint)
        case Constraint("bool_or", _, _) =>
            compileTernaryBoolConstraint(new Or(_, _, _, _, _), (_, _, z) => enforceBoolDomain(z), maybeGoal, constraint)
        case Constraint("bool_xor", _, _) =>
            compileTernaryBoolConstraint(new Ne[BooleanValue](_, _, _, _, _), (_, _, z) => enforceBoolDomain(z), maybeGoal, constraint)
        case Constraint("array_bool_and", Seq(as, b), _) =>
            val xs = compileBoolArray(as)
            val y = compileBoolExpr(b)
            def functionalCase = {
                postConjunction(maybeGoal, xs, Some(y))
                enforceBoolDomain(y)
            }
            def generalCase = {
                if (y.domain == TrueDomain) {
                    List(postConjunction(maybeGoal, xs))
                } else {
                    val costs0 = postConjunction(maybeGoal, xs)
                    val costs = createBoolChannel()
                    cc.space.post(new Eq[BooleanValue](nextConstraintId(), maybeGoal, costs0, y, costs))
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
                if (y.domain == TrueDomain) {
                    List(postDisjunction(maybeGoal, xs))
                } else {
                    val costs0 = postDisjunction(maybeGoal, xs)
                    val costs = createBoolChannel()
                    cc.space.post(new Eq[BooleanValue](nextConstraintId(), maybeGoal, costs0, y, costs))
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
            if (maybeY.isDefined) {
                val y = maybeY.get
                val trueCount = createNonNegativeIntChannel()
                cc.space.post(new CountConst[BooleanValue](nextConstraintId(), maybeGoal, xs.filter(_ != y), True, trueCount))
                cc.space.post(new Even(nextConstraintId(), maybeGoal, trueCount, y))
                Nil
            } else {
                val trueCount = createNonNegativeIntChannel()
                cc.space.post(new CountConst[BooleanValue](nextConstraintId(), maybeGoal, xs, True, trueCount))
                val costs = createBoolChannel()
                cc.space.post(new Uneven[IntegerValue](nextConstraintId(), maybeGoal, trueCount, costs))
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
                    cc.space.post(new Not(nextConstraintId(), maybeGoal, costs0, costs))
                    List(costs)
                case (_, Nil) =>
                    List(postDisjunction(maybeGoal, xs, maybeCosts))
                case _ =>
                    val costs0 = postDisjunction(maybeGoal, xs)
                    val costs1 = postConjunction(maybeGoal, ys)
                    val costs = maybeCosts.getOrElse(createBoolChannel())
                    cc.space.post(new Le(nextConstraintId(), maybeGoal, costs1, costs0, costs))
                    List(costs)
            }
        case Constraint("int_eq", _, _) =>
            compileOrderingConstraint[IntegerValue](maybeGoal, constraint, EqRelation, maybeCosts)
        case Constraint("int_ne", _, _) =>
            compileOrderingConstraint[IntegerValue](maybeGoal, constraint, NeRelation, maybeCosts)
        case Constraint("int_lt", _, _) =>
            compileOrderingConstraint[IntegerValue](maybeGoal, constraint, LtRelation, maybeCosts)
        case Constraint("int_le", _, _) =>
             compileOrderingConstraint[IntegerValue](maybeGoal, constraint, LeRelation, maybeCosts)
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
            compileBinaryConstraint1[IntegerValue, IntegerVariable, IntegerValue, IntegerVariable](
                new Abs(_, _, _, _),
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
                constraint.copy(params = as :: bs :: IntConst(getConst[IntegerValue](c).value) :: t),
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
            if ! definesVar(constraint, c) && as.iterator.zip(bs.iterator).exists {
                case (IntConst(a), b) => (a == -1 || a == 1) && definesVar(constraint, b)
            } =>
            val abs = as.zip(bs)
            val (a, b) = abs.find { case (IntConst(a), b) => (a == -1 || a == 1) && definesVar(constraint, b) }.get
            a match {
                case IntConst(1) =>
                    // b1 + a2 b2 + ... = c
                    // b1               = c - a2 b2 - ...
                    val (as1, bs1) = (for (case (IntConst(a), b1) <- abs if b1 != b) yield (IntConst(-a), b1)).unzip
                    compileConstraint(
                        maybeGoal,
                        Constraint("int_lin_eq", List(ArrayConst(IntConst(1) +: as1), ArrayConst(c +: bs1), b), annotations),
                        maybeCosts)
                case IntConst(-1) =>
                    // -1 b1 + a2 b2 + ... =    c
                    // -1 b1               =    c - a2 b2 - ...
                    //    b1               = -1 c + a2 b2 + ...
                    val bs1 = for (b1 <- bs) yield if (b1 == b) c else b1
                    compileConstraint(
                        maybeGoal,
                        Constraint("int_lin_eq", List(ArrayConst(as), ArrayConst(bs1), b), annotations),
                        maybeCosts)
            }
        case Constraint("int_lin_eq", Seq(as, bs, c), _) =>
            def functionalCase = {
                val y = compileIntExpr(c)
                compileLinearCombination[IntegerValue](maybeGoal, as, bs, Some(y))
                val lhs = compileIntArray(as).view.map(_.domain.singleValue).zip(compileIntArray(bs).view.map(_.domain))
                if IntegerDomainPruner.linEqRule(lhs, CompleteIntegerRange)._2.isSubsetOf(y.domain)
                then Nil
                else enforceIntDomain(y)
            }
            def generalCase = {
                List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, EqRelation, c, maybeCosts))
            }
            compileConstraint(constraint, c, functionalCase, generalCase)
        case Constraint("int_lin_ne", Seq(as, bs, c), _) =>
            List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, NeRelation, c, maybeCosts))
        case Constraint("int_lin_le", Seq(as, bs, c), _) =>
            List(compileLinearConstraint[IntegerValue](maybeGoal, as, bs, LeRelation, c, maybeCosts))
        case Constraint("array_int_maximum", _, _) =>
            compileBinaryConstraint2[IntegerValue, IntegerVariable, IntegerValue, IntegerVariable](
                new Maximum[IntegerValue](_, _, _, _),
                (xs, y) =>
                    if IntegerDomainPruner.maxRule(xs.view.map(_.domain), CompleteIntegerRange)._2.isSubsetOf(y.domain)
                    then Nil
                    else enforceIntDomain(y),
                maybeGoal,
                constraint)
        case Constraint("array_int_minimum", _, _) =>
            compileBinaryConstraint2[IntegerValue, IntegerVariable, IntegerValue, IntegerVariable](
                new Minimum[IntegerValue](_, _, _, _),
                (xs, y) =>
                    if IntegerDomainPruner.minRule(xs.view.map(_.domain), CompleteIntegerRange)._2.isSubsetOf(y.domain)
                    then Nil
                    else enforceIntDomain(y),
                maybeGoal,
                constraint)
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
        case Constraint("set_eq", _, _) =>
            compileOrderingConstraint[IntegerSetValue](maybeGoal, constraint, EqRelation, maybeCosts)
        case Constraint("set_ne", _, _) =>
            compileOrderingConstraint[IntegerSetValue](maybeGoal, constraint, NeRelation, maybeCosts)
        case Constraint("set_lt", _, _) =>
            compileOrderingConstraint[IntegerSetValue](maybeGoal, constraint, LtRelation, maybeCosts)
        case Constraint("set_le", _, _) =>
            compileOrderingConstraint[IntegerSetValue](maybeGoal, constraint, LeRelation, maybeCosts)
        case Constraint("set_card", _, _) =>
            def enforceDomain(x: IntegerSetVariable, y: IntegerVariable) = {
                val (minCard, maxCard) = x.domain match {
                    case dx: SingletonIntegerSetDomain => (dx.base.size, dx.base.size)
                    case dx: IntegerPowersetDomain => (0, dx.base.size)
                }
                if IntegerRange(minCard, maxCard).isSubsetOf(y.domain) then Nil else enforceIntDomain(y)
            }
            compileBinaryConstraint1
                [IntegerSetValue, IntegerSetVariable, IntegerValue, IntegerVariable]
                (new SetCardinality(_, _, _, _), enforceDomain, maybeGoal, constraint)
        case Constraint("set_in", Seq(a, b), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new Contains(nextConstraintId(), maybeGoal, a, b, costs))
            List(costs)
        case Constraint("set_subset", Seq(a, b), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new Subset(nextConstraintId(), maybeGoal, a, b, costs))
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
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new Alldistinct[IntegerValue](nextConstraintId(), maybeGoal, as, costs, cc.logger))
            List(costs)
        case Constraint("fzn_all_different_set", Seq(as), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new Alldistinct[IntegerSetValue](nextConstraintId(), maybeGoal, as, costs, cc.logger))
            List(costs)
        case Constraint("fzn_alldifferent_except", Seq(as, s), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new AlldistinctExcept(nextConstraintId(), maybeGoal, as, s.set.values.toSet, costs))
            List(costs)
        case Constraint("fzn_alldifferent_except_0", Seq(as), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new AlldistinctExcept(nextConstraintId(), maybeGoal, as, immutable.Set(Zero), costs))
            List(costs)
        case Constraint("fzn_increasing_bool", Seq(as), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new BooleanIncreasing(nextConstraintId(), maybeGoal, as, costs))
            List(costs)
        case Constraint("yuck_increasing_int", Seq(as, BoolConst(strict)), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new IntegerIncreasing(nextConstraintId(), maybeGoal, as, strict, costs))
            List(costs)
        case Constraint("fzn_nvalue", _, _) =>
            compileBinaryConstraint2[IntegerValue, IntegerVariable, IntegerValue, IntegerVariable](
                new NumberOfDistinctValues[IntegerValue](_, _, _, _),
                (xs, y) => if IntegerRange(if xs.isEmpty then 0 else 1, xs.size).isSubsetOf(y.domain) then Nil else enforceIntDomain(y),
                maybeGoal,
                constraint)
        case Constraint(Count(_, "bool"), _, _) =>
            compileCountConstraint[BooleanValue](maybeGoal, constraint, maybeCosts)
        case Constraint(Count(_, "int"), _, _) =>
            compileCountConstraint[IntegerValue](maybeGoal, constraint, maybeCosts)
        case Constraint(Count(_, "set"), _, _) =>
            compileCountConstraint[IntegerSetValue](maybeGoal, constraint, maybeCosts)
        case Constraint("fzn_cumulative", Seq(s, d, r, b), _) =>
            val xs = compileIntArray(s)
            val ys = compileIntArray(d)
            val zs = compileIntArray(r)
            assert(xs.size == ys.size)
            assert(ys.size == zs.size)
            val tasks = for (((x, y), z) <- xs.zip(ys).zip(zs)) yield new CumulativeTask(x, y, z)
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new Cumulative(nextConstraintId(), maybeGoal, tasks, b, costs))
            List(costs)
        case Constraint("yuck_disjunctive", Seq(x, w, BoolConst(strict)), _) =>
            val xs = compileIntArray(x)
            val ws = compileIntArray(w)
            assert(xs.size == ws.size)
            val y = compileConstant(Zero)
            val h = compileConstant(One)
            val rects = Vector.tabulate(xs.size)(i => new Disjoint2Rect(xs(i), y, ws(i), h))
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new Disjoint2(nextConstraintId(), maybeGoal, rects, strict, costs))
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
            cc.space.post(new Disjoint2(nextConstraintId(), maybeGoal, rects, strict, costs))
            List(costs)
        case Constraint("yuck_table_bool", Seq(as, flatTable), _) =>
            val xs = compileBoolArray(as)
            val rows = compileBoolArray(flatTable).map(_.domain.singleValue).grouped(xs.size).toVector
            val costs = maybeCosts.getOrElse(createBoolChannel())
            val forceImplicitSolving = constraint.annotations.exists(forcesImplicitSolving)
            cc.space.post(new Table(nextConstraintId(), maybeGoal, xs, rows, costs, forceImplicitSolving))
            List(costs)
        case Constraint("yuck_table_int", Seq(as, flatTable), _) =>
            val xs = compileIntArray(as)
            val rows = compileIntArray(flatTable).map(_.domain.singleValue).grouped(xs.size).toVector
            val costs = maybeCosts.getOrElse(createBoolChannel())
            val forceImplicitSolving = constraint.annotations.exists(forcesImplicitSolving)
            cc.space.post(new Table(nextConstraintId(), maybeGoal, xs, rows, costs, forceImplicitSolving))
            List(costs)
        case Constraint("yuck_regular", Seq(xs, q, s, flatDelta, q0, f), _) =>
            val delta = compileIntArray(flatDelta).map(_.domain.singleValue.toInt).grouped(s.toInt).toVector
            val costs = maybeCosts.getOrElse(createBoolChannel())
            val dfa = new RegularDfa(xs, q.toInt, s.toInt, delta, q0.toInt, f.set)
            cc.space.post(new Regular(nextConstraintId(), maybeGoal, dfa, costs, cc.logger))
            List(costs)
        case Constraint("yuck_circuit", Seq(succ, IntConst(offset)), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new Circuit(nextConstraintId(), maybeGoal, succ, safeToInt(offset), costs, cc.logger, cc.sigint))
            List(costs)
        case Constraint("yuck_delivery", _, _) =>
            compileDeliveryConstraint[IntegerValue](maybeGoal, constraint)
        case Constraint("yuck_inverse", Seq(f, IntConst(fOffset), g, IntConst(gOffset)), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            val constraint = new Inverse(nextConstraintId(), maybeGoal, new InverseFunction(f, safeToInt(fOffset)), new InverseFunction(g, safeToInt(gOffset)), costs, cc.logger)
            val constraints = constraint.decompose(cc.space)
            constraints.foreach(cc.space.post)
            constraints.view.flatMap(_.outVariables).map(_.asInstanceOf[BooleanVariable])
        case Constraint("yuck_bin_packing_load", Seq(loads0, bins0, weights0, IntConst(minLoadIndex0)), _) =>
            val bins = compileIntArray(bins0)
            val weights = getArrayElems(weights0).map(getConst[IntegerValue])
            val minLoadIndex = safeToInt(minLoadIndex0)
            require(bins.size == weights.size)
            val itemGenerator =
                for ((bin, weight) <- bins.iterator.zip(weights.iterator)) yield
                    new BinPackingItem(bin, weight)
            val items = itemGenerator.toVector
            val loads1 = compileIntArray(loads0)
            val loads = (minLoadIndex until minLoadIndex + loads1.size).iterator.zip(loads1.iterator).toMap
            compileBinPackingConstraint(maybeGoal, constraint, items, loads)
        case Constraint("fzn_global_cardinality", Seq(xs0, cover0, counts0), _) =>
            val xs = compileIntArray(xs0)
            val items = xs.map(new BinPackingItem(_, One))
            val cover = getArrayElems(cover0).map(getConst[IntegerValue](_).toInt)
            val counts = compileIntArray(counts0)
            require(cover.size == counts.size)
            val loads = cover.iterator.zip(counts.iterator).toMap
            compileBinPackingConstraint(maybeGoal, constraint, items, loads)
        case Constraint("fzn_lex_less_int", Seq(as, bs), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new LexLess[IntegerValue](nextConstraintId(), maybeGoal, as, bs, costs))
            List(costs)
        case Constraint("fzn_lex_less_bool", Seq(as, bs), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new LexLess[BooleanValue](nextConstraintId(), maybeGoal, as, bs, costs)(using FlatZincBooleanValueOrdering))
            List(costs)
        case Constraint("fzn_lex_less_set", Seq(as, bs), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new LexLess[IntegerSetValue](nextConstraintId(), maybeGoal, as, bs, costs))
            List(costs)
        case Constraint("fzn_lex_lesseq_int", Seq(as, bs), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new LexLessEq[IntegerValue](nextConstraintId(), maybeGoal, as, bs, costs))
            List(costs)
        case Constraint("fzn_lex_lesseq_bool", Seq(as, bs), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new LexLessEq[BooleanValue](nextConstraintId(), maybeGoal, as, bs, costs)(using FlatZincBooleanValueOrdering))
            List(costs)
        case Constraint("fzn_lex_lesseq_set", Seq(as, bs), _) =>
            val costs = maybeCosts.getOrElse(createBoolChannel())
            cc.space.post(new LexLessEq[IntegerSetValue](nextConstraintId(), maybeGoal, as, bs, costs))
            List(costs)
        case Constraint("redundant_constraint", Seq(b), _) =>
            cc.costVarsFromRedundantConstraints += b
            Nil
    }

    private def compileOrderingConstraint
        [V <: OrderedValue[V]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         relation: OrderingRelation,
         maybeCosts: Option[BooleanVariable] = None)
        (using valueTraits: OrderedValueTraits[V]):
        Iterable[BooleanVariable] =
    {
        val Constraint(_, Seq(a, b), _) = constraint: @unchecked
        val costs = maybeCosts.getOrElse(createBoolChannel())
        relation match {
            case EqRelation => cc.space.post(new Eq[V](nextConstraintId(), maybeGoal, a, b, costs))
            case NeRelation => cc.space.post(new Ne[V](nextConstraintId(), maybeGoal, a, b, costs))
            case LtRelation => cc.space.post(new Lt[V](nextConstraintId(), maybeGoal, a, b, costs))
            case LeRelation => cc.space.post(new Le[V](nextConstraintId(), maybeGoal, a, b, costs))
        }
        List(costs)
    }

    private def compileBinaryConstraint1
        [InValue <: Value[InValue], InVariable <: Variable[InValue],
         OutValue <: Value[OutValue], OutVariable <: Variable[OutValue]]
        (createConstraint: (Id[yuck.core.Constraint], Option[Goal], InVariable, OutVariable) => yuck.core.Constraint,
         enforceDomain: Function2[InVariable, OutVariable, Iterable[BooleanVariable]],
         maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using
         inHelper: CompilationHelper[InValue, InVariable],
         outHelper: CompilationHelper[OutValue, OutVariable],
         outValueTraits: ValueTraits[OutValue]):
        Iterable[BooleanVariable] =
    {
        val Seq(a, b) = constraint.params: @unchecked
        val x = inHelper.compileExpr(a)
        val y = outHelper.compileExpr(b)
        def functionalCase = {
            cc.space.post(createConstraint(nextConstraintId(), maybeGoal, x, y))
            enforceDomain(x, y)
        }
        def generalCase = {
            val channel = outHelper.createChannel()
            cc.space.post(createConstraint(nextConstraintId(), maybeGoal, x, channel))
            val costs = createBoolChannel()
            cc.space.post(new Eq[OutValue](nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, b, functionalCase, generalCase)
    }

    private def compileBinaryConstraint2
        [InValue <: Value[InValue], InVariable <: Variable[InValue],
         OutValue <: Value[OutValue], OutVariable <: Variable[OutValue]]
        (createConstraint:
            (Id[yuck.core.Constraint], Option[Goal], immutable.IndexedSeq[InVariable], OutVariable) =>
                yuck.core.Constraint,
         enforceDomain: Function2[Seq[InVariable], OutVariable, Iterable[BooleanVariable]],
         maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using
         inHelper: CompilationHelper[InValue, InVariable],
         outHelper: CompilationHelper[OutValue, OutVariable],
         outValueTraits: ValueTraits[OutValue]):
        Iterable[BooleanVariable] =
    {
        val Seq(b, as) = constraint.params: @unchecked
        val xs = inHelper.compileArray(as)
        val y = outHelper.compileExpr(b)
        def functionalCase = {
            cc.space.post(createConstraint(nextConstraintId(), maybeGoal, xs, y))
            enforceDomain(xs, y)
        }
        def generalCase = {
            val channel = outHelper.createChannel()
            cc.space.post(createConstraint(nextConstraintId(), maybeGoal, xs, channel))
            val costs = createBoolChannel()
            cc.space.post(new Eq[OutValue](nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, b, functionalCase, generalCase)
    }

    private def compileTernaryConstraint
        [In1Value <: Value[In1Value], In1Variable <: Variable[In1Value],
         In2Value <: Value[In2Value], In2Variable <: Variable[In2Value],
         OutValue <: Value[OutValue], OutVariable <: Variable[OutValue]]
        (createConstraint:
            (Id[yuck.core.Constraint], Option[Goal], In1Variable, In2Variable, OutVariable) => yuck.core.Constraint,
         enforceDomain: Function3[In1Variable, In2Variable, OutVariable, Iterable[BooleanVariable]],
         maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using
         in1Helper: CompilationHelper[In1Value, In1Variable],
         in2Helper: CompilationHelper[In2Value, In2Variable],
         outHelper: CompilationHelper[OutValue, OutVariable],
         outValueTraits: ValueTraits[OutValue]):
        Iterable[BooleanVariable] =
    {
        val Seq(a, b, c) = constraint.params: @unchecked
        val x = in1Helper.compileExpr(a)
        val y = in2Helper.compileExpr(b)
        val z = outHelper.compileExpr(c)
        def functionalCase = {
            cc.space.post(createConstraint(nextConstraintId(), maybeGoal, x, y, z))
            enforceDomain(x, y, z)
        }
        def generalCase = {
            if (z.isInstanceOf[BooleanVariable] && z.domain == TrueDomain) {
                val costs = outHelper.createChannel()
                cc.space.post(createConstraint(nextConstraintId(), maybeGoal, x, y, costs))
                List(costs.asInstanceOf[BooleanVariable])
            } else {
                val channel = outHelper.createChannel()
                cc.space.post(createConstraint(nextConstraintId(), maybeGoal, x, y, channel))
                val costs = createBoolChannel()
                cc.space.post(new Eq[OutValue](nextConstraintId(), maybeGoal, channel, z, costs))
                List(costs)
            }
        }
        compileConstraint(constraint, c, functionalCase, generalCase)
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
        (using loadTraits: NumericalValueTraits[Load]):
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
        val maxLoad = items.map(_.weight).sum(loadTraits.numericalOperations)
        val trivialLoadDomain = loadTraits.createDomain(loadTraits.zero, maxLoad)
        def hasRedundantDomain(load: NumericalVariable[Load]) = trivialLoadDomain.isSubsetOf(load.domain)
        def functionalCase = {
            cc.space.post(new BinPacking[Load](nextConstraintId(), maybeGoal, items1, loads))
            loads.values.filterNot(hasRedundantDomain).flatMap(enforceDomain)
        }
        def generalCase = {
            val loads1: immutable.Map[Int, NumericalVariable[Load]] = {
                val definedVars = new mutable.HashSet[NumericalVariable[Load]]
                for ((bin, load) <- loads) yield
                    if (! definedVars.contains(load) && definesVar(constraint, bins, loads(bin))) {
                        definedVars += load
                        bin -> load
                    }
                    else bin -> createNonNegativeChannel[Load]()
            }
            cc.space.post(new BinPacking[Load](nextConstraintId(), maybeGoal, items1, loads1))
            val deltas: Iterable[BooleanVariable] =
                loads.flatMap((bin, load) =>
                    if (load == loads1(bin)) {
                        if hasRedundantDomain(load) then Nil else enforceDomain(load)
                    } else {
                        val delta = createBoolChannel()
                        cc.space.post(new Eq[Load](nextConstraintId(), maybeGoal, load, loads1(bin), delta))
                        List(delta)
                    }
                )
            deltas
        }
        compileConstraint(constraint, loads.values, functionalCase, generalCase)
    }

    private def compileDeliveryConstraint
        [Time <: NumericalValue[Time]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint)
        (using timeTraits: NumericalValueTraits[Time]):
        Iterable[BooleanVariable] =
    {
        val Seq(startNodes0, endNodes0, succ0, IntConst(offset), arrivalTimes0, serviceTimes0, travelTimes0,
                    BoolConst(withWaiting), totalTravelTime0) =
            constraint.params: @unchecked
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
                 .toVector
        require(travelTimes1.isEmpty || (travelTimes1.size == nodes.size && travelTimes1.forall(_.size == nodes.size)))
        val travelTimesAreSymmetric =
            travelTimes1.isEmpty ||
            Range(0, nodes.size).forall(
                i => Range(i + 1, nodes.size).forall(j => travelTimes1(i)(j) == travelTimes1(j)(i)))
        val travelTimes2 =
            if (! travelTimes1.isEmpty && travelTimesAreSymmetric)
                Vector.tabulate(nodes.size)(i => travelTimes1(i).drop(i))
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
                createNumChannel[Time]()
            } else {
                totalTravelTime1
            }
        }
        val costs = createBoolChannel()
        def functionalCase = {
            val delivery =
                new Delivery[Time](
                    WeakReference(cc.space), nextConstraintId(), maybeGoal,
                    startNodes, endNodes, succ, safeToInt(offset), arrivalTimes, serviceTimes, travelTimes,
                    withWaiting, totalTravelTime, costs)
            cc.space.post(delivery)
            List(costs)
        }
        def generalCase = {
            val nodes = IntegerRange(offset, offset + succ.size - 1)
            val arrivalTimes1: immutable.IndexedSeq[NumericalVariable[Time]] = {
                val definableVars = this.definedVars(constraint)
                val definedVars = new mutable.HashSet[NumericalVariable[Time]]
                nodes.valuesIterator.map(i =>
                    val arrivalTime = arrivalTimes(safeToInt(safeSub(i.value, offset)))
                    if (startNodes.contains(i)) {
                        arrivalTime
                    }
                    else if (definableVars.contains(arrivalTime) && ! definedVars.contains(arrivalTime) &&
                             isViableConstraint(succ, arrivalTime))
                    {
                        definedVars += arrivalTime
                        arrivalTime
                    }
                    else timeTraits.createVariable(cc.space, "", arrivalTime.domain)
                ).toVector
            }
            val totalTravelTime1: NumericalVariable[Time] =
                if (isViableConstraint(succ, totalTravelTime)) totalTravelTime
                else timeTraits.createVariable(cc.space, "", totalTravelTime.domain)
            cc.space.post(
                new Delivery[Time](
                    WeakReference(cc.space), nextConstraintId(), maybeGoal,
                    startNodes, endNodes, succ, safeToInt(offset), arrivalTimes1, serviceTimes, travelTimes,
                    withWaiting, totalTravelTime1, costs))
            val pairs = (arrivalTimes :+ totalTravelTime).zip(arrivalTimes1 :+ totalTravelTime1)
            val deltas: Iterable[BooleanVariable] =
                pairs.flatMap((x, x1) =>
                    if (x == x1) {
                        enforceDomain(x)
                    } else {
                        val delta = createBoolChannel()
                        cc.space.post(new Eq[Time](nextConstraintId(), maybeGoal, x, x1, delta))
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
        [V <: NumericalValue[V]]
        (maybeGoal: Option[Goal],
         as0: Expr, bs: Expr,
         maybeChannel: Option[NumericalVariable[V]] = None)
        (using valueTraits: NumericalValueTraits[V]):
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
                val channel = maybeChannel.getOrElse(createNumChannel[V]())
                cc.space.post(new Minus[V](nextConstraintId(), maybeGoal, x, y, channel))
                channel
            case List(AX(`minusOne`, x), AX(`one`, y)) =>
                val channel = maybeChannel.getOrElse(createNumChannel[V]())
                cc.space.post(new Minus[V](nextConstraintId(), maybeGoal, y, x, channel))
                channel
            case _ =>
                val channel = maybeChannel.getOrElse(createNumChannel[V]())
                if (axs.forall(_.a == one)) {
                    if (axs.size == 2) {
                        val List(AX(_, x), AX(_, y)) = axs
                        cc.space.post(new Plus(nextConstraintId(), maybeGoal, x, y, channel))
                    } else {
                        val xs = axs.iterator.map(_.x).toVector
                        cc.space.post(new Sum(nextConstraintId(), maybeGoal, xs , channel))
                    }
                } else {
                   cc.space.post(new LinearCombination(nextConstraintId(), maybeGoal, axs.toVector, channel))
                }
                channel
        }
    }

    private def compileLinearConstraint
        [V <: NumericalValue[V]]
        (maybeGoal: Option[Goal],
         as0: Expr, bs: Expr, relation: OrderingRelation, c: Expr,
         maybeCosts: Option[BooleanVariable] = None)
        (using valueTraits: NumericalValueTraits[V]):
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
        val y = createNumChannel[V]()
        val z = compileNumExpr[V](c)
        val costs = maybeCosts.getOrElse(createBoolChannel())
        if (axs.forall(_.a == valueTraits.one)) {
            cc.space.post(new SumConstraint(nextConstraintId(), maybeGoal, axs.map(_.x).toVector, y, relation, z, costs))
        } else {
            cc.space.post(new LinearConstraint(nextConstraintId(), maybeGoal, axs.toVector, y, relation, z, costs))
        }
        costs
    }

    private def compileCountConstraint
        [V <: Value[V]]
        (maybeGoal: Option[Goal],
         constraint: yuck.flatzinc.ast.Constraint,
         maybeCosts: Option[BooleanVariable] = None)
        (using valueTraits: ValueTraits[V]):
        Iterable[BooleanVariable] =
    {
        val Constraint(Count(relation, _), Seq(as, a, b), _) = constraint: @unchecked
        val y = compileExpr[V](a)
        // If xs(j) does not play a role (because its domain is disjoint from y.domain and hence
        // its values will never be counted), we omit xs(j) from the constraint and hence an
        // useless arc from the constraint network.
        val xs = compileArray[V](as).filter(_.domain.intersects(y.domain))
        val m = compileIntExpr(b)
        def functionalCase = {
            if (y.domain.isSingleton) {
                cc.space.post(new CountConst[V](nextConstraintId(), maybeGoal, xs, y.domain.singleValue, m))
                val minCount = xs.count(_.domain == y.domain)
                val maxCount = xs.count(_.domain.intersects(y.domain))
                if IntegerRange(minCount, maxCount).isSubsetOf(m.domain) then Nil else enforceIntDomain(m)
            } else {
                cc.space.post(new CountVar[V](nextConstraintId(), maybeGoal, xs, y, m))
                if IntegerRange(0, xs.size).isSubsetOf(m.domain) then Nil else enforceIntDomain(m)
            }
        }
        def generalCase = {
            val n = createNonNegativeIntChannel()
            if (y.domain.isSingleton) {
                cc.space.post(new CountConst[V](nextConstraintId(), maybeGoal, xs, y.domain.singleValue, n))
            } else {
                cc.space.post(new CountVar[V](nextConstraintId(), maybeGoal, xs, y, n))
            }
            val costs = maybeCosts.getOrElse(createBoolChannel())
            relation match {
                case "eq" => cc.space.post(new Eq[IntegerValue](nextConstraintId(), maybeGoal, m, n, costs))
                case "neq" => cc.space.post(new Ne[IntegerValue](nextConstraintId(), maybeGoal, m, n, costs))
                case "leq" => cc.space.post(new Le[IntegerValue](nextConstraintId(), maybeGoal, m, n, costs))
                case "lt" => cc.space.post(new Lt[IntegerValue](nextConstraintId(), maybeGoal, m, n, costs))
                case "geq" => cc.space.post(new Le[IntegerValue](nextConstraintId(), maybeGoal, n, m, costs))
                case "gt" => cc.space.post(new Lt[IntegerValue](nextConstraintId(), maybeGoal, n, m, costs))
             }
            List(costs)
        }
        if (relation == "eq" && maybeCosts.isEmpty) {
            compileConstraint(constraint, List(m), functionalCase, generalCase)
        } else {
            generalCase
        }
    }

    private def compileElementConstraint
        [V <: OrderedValue[V]]
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (using valueTraits: OrderedValueTraits[V]):
        Iterable[BooleanVariable] =
    {
        val Seq(IntConst(offset0), b, as, c) =
            if (constraint.params.size == 4) constraint.params: @unchecked
            else IntConst(1) +: constraint.params: @unchecked
        val i = compileIntExpr(b)
        val xs0 = compileArray[V](as)
        val y = compileOrdExpr[V](c)
        val indexRange0 = IntegerRange(offset0, offset0 + xs0.size - 1)
        if (! i.domain.intersects(indexRange0)) {
            throw new InconsistentConstraintException(constraint)
        }
        // If xs0(j) does not play a role (because j is not in i.domain), then there is no
        // need to monitor xs0(j) and we either drop it or replace it by some xs0(j') with j' in i.domain
        // to omit a useless arc from the constraint network.
        val indexRange = indexRange0.intersect(i.domain.hull)
        val offset = indexRange.lb.toInt
        val xs1 = xs0.drop(max(0, i.domain.lb.toInt - indexRange0.lb.toInt)).take(indexRange.size)
        val xs =
            (for (j <- indexRange.values) yield xs1((if (i.domain.contains(j)) j else i.domain.lb).toInt - offset))
                .toVector
        def post(y: OrderedVariable[V]): OrderedVariable[V] = {
            if (xs.forall(_.domain.isSingleton)) {
                val as = xs.map(_.domain.singleValue)
                cc.space.post(new ElementConst[V](nextConstraintId(), maybeGoal, as, i, y, offset))
            } else {
                cc.space.post(new ElementVar[V](nextConstraintId(), maybeGoal, xs, i, y, offset))
            }
            y
        }
        def functionalCase = {
            post(y)
            if xs.forall(_.domain.isSubsetOf(y.domain)) then Nil else enforceDomain(y)
        }
        def generalCase = {
            val channel = post(createOrdChannel[V]())
            val costs = createBoolChannel()
            cc.space.post(new Eq[V](nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, List(y), functionalCase, generalCase)
    }

    private def compileIfThenElseConstraint
        [V <: OrderedValue[V]]
        (maybeGoal: Option[Goal], constraint: yuck.flatzinc.ast.Constraint)
        (using valueTraits: OrderedValueTraits[V]):
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
            cc.space.post(new IfThenElse[V](nextConstraintId(), maybeGoal, cs, xs, y))
            y
        }
        def functionalCase = {
            post(y)
            if xs.forall(_.domain.isSubsetOf(y.domain)) then Nil else enforceDomain(y)
        }
        def generalCase = {
            val channel = post(createOrdChannel[V]())
            val costs = createBoolChannel()
            cc.space.post(new Eq[V](nextConstraintId(), maybeGoal, channel, y, costs))
            List(costs)
        }
        compileConstraint(constraint, List(y), functionalCase, generalCase)
    }

    private def compileReifiedConstraint
        (maybeGoal: Option[Goal], reifiedConstraint: yuck.flatzinc.ast.Constraint, maybeCosts: Option[BooleanVariable] = None):
        Iterable[BooleanVariable] =
    {
        val Constraint(Reif(name), params, annotations) = reifiedConstraint: @unchecked
        val constraint = Constraint(name, params.take(params.size - 1), annotations)
        val satisfied = compileBoolExpr(params.last)
        if (compilesToConst(params.last, True)) {
            if (cc.impliedConstraints.contains(constraint)) Nil
            else compileConstraint(maybeGoal, constraint)
        } else if (cc.impliedConstraints.contains(constraint)) {
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
                if (costs0.size != 1 || costs0.head != satisfied) {
                    postConjunction(maybeGoal, costs0, Some(satisfied))
                }
                enforceBoolDomain(satisfied)
            }
            def generalCase = {
                val costs0 = compileConstraint(maybeGoal, constraint, None).toVector
                val costs = createBoolChannel()
                if (costs0.size == 1) {
                    cc.space.post(new Eq(nextConstraintId(), maybeGoal, costs0.head, satisfied, costs))
                } else {
                    val costs1 = postConjunction(maybeGoal, costs0)
                    cc.space.post(new Eq(nextConstraintId(), maybeGoal, costs1, satisfied, costs))
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
        if (xs.size == 1 && maybeY.isEmpty) {
            xs(0)
        } else {
            val y = maybeY.getOrElse(createBoolChannel())
            if (xs.size == 2) {
               cc.space.post(new Or(nextConstraintId(), maybeGoal, xs(0), xs(1), y))
            } else {
                cc.space.post(new Disjunction(nextConstraintId(), maybeGoal, xs, y))
            }
            y
        }
    }

    private def postConjunction
        (maybeGoal: Option[Goal], xs0: Seq[BooleanVariable], maybeY: Option[BooleanVariable] = None):
        BooleanVariable =
    {
        val xs = xs0.iterator.filterNot(_.domain == TrueDomain).toSet.toVector
        if (xs.size == 1 && maybeY.isEmpty) {
            xs(0)
        } else {
            val y = maybeY.getOrElse(createBoolChannel())
            if (xs.size == 2) {
                cc.space.post(new And(nextConstraintId(), maybeGoal, xs(0), xs(1), y))
            } else {
                cc.space.post(new Conjunction(nextConstraintId(), maybeGoal, xs, y))
            }
            y
        }
    }

    private object DomainEnforcementGoal extends Goal("Domain enforcement")

    private def enforceDomain(x: AnyVariable): List[BooleanVariable] = {
        x match {
            case x: BooleanVariable => enforceBoolDomain(x)
            case x: IntegerVariable => enforceIntDomain(x)
            case x: IntegerSetVariable => enforceIntSetDomain(x)
        }
    }

    private def enforceBoolDomain(x: BooleanVariable): List[BooleanVariable] = {
        val dx = x.domain
        if (dx.isSingleton) {
            if (cc.space.isChannelVariable(x)) {
                if (dx.singleValue.truthValue) {
                    List(x)
                } else {
                    val costs = createBoolChannel()
                    cc.space.post(new Not(nextConstraintId(), Some(DomainEnforcementGoal), x, costs))
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
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                val costs = createBoolChannel()
                cc.space.post(new Contains(nextConstraintId(), Some(DomainEnforcementGoal), x, x.domain, costs))
                List(costs)
            } else if (dx.isSingleton) {
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
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                val costs = createBoolChannel()
                dx match {
                    case dx: IntegerPowersetDomain =>
                        cc.space.post(new Subset(nextConstraintId(), Some(DomainEnforcementGoal), x, dx.base, costs))
                    case dx: SingletonIntegerSetDomain =>
                        cc.space.post(new Eq(nextConstraintId(), Some(DomainEnforcementGoal), x, dx.base, costs))
                }
                List(costs)
            } else if (dx.isSingleton) {
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
                .map(cc.space.maybeDefiningConstraint(_))
                .filter(_.isDefined)
                .map(_.get)
                .filter(_.isInstanceOf[Contains])
                .filter(_.maybeGoal == Some(DomainEnforcementGoal))
                .toVector
        if (intDomainEnforcementConstraints.size > 32) {
            // When there are many integer channels, we enforce their domains using a single InDomain constraint.
            // This way we speed up neighbourhood generation and reduce the overhead of goal tracking.
            intDomainEnforcementConstraints.foreach(cc.space.retract)
            cc.costVars --= intDomainEnforcementConstraints.iterator.map(_.outVariables.head.asInstanceOf[BooleanVariable])
            val xs = intDomainEnforcementConstraints.map(_.inVariables.head.asInstanceOf[IntegerVariable])
            val costs = createBoolChannel()
            cc.space.post(new InDomain(nextConstraintId(), Some(DomainEnforcementGoal), xs, costs))
            cc.costVars += costs
        } else {
            // InDomain comes with more overhead than Contains and, when there are only a few integer channels,
            // a set of Contains constraints is faster than a single InDomain constraint.
        }
    }

}

/**
 * Companion object to ConstraintFactory.
 *
 * @author Michael Marte
 */
object ConstraintFactory {

    private val Count = "yuck_count_(.*)_(.*)".r
    private val IntLin = "int_lin_(.*)".r
    private val Reif = "(.*)_reif".r

    // In Yuck, True < False, but in FlatZinc, false < true.
    private val FlatZincBooleanValueOrdering = BooleanValueOrdering.reverse

}
