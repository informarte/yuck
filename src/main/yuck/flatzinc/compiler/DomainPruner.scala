package yuck.flatzinc.compiler

import scala.collection._
import scala.math._

import yuck.constraints._
import yuck.core._
import yuck.flatzinc.ast._

/**
 * Identifies equivalent variables and reduces variable domains by propagating constraints.
 *
 * @author Michael Marte
 */
final class DomainPruner
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    private val cfg = cc.cfg
    private val logger = cc.logger
    private val impliedConstraints = cc.impliedConstraints
    private val domains = cc.domains
    private val equalVars = cc.equalVars

    override def run {
        reduceDomains
    }

    private def reduceDomains {
        var reduction = false
        var pass = 0
        do {
            reduction = false
            pass += 1
            logger.withTimedLogScope("Pass %d".format(pass)) {
                for (constraint <- cc.ast.constraints if ! impliedConstraints.contains(constraint)) {
                    if (propagateConstraint(constraint)) {
                        reduction = true
                    }
                }
            }
        } while(reduction)
    }

    private def propagateConstraint(constraint: yuck.flatzinc.ast.Constraint): Boolean = {
        val effects = new mutable.HashMap[Expr, AnyDomain]
        def propagateEquality(a: Expr, b: Expr, d: AnyDomain) {
            val e = equalVars(a)
            val f = equalVars(b)
            if (domains(a) != d) {
                e.foreach(a => effects += a -> d)
            }
            if (domains(b) != d) {
                f.foreach(a => effects += a -> d)
            }
            if (e.size > f.size) {
                e ++= f
                f.foreach(a => equalVars += a -> e)
            } else {
                f ++= e
                e.foreach(a => equalVars += a -> f)
            }
            impliedConstraints += constraint
        }
        def propagateInverse(f: immutable.IndexedSeq[Expr], foff: Int, g: immutable.IndexedSeq[Expr], goff: Int) {
            for (i <- foff until foff + f.size) {
                val x = f(i - foff)
                val dx = x match {
                    case IntConst(a) => createIntegerDomain(a, a)
                    case _ => effects.get(x).getOrElse(domains(x)).asInstanceOf[IntegerDomain]
                }
                val di = createIntegerDomain(i, i)
                for (j <- createIntegerDomain(goff, goff + g.size - 1).subtract(dx).values.map(_.value)) {
                    val y = g(j - goff)
                    y match {
                        case IntConst(a) =>
                            assertConsistency(a != i, constraint)
                        case _ =>
                            val dy1 = effects.get(y).getOrElse(domains(y)).asInstanceOf[IntegerDomain]
                            val dy2 = dy1.subtract(di)
                            equalVars(y).foreach(x => effects += x -> dy2)
                    }
                }
            }
        }
        def propagateTranslation(translation: yuck.flatzinc.ast.Constraint) {
            propagateConstraint(translation)
            if (impliedConstraints.contains(translation)) {
                impliedConstraints += constraint
            }
        }
        def assertConsistency(consistent: Boolean, constraint: yuck.flatzinc.ast.Constraint) {
            if (! consistent) {
                throw new InconsistentConstraintException(constraint)
            }
        }
        val Reif = "(.*)_reif".r
        constraint match {
            case Constraint("bool2int", params, _) => params match {
                case List(BoolConst(a), IntConst(b)) =>
                    assertConsistency(if (a) b == 1 else b == 0, constraint)
                    impliedConstraints += constraint
                // In the remaining bool2int cases, we keep the constraint because
                // (1) removing it would break some downstream logic and
                // (2) there is no performance penalty for not doing it.
                case List(a, IntConst(b)) =>
                    assertConsistency(b == 0 || b == 1, constraint)
                    val da1 = domains(a).asInstanceOf[BooleanDomain]
                    val da2 = new BooleanDomain(b == 0 && da1.containsFalse, b == 1 && da1.containsTrue)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                case List(BoolConst(a), b) =>
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val db2 = IntegerDomainPruner.eq(db1, if (a) One else Zero)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                case List(a, b) =>
                    val da1 = domains(a).asInstanceOf[BooleanDomain]
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val da2 =
                        new BooleanDomain(db1.contains(Zero) && da1.containsFalse, db1.contains(One) && da1.containsTrue)
                    val db2 =
                        IntegerDomainPruner.eq(
                            db1,
                            new IntegerDomain(if (da1.containsFalse) Zero else One, if (da1.containsTrue) One else Zero))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
            }
            case Constraint("bool_eq", params, _) => params match {
                case List(BoolConst(a), BoolConst(b)) =>
                    assertConsistency(a == b, constraint)
                    impliedConstraints += constraint
                case List(a, BoolConst(value)) =>
                    val da1 = domains(a).asInstanceOf[BooleanDomain]
                    val da2 = new BooleanDomain(! value && da1.containsFalse, value && da1.containsTrue)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(BoolConst(value), b) =>
                    val db1 = domains(b).asInstanceOf[BooleanDomain]
                    val db2 = new BooleanDomain(! value && db1.containsFalse, value && db1.containsTrue)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(a, b) =>
                    val da = domains(a).asInstanceOf[BooleanDomain]
                    val db = domains(b).asInstanceOf[BooleanDomain]
                    val d = new BooleanDomain(da.containsFalse && db.containsFalse, da.containsTrue && db.containsTrue)
                    propagateEquality(a, b, d)
            }
            case Constraint("bool_le", params, _) => params match {
                case List(BoolConst(a), BoolConst(b)) =>
                    assertConsistency(a <= b, constraint)
                    impliedConstraints += constraint
                case List(BoolConst(false), _) =>
                    impliedConstraints += constraint
                case List(BoolConst(true), b) =>
                    val db1 = domains(b).asInstanceOf[BooleanDomain]
                    val db2 = new BooleanDomain(false, db1.containsTrue)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(_, BoolConst(true)) =>
                    impliedConstraints += constraint
                case List(a, BoolConst(false)) =>
                    val da1 = domains(a).asInstanceOf[BooleanDomain]
                    val da2 = new BooleanDomain(da1.containsFalse, false)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(a, b) =>
                    val da1 = domains(a).asInstanceOf[BooleanDomain]
                    val db1 = domains(b).asInstanceOf[BooleanDomain]
                    if (da1.isSingleton && da1.singleValue == True) {
                        val db2 = new BooleanDomain(false, db1.containsTrue)
                        if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                        impliedConstraints += constraint
                    }
                    if (db1.isSingleton && db1.singleValue == False) {
                        val da2 = new BooleanDomain(da1.containsFalse, false)
                        if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                        impliedConstraints += constraint
                    }
            }
            case Constraint("array_bool_and", params, _) => params match {
                case List(as, BoolConst(true)) =>
                    for (a <- getArrayElems(as)) {
                        if (a.isConst) {
                            assertConsistency(a == BoolConst(true), constraint)
                        } else {
                            val da1 = domains(a).asInstanceOf[BooleanDomain]
                            val da2 = new BooleanDomain(false, da1.containsTrue)
                            if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                        }
                    }
                    impliedConstraints += constraint
                case List(as0, BoolConst(false)) =>
                    val as = getArrayElems(as0)
                    if (as.exists(a => compilesToConst(a, False))) {
                        impliedConstraints += constraint
                    }
                    else assertConsistency(! as.forall(a => compilesToConst(a, True)), constraint)
                case List(as0, r) =>
                    val as = getArrayElems(as0)
                    if (as.exists(a => compilesToConst(a, False))) {
                        val dr1 = domains(r).asInstanceOf[BooleanDomain]
                        val dr2 = new BooleanDomain(dr1.containsFalse, false)
                        if (dr1 != dr2) equalVars(r).foreach(s => effects += s -> dr2)
                        impliedConstraints += constraint
                    }
                    else if (as.forall(a => compilesToConst(a, True))) {
                        val dr1 = domains(r).asInstanceOf[BooleanDomain]
                        val dr2 = new BooleanDomain(false, dr1.containsTrue)
                        if (dr1 != dr2) equalVars(r).foreach(s => effects += s -> dr2)
                        impliedConstraints += constraint
                    }
            }
            case Constraint("array_bool_or", params, _) => params match {
                case List(as, BoolConst(false)) =>
                    for (a <- getArrayElems(as)) {
                        if (a.isConst) {
                            assertConsistency(a == BoolConst(false), constraint)
                        } else {
                            val da1 = domains(a).asInstanceOf[BooleanDomain]
                            val da2 = new BooleanDomain(da1.containsFalse, false)
                            if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                        }
                    }
                    impliedConstraints += constraint
                case List(as0, BoolConst(true)) =>
                    val as = getArrayElems(as0)
                    if (as.exists(a => compilesToConst(a, True))) {
                        impliedConstraints += constraint
                    }
                    else assertConsistency(! as.forall(a => compilesToConst(a, False)), constraint)
                case List(as0, r) =>
                    val as = getArrayElems(as0)
                    if (as.exists(a => compilesToConst(a, True))) {
                        val dr1 = domains(r).asInstanceOf[BooleanDomain]
                        val dr2 = new BooleanDomain(false, dr1.containsTrue)
                        if (dr1 != dr2) equalVars(r).foreach(s => effects += s -> dr2)
                        impliedConstraints += constraint
                    }
                    else if (as.forall(a => compilesToConst(a, False))) {
                        val dr1 = domains(r).asInstanceOf[BooleanDomain]
                        val dr2 = new BooleanDomain(dr1.containsFalse, false)
                        if (dr1 != dr2) equalVars(r).foreach(s => effects += s -> dr2)
                        impliedConstraints += constraint
                    }
            }
            case Constraint("bool_clause", params, _) => params match {
                case List(ArrayConst(List(a)), ArrayConst(List(b))) =>
                    propagateTranslation(Constraint("bool_le", List(b, a), Nil))
                case List(as0, bs0) =>
                    // as0 are positive literals, bs0 are negative literals
                    val as = getArrayElems(as0).toList
                    val bs = getArrayElems(bs0).toList
                    (as, bs) match {
                        case (Nil, Nil) => throw new InconsistentConstraintException(constraint)
                        case (Nil, _) => propagateTranslation(Constraint("array_bool_and", List(bs0, BoolConst(false)), Nil))
                        case (_, Nil) => propagateTranslation(Constraint("array_bool_or", List(as0, BoolConst(true)), Nil))
                        case _ =>
                    }
            }
            case Constraint("int_eq", params, _) => params match {
                case List(IntConst(a), IntConst(b)) =>
                    assertConsistency(a == b, constraint)
                    impliedConstraints += constraint
                case List(a, IntConst(value)) =>
                    val da1 = domains(a).asInstanceOf[IntegerDomain]
                    val da2 = IntegerDomainPruner.eq(da1, IntegerValue.get(value))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(value), b) =>
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val db2 = IntegerDomainPruner.eq(db1, IntegerValue.get(value))
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(a, b) =>
                    val da = domains(a).asInstanceOf[IntegerDomain]
                    val db = domains(b).asInstanceOf[IntegerDomain]
                    val d = IntegerDomainPruner.eq(da, db)
                    propagateEquality(a, b, d)
            }
            case Constraint("int_ne", params, _) => params match {
                case List(IntConst(a), IntConst(b)) =>
                    assertConsistency(a != b, constraint)
                    impliedConstraints += constraint
                case List(a, IntConst(value)) =>
                    val da1 = domains(a).asInstanceOf[IntegerDomain]
                    val da2 = IntegerDomainPruner.ne(da1, IntegerValue.get(value))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(value), b) =>
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val db2 = IntegerDomainPruner.ne(db1, IntegerValue.get(value))
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
            }
            case Constraint("int_le", params, _) => params match {
                case List(IntConst(a), IntConst(b)) =>
                    assertConsistency(a <= b, constraint)
                    impliedConstraints += constraint
                case List(a, IntConst(ub)) =>
                    val da1 = domains(a).asInstanceOf[IntegerDomain]
                    val da2 = IntegerDomainPruner.le(da1, IntegerValue.get(ub))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(lb), b) =>
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val db2 = IntegerDomainPruner.le(IntegerValue.get(lb), db1)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                 case List(a, b) =>
                    val da1 = domains(a).asInstanceOf[IntegerDomain]
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val (da2, db2) = IntegerDomainPruner.le(da1, db1)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
            }
            case Constraint("int_lt", params, _) => params match {
                case List(IntConst(a), IntConst(b)) =>
                    assertConsistency(a < b, constraint)
                    impliedConstraints += constraint
                case List(a, IntConst(ub)) =>
                    val da1 = domains(a).asInstanceOf[IntegerDomain]
                    val da2 = IntegerDomainPruner.lt(da1, IntegerValue.get(ub))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(lb), b) =>
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val db2 = IntegerDomainPruner.lt(IntegerValue.get(lb), db1)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(a, b) =>
                    val da1 = domains(a).asInstanceOf[IntegerDomain]
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val (da2, db2) = IntegerDomainPruner.lt(da1, db1)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
            }
            case Constraint("int_lin_eq", params, annotations) => params match {
                case List(ArrayConst(List(IntConst(1))), ArrayConst(List(b)), c) =>
                    propagateTranslation(Constraint("int_eq", List(b, c), annotations))
                case List(ArrayConst(List(IntConst(-1))), ArrayConst(List(b)), IntConst(c)) =>
                    propagateTranslation(Constraint("int_eq", List(b, IntConst(-c)), annotations))
                case _ =>
            }
            case Constraint("int_lin_ne", _, annotations) => constraint.params match {
                case List(ArrayConst(List(IntConst(1))), ArrayConst(List(b)), c) =>
                    propagateTranslation(Constraint("int_ne", List(b, c), annotations))
                case List(ArrayConst(List(IntConst(-1))), ArrayConst(List(b)), IntConst(c)) =>
                    propagateTranslation(Constraint("int_ne", List(b, IntConst(-c)), annotations))
                case _ =>
            }
            case Constraint("int_lin_le", params, annotations) => params match {
                case List(ArrayConst(List(IntConst(a))), ArrayConst(List(b)), IntConst(c)) =>
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    if (a > 0) {
                        val ub = c / a + (if (c < 0 || c % a == 0) 0 else 1)
                        val db2 = IntegerDomainPruner.le(db1, IntegerValue.get(ub))
                        if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                        if (c % a == 0) {
                            impliedConstraints += constraint
                        }
                    } else if (a < 0) {
                        val lb = c / a - (if (c < 0 || c % a == 0) 0 else 1)
                        val db2 = IntegerDomainPruner.le(IntegerValue.get(lb), db1)
                        if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                        if (c % a == 0) {
                            impliedConstraints += constraint
                        }
                    } else {
                        assertConsistency(0 <= c, constraint)
                        impliedConstraints += constraint
                    }
                // precedence constraint
                // s[i] + d[i] <= s[j] compiles to int_lin_le([1, -1], [s[i], s[j]], -d[i])
                case List(ArrayConst(List(IntConst(1), IntConst(-1))), ArrayConst(List(a, b)), IntConst(c)) =>
                    val da1 = domains(a).asInstanceOf[IntegerDomain]
                    val db1 = domains(b).asInstanceOf[IntegerDomain]
                    val da2 = IntegerDomainPruner.le(da1, db1.ub - IntegerValue.get(-c))
                    val db2 = IntegerDomainPruner.le(da1.lb + IntegerValue.get(-c), db1)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                // precedence constraint
                case List(ArrayConst(List(IntConst(-1), IntConst(1))), ArrayConst(List(a, b)), c) =>
                    propagateConstraint(
                        Constraint(
                            "int_lin_le",
                            List(ArrayConst(List(IntConst(1), IntConst(-1))), ArrayConst(List(b, a)), c),
                            annotations))
                // int_lin_le normalization
                case List(a0, b0, c0) =>
                    val a1 = ArrayConst(getArrayElems(a0).toList)
                    if (a1.value.size < 3) {
                        val b1 = ArrayConst(getArrayElems(b0).toList)
                        val c1 = c0 match {
                            case IntConst(_) => c0
                            case _ if domains(c0).isSingleton =>
                                IntConst(domains(c0).asInstanceOf[IntegerDomain].singleValue.value)
                            case _ => c0
                        }
                        if (a0 != a1 || b0 != b1 || c0 != c1) {
                            propagateConstraint(Constraint("int_lin_le", List(a1, b1, c1), annotations))
                        }
                    }
            }
            case Constraint("set_eq", List(a, b), _) =>
                val da = domains(a).asInstanceOf[IntegerPowersetDomain]
                val db = domains(b).asInstanceOf[IntegerPowersetDomain]
                val d = new IntegerPowersetDomain(IntegerDomainPruner.eq(da.base, db.base))
                propagateEquality(a, b, d)
            case Constraint("set_in", List(a, IntSetConst(IntRange(lb, ub))), _) =>
                val da1 = domains(a).asInstanceOf[IntegerDomain]
                val db = createIntegerDomain(lb, ub)
                val da2 = IntegerDomainPruner.eq(da1, db)
                if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                impliedConstraints += constraint
            case Constraint("set_in", List(a, IntSetConst(IntSet(set))), _) =>
                val da1 = domains(a).asInstanceOf[IntegerDomain]
                val db = createIntegerDomain(set)
                val da2 = IntegerDomainPruner.eq(da1, db)
                if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                impliedConstraints += constraint
            case Constraint("all_different_int", _, _) =>
                val (as, xs) =
                     getArrayElems(constraint.params.head)
                    .partition(_ match {case IntConst(_) => true; case _ => false})
                val ys = xs.filter(domains(_).isSingleton)
                for (x <- xs) {
                    for (IntConst(a) <- as) {
                        val dx1 = effects.get(x).getOrElse(domains(x)).asInstanceOf[IntegerDomain]
                        val dx2 = IntegerDomainPruner.ne(dx1, IntegerValue.get(a))
                        if (dx1 != dx2) equalVars(x).foreach(y => effects += y -> dx2)
                    }
                    for (y <- ys if y != x) {
                        val dx1 = effects.get(x).getOrElse(domains(x)).asInstanceOf[IntegerDomain]
                        val dy = effects.get(y).getOrElse(domains(y)).asInstanceOf[IntegerDomain]
                        val dx2 = IntegerDomainPruner.ne(dx1, dy)
                        if (dx1 != dx2) equalVars(x).foreach(y => effects += y -> dx2)
                    }
                }
            case Constraint("yuck_table_int", List(xs0, t0), _) =>
                val xs = getArrayElems(xs0).toIndexedSeq
                val t = getArrayElems(t0).toIterator.map{case IntConst(a) => a}.toIndexedSeq
                val n = xs.size // number of columns
                require(t.size % n == 0)
                val m = t.size / n // number of rows
                def f(x: Expr, a: Int) = x match {
                    case IntConst(b) => b == a
                    case _ => domains(x).asInstanceOf[IntegerDomain].contains(IntegerValue.get(a))
                }
                val feasibleRows =
                     (0 until n)
                    .foldLeft(t.grouped(n)){case (rows, i) => rows.filter(row => f(xs(i), row(i)))}
                    .toIndexedSeq
                for (i <- 0 until n) {
                    val feasibleValues = feasibleRows.toIterator.map(row => row(i)).toSet
                    val x = xs(i)
                    x match {
                        case IntConst(a) =>
                            assertConsistency(feasibleValues.contains(a), constraint)
                        case _ =>
                            val dx1 = effects.get(x).getOrElse(domains(x)).asInstanceOf[IntegerDomain]
                            val dx2 = IntegerDomainPruner.eq(dx1, createIntegerDomain(feasibleValues))
                            if (dx1 != dx2) equalVars(x).foreach(y => effects += y -> dx2)
                    }
                }
            case Constraint("yuck_inverse", List(f, IntConst(foff), g, IntConst(goff)), _) =>
                propagateConstraint(Constraint("all_different_int", List(f), Nil))
                propagateConstraint(Constraint("all_different_int", List(g), Nil))
                val xs = getArrayElems(f).toIndexedSeq
                val ys = getArrayElems(g).toIndexedSeq
                assertConsistency(xs.size == ys.size, constraint)
                propagateInverse(xs, foff, ys, goff)
                propagateInverse(ys, goff, xs, foff)
           case Constraint(Reif(name), params, annotations) if (params.last.isConst || domains(params.last).isSingleton) =>
               if (params.last.isConst) {
                   logger.logg("Propagating %s".format(constraint))
               } else {
                   logger.logg(
                       "Propagating %s with %s = %s".format(
                           constraint, params.last, domains(params.last).asInstanceOf[BooleanDomain].singleValue))
               }
               val r = params.last match {
                   case BoolConst(b) => b
                   case _ => domains(params.last).asInstanceOf[BooleanDomain].singleValue.value
               }
               if (r) {
                   propagateTranslation(Constraint(name, params.take(params.size - 1), annotations))
               } else name match {
                   case "bool_eq" => propagateTranslation(Constraint("bool_ne", params.take(2), annotations))
                   case "bool_ne" => propagateTranslation(Constraint("bool_eq", params.take(2), annotations))
                   case "int_lt" => propagateTranslation(Constraint("int_le", List(params(1), params(0)), annotations))
                   case "int_le" => propagateTranslation(Constraint("int_lt", List(params(1), params(0)), annotations))
                   case "int_eq" => propagateTranslation(Constraint("int_ne", params.take(2), annotations))
                   case "int_ne" => propagateTranslation(Constraint("int_eq", params.take(2), annotations))
                   case "int_lin_eq" => propagateTranslation(Constraint("int_lin_ne", params.take(2), annotations))
                   case "int_lin_ne" => propagateTranslation(Constraint("int_lin_eq", params.take(2), annotations))
                   case _ =>
               }
           case _ =>
        }
        var reduction = false
        for ((decl, domain) <- effects) {
            if (domains(decl) != domain) {
                if (domain.isEmpty) {
                    throw new DomainWipeOutException(decl)
                }
                if (domain.isInstanceOf[BooleanDomain]) {
                    assert(
                        domain.asInstanceOf[BooleanDomain].isSubsetOf(domains(decl).asInstanceOf[BooleanDomain]),
                        "Domain %s of %s grew to %s".format(domains(decl), decl, domain))
                } else if (domain.isInstanceOf[IntegerDomain]) {
                    assert(
                        domain.asInstanceOf[IntegerDomain].isSubsetOf(domains(decl).asInstanceOf[IntegerDomain]),
                        "Domain %s of %s grew to %s".format(domains(decl), decl, domain))
                }
                logger.logg("%s reduced domain of %s from %s to %s".format(constraint, decl, domains(decl), domain))
                domains += decl -> domain
                reduction = true
            }
        }
        reduction
    }

}
