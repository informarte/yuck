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
    (cc: CompilationContext, randomGenerator: RandomGenerator, sigint: Sigint)
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
            if (sigint.isSet) {
                throw new FlatZincCompilerInterruptedException
            }
            val effects = new mutable.AnyRefMap[Expr, AnyDomain]
            reduction = false
            pass += 1
            logger.withTimedLogScope("Pass %d".format(pass)) {
                for (constraint <- cc.ast.constraints if ! impliedConstraints.contains(constraint)) {
                    propagateConstraint(constraint, effects)
                    reduction |= propagateEffects(constraint, effects)
                }
            }
        } while(reduction)
    }

    private def boolDomain(a: Expr): BooleanDomain = a match {
        case BoolConst(false) => FalseDomain
        case BoolConst(true) => TrueDomain
        case _ => domains(a).asInstanceOf[BooleanDomain]
    }
    private def intDomain(a: Expr): IntegerDomain = a match {
        case IntConst(a) => createIntegerDomain(a, a)
        case _ => domains(a).asInstanceOf[IntegerDomain]
    }
    private def intSetDomain(a: Expr): IntegerSetDomain = a match {
        case IntSetConst(IntRange(lb, ub)) => new SingletonIntegerSetDomain(createIntegerDomain(lb, ub))
        case IntSetConst(IntSet(set)) => new SingletonIntegerSetDomain(createIntegerDomain(set))
        case _ => domains(a).asInstanceOf[IntegerSetDomain]
    }

    private def normalizeBool(a: Expr): Expr =
        tryGetConst[BooleanValue](a).map(_.value).map(BoolConst).getOrElse(a)
    private def normalizeInt(a: Expr): Expr =
        tryGetConst[IntegerValue](a).map(_.value).map(IntConst).getOrElse(a)
    private def normalizeArray(a: Expr): Expr = a match {
        case ArrayConst(a) => ArrayConst(a)
        case a => ArrayConst(getArrayElems(a).toList)
    }

    private def union(domains: TraversableOnce[IntegerDomain]): IntegerDomain =
        domains.foldLeft(IntegerValueTraits.emptyDomain)((a, b) => a.union(b))

    private def propagateEffects(constraint: yuck.flatzinc.ast.Constraint, effects: mutable.Map[Expr, AnyDomain]): Boolean = {
        var reduction = false
        for ((decl, domain) <- effects) {
            if (domains(decl) != domain) {
                lazy val growMsg = "Domain %s of %s grew to %s".format(domains(decl), decl, domain)
                if (domain.isInstanceOf[BooleanDomain]) {
                    assert(domain.asInstanceOf[BooleanDomain].isSubsetOf(boolDomain(decl)), growMsg)
                } else if (domain.isInstanceOf[IntegerDomain]) {
                    assert(domain.asInstanceOf[IntegerDomain].isSubsetOf(intDomain(decl)), growMsg)
                } else if (domain.isInstanceOf[IntegerSetDomain]) {
                    assert(domain.asInstanceOf[IntegerSetDomain].isSubsetOf(intSetDomain(decl)), growMsg)
                }
                logger.logg("%s reduced domain of %s from %s to %s".format(constraint, decl, domains(decl), domain))
                if (domain.isEmpty) {
                    throw new DomainWipeOutException(decl)
                }
                domains += decl -> domain
                reduction = true
            }
        }
        reduction
    }

    private def propagateConstraint(constraint: yuck.flatzinc.ast.Constraint, effects: mutable.Map[Expr, AnyDomain]) {
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
                for (j <- createIntegerDomain(goff, goff + g.size - 1).diff(dx).values.map(_.value)) {
                    val y = g(j - goff)
                    y match {
                        case IntConst(a) =>
                            assertConsistency(a != i, constraint)
                        case _ =>
                            val dy1 = effects.get(y).getOrElse(domains(y)).asInstanceOf[IntegerDomain]
                            val dy2 = dy1.diff(di)
                            equalVars(y).foreach(x => effects += x -> dy2)
                    }
                }
            }
        }
        def propagateTranslation(translation: yuck.flatzinc.ast.Constraint) {
            propagateConstraint(translation, effects)
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
            case Constraint("bool2int", List(a, b), _) => List(normalizeBool(a), normalizeInt(b)) match {
                case List(BoolConst(a), IntConst(b)) =>
                    assertConsistency(if (a) b == 1 else b == 0, constraint)
                    impliedConstraints += constraint
                // In the remaining bool2int cases, we keep the constraint because
                // (1) removing it would break some downstream logic and
                // (2) there is no performance penalty for not doing it.
                case List(a, IntConst(b)) =>
                    assertConsistency(b == 0 || b == 1, constraint)
                    val da1 = boolDomain(a)
                    val da2 = new BooleanDomain(b == 0 && da1.containsFalse, b == 1 && da1.containsTrue)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                case List(BoolConst(a), b) =>
                    val db1 = intDomain(b)
                    val db2 = IntegerDomainPruner.eq(db1, if (a) One else Zero)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                case List(a, b) =>
                    val da1 = boolDomain(a)
                    val db1 = intDomain(b)
                    val da2 =
                        new BooleanDomain(db1.contains(Zero) && da1.containsFalse, db1.contains(One) && da1.containsTrue)
                    val db2 =
                        IntegerDomainPruner.eq(
                            db1,
                            IntegerDomain.createRange(if (da1.containsFalse) Zero else One, if (da1.containsTrue) One else Zero))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
            }
            case Constraint("bool_eq", params, _) => params.map(normalizeBool) match {
                case List(BoolConst(a), BoolConst(b)) =>
                    assertConsistency(a == b, constraint)
                    impliedConstraints += constraint
                case List(a, BoolConst(value)) =>
                    val da1 = boolDomain(a)
                    val da2 = new BooleanDomain(! value && da1.containsFalse, value && da1.containsTrue)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(BoolConst(value), b) =>
                    val db1 = boolDomain(b)
                    val db2 = new BooleanDomain(! value && db1.containsFalse, value && db1.containsTrue)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(a, b) =>
                    val da = boolDomain(a)
                    val db = boolDomain(b)
                    val d = new BooleanDomain(da.containsFalse && db.containsFalse, da.containsTrue && db.containsTrue)
                    propagateEquality(a, b, d)
            }
            case Constraint("bool_eq_reif", params, _) => params.map(normalizeBool) match {
                case List(a, b, BoolConst(true)) =>
                    propagateTranslation(constraint.copy(id = "bool_eq", params = List(a, b)))
                case List(a, b, BoolConst(false)) =>
                    propagateTranslation(constraint.copy(id = "bool_not", params = List(a, b)))
                case List(BoolConst(a), BoolConst(b), r) =>
                    val dr1 = boolDomain(r)
                    val dr2 = new BooleanDomain(dr1.containsFalse && a != b, dr1.containsTrue && a == b)
                    if (dr1 != dr2) equalVars(r).foreach(a => effects += a -> dr2)
                    impliedConstraints += constraint
                case _ =>
            }
            case Constraint("bool_xor", _, _) =>
                propagateTranslation(constraint.copy(id = "bool_not_reif"))
            case Constraint("bool_not", params, _) => params.map(normalizeBool) match {
                case List(BoolConst(a), BoolConst(b)) =>
                    assertConsistency(a != b, constraint)
                    impliedConstraints += constraint
                case List(a, BoolConst(value)) =>
                    val da1 = boolDomain(a)
                    val da2 = new BooleanDomain(value && da1.containsFalse, ! value && da1.containsTrue)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(BoolConst(value), b) =>
                    val db1 = boolDomain(b)
                    val db2 = new BooleanDomain(value && db1.containsFalse, ! value && db1.containsTrue)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(a, b) =>
            }
            case Constraint("bool_le", params, _) => params.map(normalizeBool) match {
                case List(BoolConst(a), BoolConst(b)) =>
                    assertConsistency(a <= b, constraint)
                    impliedConstraints += constraint
                case List(BoolConst(false), _) =>
                    impliedConstraints += constraint
                case List(BoolConst(true), b) =>
                    val db1 = boolDomain(b)
                    val db2 = new BooleanDomain(false, db1.containsTrue)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(_, BoolConst(true)) =>
                    impliedConstraints += constraint
                case List(a, BoolConst(false)) =>
                    val da1 = boolDomain(a)
                    val da2 = new BooleanDomain(da1.containsFalse, false)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(a, b) =>
                    val da1 = boolDomain(a)
                    val db1 = boolDomain(b)
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
            case Constraint("array_bool_and", List(as, b), _) => List(as, normalizeBool(b)) match {
                case List(as, BoolConst(true)) =>
                    for (a <- getArrayElems(as)) {
                        if (a.isConst) {
                            assertConsistency(a == BoolConst(true), constraint)
                        } else {
                            val da1 = boolDomain(a)
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
                        val dr1 = boolDomain(r)
                        val dr2 = new BooleanDomain(dr1.containsFalse, false)
                        if (dr1 != dr2) equalVars(r).foreach(s => effects += s -> dr2)
                        impliedConstraints += constraint
                    }
                    else if (as.forall(a => compilesToConst(a, True))) {
                        val dr1 = boolDomain(r)
                        val dr2 = new BooleanDomain(false, dr1.containsTrue)
                        if (dr1 != dr2) equalVars(r).foreach(s => effects += s -> dr2)
                        impliedConstraints += constraint
                    }
            }
            case Constraint("array_bool_or", List(as, b), _) => List(as, normalizeBool(b)) match {
                case List(as, BoolConst(false)) =>
                    for (a <- getArrayElems(as)) {
                        if (a.isConst) {
                            assertConsistency(a == BoolConst(false), constraint)
                        } else {
                            val da1 = boolDomain(a)
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
                        val dr1 = boolDomain(r)
                        val dr2 = new BooleanDomain(false, dr1.containsTrue)
                        if (dr1 != dr2) equalVars(r).foreach(s => effects += s -> dr2)
                        impliedConstraints += constraint
                    }
                    else if (as.forall(a => compilesToConst(a, False))) {
                        val dr1 = boolDomain(r)
                        val dr2 = new BooleanDomain(dr1.containsFalse, false)
                        if (dr1 != dr2) equalVars(r).foreach(s => effects += s -> dr2)
                        impliedConstraints += constraint
                    }
            }
            case Constraint("bool_clause", params, _) => params.map(normalizeArray) match {
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
            case Constraint("int_eq", params, _) => params.map(normalizeInt) match {
                case List(IntConst(a), IntConst(b)) =>
                    assertConsistency(a == b, constraint)
                    impliedConstraints += constraint
                case List(a, IntConst(value)) =>
                    val da1 = intDomain(a)
                    val da2 = IntegerDomainPruner.eq(da1, IntegerValue.get(value))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(value), b) =>
                    val db1 = intDomain(b)
                    val db2 = IntegerDomainPruner.eq(db1, IntegerValue.get(value))
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(a, b) =>
                    val da = intDomain(a)
                    val db = intDomain(b)
                    val d = IntegerDomainPruner.eq(da, db)
                    propagateEquality(a, b, d)
            }
            case Constraint("int_eq_reif", List(a, b, r), _) => List(normalizeInt(a), normalizeInt(b), normalizeBool(r)) match {
                case List(a, b, BoolConst(true)) =>
                    propagateTranslation(constraint.copy(id = "int_eq", params = List(a, b)))
                case List(a, b, BoolConst(false)) =>
                    propagateTranslation(constraint.copy(id = "int_ne", params = List(a, b)))
                case List(IntConst(a), IntConst(b), r) =>
                    val dr1 = boolDomain(r)
                    val dr2 = new BooleanDomain(dr1.containsFalse && a != b, dr1.containsTrue && a == b)
                    if (dr1 != dr2) equalVars(r).foreach(a => effects += a -> dr2)
                    impliedConstraints += constraint
                case _ =>
            }
            case Constraint("int_ne", params, _) => params.map(normalizeInt) match {
                case List(IntConst(a), IntConst(b)) =>
                    assertConsistency(a != b, constraint)
                    impliedConstraints += constraint
                case List(a, IntConst(value)) =>
                    val da1 = intDomain(a)
                    val da2 = IntegerDomainPruner.ne(da1, IntegerValue.get(value))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(value), b) =>
                    val db1 = intDomain(b)
                    val db2 = IntegerDomainPruner.ne(db1, IntegerValue.get(value))
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
            }
            case Constraint("int_ne_reif", List(a, b, r), _) => List(normalizeInt(a), normalizeInt(b), normalizeBool(r)) match {
                case List(a, b, BoolConst(true)) =>
                    propagateTranslation(constraint.copy(id = "int_ne", params = List(a, b)))
                case List(a, b, BoolConst(false)) =>
                    propagateTranslation(constraint.copy(id = "int_eq", params = List(a, b)))
                case List(IntConst(a), IntConst(b), r) =>
                    val dr1 = boolDomain(r)
                    val dr2 = new BooleanDomain(dr1.containsFalse && a == b, dr1.containsTrue && a != b)
                    if (dr1 != dr2) equalVars(r).foreach(a => effects += a -> dr2)
                    impliedConstraints += constraint
                case _ =>
            }
            case Constraint("int_le", params, _) => params.map(normalizeInt) match {
                case List(IntConst(a), IntConst(b)) =>
                    assertConsistency(a <= b, constraint)
                    impliedConstraints += constraint
                case List(a, IntConst(ub)) =>
                    val da1 = intDomain(a)
                    val da2 = IntegerDomainPruner.le(da1, IntegerValue.get(ub))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(lb), b) =>
                    val db1 = intDomain(b)
                    val db2 = IntegerDomainPruner.le(IntegerValue.get(lb), db1)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                 case List(a, b) =>
                    val da1 = intDomain(a)
                    val db1 = intDomain(b)
                    val (da2, db2) = IntegerDomainPruner.le(da1, db1)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
            }
            case Constraint("int_le_reif", List(a, b, r), _) => List(normalizeInt(a), normalizeInt(b), normalizeBool(r)) match {
                case List(a, b, BoolConst(true)) =>
                    propagateTranslation(constraint.copy(id = "int_le", params = List(a, b)))
                case List(a, b, BoolConst(false)) =>
                    propagateTranslation(constraint.copy(id = "int_lt", params = List(b, a)))
                case List(IntConst(a), IntConst(b), r) =>
                    val dr1 = boolDomain(r)
                    val dr2 = new BooleanDomain(dr1.containsFalse && a > b, dr1.containsTrue && a <= b)
                    if (dr1 != dr2) equalVars(r).foreach(a => effects += a -> dr2)
                    impliedConstraints += constraint
                case _ =>
            }
            case Constraint("int_lt", params, _) => params.map(normalizeInt) match {
                case List(IntConst(a), IntConst(b)) =>
                    assertConsistency(a < b, constraint)
                    impliedConstraints += constraint
                case List(a, IntConst(ub)) =>
                    val da1 = intDomain(a)
                    val da2 = IntegerDomainPruner.lt(da1, IntegerValue.get(ub))
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(lb), b) =>
                    val db1 = intDomain(b)
                    val db2 = IntegerDomainPruner.lt(IntegerValue.get(lb), db1)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
                    impliedConstraints += constraint
                case List(a, b) =>
                    val da1 = intDomain(a)
                    val db1 = intDomain(b)
                    val (da2, db2) = IntegerDomainPruner.lt(da1, db1)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    if (db1 != db2) equalVars(b).foreach(a => effects += a -> db2)
            }
            case Constraint("int_lt_reif", List(a, b, r), _) => List(normalizeInt(a), normalizeInt(b), normalizeBool(r)) match {
                case List(a, b, BoolConst(true)) =>
                    propagateTranslation(constraint.copy(id = "int_lt", params = List(a, b)))
                case List(a, b, BoolConst(false)) =>
                    propagateTranslation(constraint.copy(id = "int_le", params = List(b, a)))
                case List(IntConst(a), IntConst(b), r) =>
                    val dr1 = boolDomain(r)
                    val dr2 = new BooleanDomain(dr1.containsFalse && a >= b, dr1.containsTrue && a < b)
                    if (dr1 != dr2) equalVars(r).foreach(a => effects += a -> dr2)
                    impliedConstraints += constraint
                case _ =>
            }
            case Constraint("int_times", List(a, b, c), _) =>
                // see K. R. Apt, Principles of Constraint Programming, p. 217
                val da1 = intDomain(a)
                val db1 = intDomain(b)
                val dc1 = intDomain(c)
                // MULTIPLICATION 1
                val dc2 =
                    if (da1.isFinite && db1.isFinite) {
                        dc1.intersect(da1.hull.mult(db1.hull))
                    } else {
                        dc1
                    }
                // MULTIPLICATION 2
                val da2 =
                    if (db1.isFinite && dc1.isFinite) {
                        da1.intersect(dc1.hull.div(db1.hull))
                    } else {
                        da1
                    }
                // MULTIPLICATION 3
                val db2 =
                    if (da1.isFinite && dc1.isFinite) {
                        db1.intersect(dc1.hull.div(da1.hull))
                    } else {
                        db1
                    }
                // publish results
                for ((a, da1, da2) <- List((a, da1, da2), (b, db1, db2), (c, dc1, dc2))) {
                    assertConsistency(! a.isConst || ! da2.isEmpty, constraint)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                }
            case Constraint("int_lin_eq", List(as, bs, c), annotations) => List(normalizeArray(as), normalizeArray(bs), c) match {
                case List(ArrayConst(List(IntConst(1))), ArrayConst(List(b)), c) =>
                    propagateTranslation(Constraint("int_eq", List(b, c), annotations))
                case List(ArrayConst(List(IntConst(-1))), ArrayConst(List(b)), IntConst(c)) =>
                    propagateTranslation(Constraint("int_eq", List(b, IntConst(-c)), annotations))
                case List(ArrayConst(as), bs, IntConst(c)) =>
                    propagateConstraint(constraint.copy(id = "int_lin_le"), effects)
                    propagateConstraint(Constraint("int_lin_le", List(ArrayConst(as.map{case IntConst(a) => IntConst(-a)}), bs, IntConst(-c)), annotations), effects)
            }
            case Constraint("int_lin_ne", List(as, bs, c), annotations) => List(normalizeArray(as), normalizeArray(bs), c) match {
                case List(ArrayConst(List(IntConst(1))), ArrayConst(List(b)), c) =>
                    propagateTranslation(Constraint("int_ne", List(b, c), annotations))
                case List(ArrayConst(List(IntConst(-1))), ArrayConst(List(b)), IntConst(c)) =>
                    propagateTranslation(Constraint("int_ne", List(b, IntConst(-c)), annotations))
                case _ =>
            }
            case Constraint("int_lin_le", List(as, bs, c), annotations) => List(normalizeArray(as), normalizeArray(bs), c) match {
                case List(ArrayConst(List(IntConst(a))), ArrayConst(List(b)), IntConst(c)) =>
                    val db1 = intDomain(b)
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
                case List(ArrayConst(as), ArrayConst(bs), IntConst(c)) =>
                    // see K. R. Apt, Principles of Constraint Programming, p. 194
                    val (pos, neg) = as.zip(bs).toIndexedSeq.map{case (IntConst(a), b) => (a, b)}.partition{case (a, _) => a >= 0}
                    if (pos.forall{case (_, b) => intDomain(b).maybeLb.isDefined} &&
                        neg.forall{case (_, b) => intDomain(b).maybeUb.isDefined})
                    {
                        if (! pos.isEmpty) {
                            lazy val negTerm = neg.toIterator.map{case (a, b) => a * intDomain(b).ub.value}.sum
                            for (j <- 0 until pos.size) {
                                val posTerm = (0 until pos.size).toIterator.filter(_ != j).map(pos).map{case (a, b) => a * intDomain(b).lb.value}.sum
                                val (a, b) = pos(j)
                                val lb = (c - posTerm - negTerm).toDouble / a
                                val db1 = intDomain(b)
                                val db2 = IntegerDomainPruner.le(db1, IntegerValue.get(scala.math.floor(lb).toInt))
                                if (db1 != db2) equalVars(b).foreach(b => effects += b -> db2)
                            }
                        }
                        if (! neg.isEmpty) {
                            lazy val posTerm = pos.toIterator.map{case (a, b) => a * intDomain(b).lb.value}.sum
                            for (j <- 0 until neg.size) {
                                val negTerm = (0 until neg.size).toIterator.filter(_ != j).map(neg).map{case (a, b) => a * intDomain(b).ub.value}.sum
                                val (a, b) = neg(j)
                                val ub = (-c + posTerm + negTerm).toDouble / -a
                                val db1 = intDomain(b)
                                val db2 = IntegerDomainPruner.le(IntegerValue.get(scala.math.ceil(ub).toInt), db1)
                                if (db1 != db2) equalVars(b).foreach(b => effects += b -> db2)
                            }
                        }
                    }
                case _ =>
            }
            case Constraint("int_min", List(a, b, c), annotations) =>
                propagateTranslation(Constraint("array_int_minimum", List(c, ArrayConst(List(a, b))), annotations))
            case Constraint("int_max", List(a, b, c), annotations) =>
                propagateTranslation(Constraint("array_int_maximum", List(c, ArrayConst(List(a, b))), annotations))
            case Constraint("set_eq", List(a, b), _) =>
                val da = intSetDomain(a)
                val db = intSetDomain(b)
                val d = da.intersect(db)
                propagateEquality(a, b, d)
            case Constraint("set_in", List(a, b), _) => List(normalizeInt(a), b) match {
                case List(IntConst(a), IntSetConst(IntRange(lb, ub))) =>
                    assertConsistency(a >= lb && a <= ub, constraint)
                    impliedConstraints += constraint
                case List(a, IntSetConst(IntRange(lb, ub))) =>
                    val da1 = intDomain(a)
                    val db = createIntegerDomain(lb, ub)
                    val da2 = IntegerDomainPruner.eq(da1, db)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(a), IntSetConst(IntSet(set))) =>
                    assertConsistency(set.contains(a), constraint)
                    impliedConstraints += constraint
                case List(a, IntSetConst(IntSet(set))) =>
                    val da1 = intDomain(a)
                    val db = createIntegerDomain(set)
                    val da2 = IntegerDomainPruner.eq(da1, db)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case _ =>
            }
            case Constraint("set_in_reif", List(a, b, r), _) => List(normalizeInt(a), b, normalizeBool(r)) match {
                case List(a, b, BoolConst(true)) =>
                    propagateTranslation(constraint.copy(id = "set_in", params = List(a, b)))
                case List(IntConst(a), IntSetConst(IntRange(lb, ub)), BoolConst(false)) =>
                    assertConsistency(a < lb || a > ub, constraint)
                    impliedConstraints += constraint
                case List(a, IntSetConst(IntRange(lb, ub)), BoolConst(false)) =>
                    val da1 = intDomain(a)
                    val db = createIntegerDomain(lb, ub)
                    val da2 = da1.diff(db)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case List(IntConst(a), IntSetConst(IntSet(set)), BoolConst(false)) =>
                    assertConsistency(! set.contains(a), constraint)
                    impliedConstraints += constraint
                case List(a, IntSetConst(IntSet(set)), BoolConst(false)) =>
                    val da1 = intDomain(a)
                    val db = createIntegerDomain(set)
                    val da2 = da1.diff(db)
                    if (da1 != da2) equalVars(a).foreach(b => effects += b -> da2)
                    impliedConstraints += constraint
                case _ =>
            }
            case Constraint("array_int_element", _, _) =>
                propagateTranslation(constraint.copy(id = "array_var_int_element"))
            case Constraint("array_var_int_element", List(b, as, c), _) =>
                val das = getArrayElems(as).toIterator.map(intDomain).toIndexedSeq
                val db1 = intDomain(b)
                val db2 = IntegerDomainPruner.eq(db1, createIntegerDomain(1, das.size))
                val dc1 = intDomain(c)
                val dc2 = IntegerDomainPruner.eq(dc1, union(db2.values.toIterator.map(i => das(i.value - 1))))
                val db3 = IntegerDomain.createDomain(db2.values.toIterator.filter(i => das(i.value - 1).intersects(dc2)).toSet)
                assertConsistency(! b.isConst || ! db3.isEmpty, constraint)
                if (db1 != db3) equalVars(b).foreach(x => effects += x -> db3)
                assertConsistency(! c.isConst || ! dc2.isEmpty, constraint)
                if (dc1 != dc2) equalVars(c).foreach(x => effects += x -> dc2)
            case Constraint("array_int_minimum", List(b, as), _) =>
                val db1 = intDomain(b)
                for (a <- getArrayElems(as) if ! a.isConst) {
                    val da1 = intDomain(a)
                    val (_, da2) = IntegerDomainPruner.le(db1, da1)
                    if (da1 != da2) equalVars(a).foreach(x => effects += x -> da2)
                }
                val (_, db2) = IntegerDomainPruner.le(union(getArrayElems(as).toIterator.map(intDomain)), db1)
                assertConsistency(! b.isConst || ! db2.isEmpty, constraint)
                if (db1 != db2) equalVars(b).foreach(x => effects += x -> db2)
            case Constraint("array_int_maximum", List(b, as), _) =>
                val db1 = intDomain(b)
                for (a <- getArrayElems(as) if ! a.isConst) {
                    val da1 = intDomain(a)
                    val (da2, _) = IntegerDomainPruner.le(da1, db1)
                    if (da1 != da2) equalVars(a).foreach(x => effects += x -> da2)
                }
                val (db2, _) = IntegerDomainPruner.le(db1, union(getArrayElems(as).toIterator.map(intDomain)))
                assertConsistency(! b.isConst || ! db2.isEmpty, constraint)
                if (db1 != db2) equalVars(b).foreach(x => effects += x -> db2)
            case Constraint("all_different_int", _, _) =>
                val (as, xs) =
                     getArrayElems(constraint.params.head)
                    .map(normalizeInt)
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
                        val dx2 = dx1.diff(dy)
                        if (dx1 != dx2) equalVars(x).foreach(y => effects += y -> dx2)
                    }
                }
            case Constraint("yuck_table_int", List(xs0, t0), _) =>
                val xs = getArrayElems(xs0).toIterator.map(normalizeInt).toIndexedSeq
                val t = getArrayElems(t0).toIterator.map{case IntConst(a) => a}.toIndexedSeq
                val n = xs.size // number of columns
                require(t.size % n == 0)
                val m = t.size / n // number of rows
                def f(x: Expr, a: Int) = x match {
                    case IntConst(b) => b == a
                    case _ => intDomain(x).contains(IntegerValue.get(a))
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
                propagateConstraint(Constraint("all_different_int", List(f), Nil), effects)
                propagateConstraint(Constraint("all_different_int", List(g), Nil), effects)
                val xs = getArrayElems(f).toIndexedSeq
                val ys = getArrayElems(g).toIndexedSeq
                assertConsistency(xs.size == ys.size, constraint)
                propagateInverse(xs, foff, ys, goff)
                propagateInverse(ys, goff, xs, foff)
           case Constraint(Reif(name), params, annotations) if compilesToConst(params.last) =>
               if (params.last.isConst) {
                   logger.logg("Propagating %s".format(constraint))
               } else {
                   logger.logg(
                       "Propagating %s with %s = %s".format(
                           constraint, params.last, boolDomain(params.last).singleValue))
               }
               val r = params.last match {
                   case BoolConst(b) => b
                   case _ => boolDomain(params.last).singleValue.value
               }
               if (r) {
                   propagateTranslation(Constraint(name, params.take(params.size - 1), annotations))
               } else name match {
                   case "bool_eq" => propagateTranslation(Constraint("bool_not", params.take(2), annotations))
                   case "bool_not" => propagateTranslation(Constraint("bool_eq", params.take(2), annotations))
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
    }

}
