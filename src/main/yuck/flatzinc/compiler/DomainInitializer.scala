package yuck.flatzinc.compiler

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.ast.*
import yuck.util.logging.LogLevel.FineLogLevel

/**
 * Builds a map from parameter and variable declarations to domains.
 *
 * Assigns declarations to equivalence classes by considering optional assignments and equality constraints.
 *
 * @author Michael Marte
 */
final class DomainInitializer
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    // puts problem variables before variables introduced by mzn2fzn
    private object ProblemVariablesFirstOrdering extends Ordering[Expr] {
        override def compare(a: Expr, b: Expr) = {
            val aWasIntroduced = a.toString.startsWith("X_INTRODUCED")
            val bWasIntroduced = b.toString.startsWith("X_INTRODUCED")
            if (aWasIntroduced) {
                if (bWasIntroduced) a.toString.compare(b.toString) else 1
            } else {
                if (bWasIntroduced) -1 else a.toString.compare(b.toString)
            }
        }
    }

    override def run() = {
        cc.logger.withRootLogLevel(FineLogLevel) {
            cc.logger.withTimedLogScope("Initializing domains") {
                initializeDomains()
            }
            cc.logger.withTimedLogScope("Propagating assignments") {
                propagateAssignments()
            }
            cc.logger.withTimedLogScope("Propagating constraints") {
                propagateConstraints()
            }
            cc.logger.withTimedLogScope("Normalizing integer-set domains") {
                normalizeIntegerSetDomains()
            }
        }
    }

    private def initializeDomains(): Unit = {
        cc.ast.paramDecls.foreach(initializeDomains)
        cc.ast.varDecls.foreach(initializeDomains)
    }

    private def initializeDomains(decl: PlaceholderDecl): Unit = {
        decl.valueType match {
            case ArrayType(Some(IntRange(1, n)), baseType) =>
                val domain = createDomain(baseType)
                for (idx <- 1 to n.toInt) {
                    if (cc.sigint.isSet) {
                        throw new FlatZincCompilerInterruptedException
                    }
                    val a = ArrayAccess(decl.id, IntConst(idx))
                    cc.declaredVars += a
                    cc.domains += a -> domain
                }
            case _ =>
                if (cc.sigint.isSet) {
                    throw new FlatZincCompilerInterruptedException
                }
                val domain = createDomain(decl.valueType)
                val a = Term(decl.id, Nil)
                cc.declaredVars += a
                cc.domains += a -> domain
        }
    }

    private def propagateAssignments(): Unit = {
        cc.ast.paramDecls.foreach(propagateAssignments)
        cc.ast.varDecls.foreach(propagateAssignments)
    }

    private def propagateAssignments(decl: PlaceholderDecl): Unit = {
        decl.valueType match {
            case ArrayType(Some(IntRange(1, n)), _) =>
                decl.optionalValue match {
                    case Some(Term(rhsId, Nil)) =>
                        val lhsId = decl.id
                        for (idx <- 1 to n.toInt) {
                            val a = ArrayAccess(lhsId, IntConst(idx))
                            val b = ArrayAccess(rhsId, IntConst(idx))
                            propagateAssignment(a, b)
                        }
                    case Some(ArrayConst(elems)) =>
                        assert(elems.size == n)
                        for ((idx, b) <- (1 to n.toInt).zip(elems)) {
                            val a = ArrayAccess(decl.id, IntConst(idx))
                            propagateAssignment(a, b)
                        }
                    case _ =>
                }
            case _ =>
                if (decl.optionalValue.isDefined) {
                    val a = Term(decl.id, Nil)
                    val b = decl.optionalValue.get
                    propagateAssignment(a, b)
                }
        }
    }

    private def propagateAssignment(a: Expr, b: Expr): Unit = {
        if (cc.sigint.isSet) {
            throw new FlatZincCompilerInterruptedException
        }
        val exprType = getExprType(a)
        checkTypeCompatibility(exprType, getExprType(b))
        exprType match {
            case BoolType => propagateAssignment(a, b, boolDomain)
            case IntType(_) => propagateAssignment(a, b, intDomain)
            case IntSetType(_) => propagateAssignment(a, b, intSetDomain)
        }
    }

    private def propagateAssignment
        [V <: Value[V], D <: Domain[V]]
        (a: Expr, b: Expr, domainFactory: Expr => D)
        (using valueTraits: ValueTraits[V]):
        Unit =
    {
        if (b.isConst) {
            val da1 = domainFactory(a)
            val da2 = da1.intersect(domainFactory(b))
            if (da1 != da2) {
                if (cc.equalVars.contains(a)) {
                    cc.equalVars(a).foreach(b => reduceDomain(b, da2))
                } else {
                    reduceDomain(a, da2)
                }
            }
        } else {
            val da = domainFactory(a)
            val db = domainFactory(b)
            val d = da.intersect(db)
            propagateEquality(a, b, d)
        }
    }

    private def propagateConstraints(): Unit = {
        for (constraint <- cc.ast.constraints) {
            constraint match {
                case Constraint("bool_eq", _, _) =>
                    propagateEqualityConstraint(constraint, boolDomain)
                case Constraint("int_eq", _, _) =>
                    propagateEqualityConstraint(constraint, intDomain)
                case Constraint("set_eq", _, _) =>
                    propagateEqualityConstraint(constraint, intSetDomain)
                case Constraint("array_var_bool_element" | "yuck_array_bool_element", _, _) =>
                    propagateElementConstraint(constraint, boolDomain)
                case Constraint("array_var_int_element" | "yuck_array_int_element", _, _) =>
                    propagateElementConstraint(constraint, intDomain)
                case Constraint("array_var_set_element" | "yuck_array_set_element", _, _) =>
                    propagateElementConstraint(constraint, intSetDomain)
                case Constraint("yuck_int_domain", Seq(as, b), _) =>
                    val d = intDomain(b)
                    for (a <- getArrayElems(as)) {
                        reduceDomain(a, intDomain(a).intersect(d))
                    }
                    cc.impliedConstraints += constraint
                case _ =>
            }
        }
    }

    private def propagateEqualityConstraint
        [V <: Value[V]]
        (constraint: yuck.flatzinc.ast.Constraint, domain: Expr => Domain[V])
        (using valueTraits: ValueTraits[V]):
        Unit =
    {
        val Seq(a, b) = constraint.params: @unchecked
        if (! a.isConst) {
            val d = domain(a).intersect(domain(b))
            if (b.isConst) {
                propagateEquality(a, d)
            } else {
                propagateEquality(a, b, d)
            }
            cc.impliedConstraints += constraint
        }
    }

    private def propagateElementConstraint
        [V <: Value[V]]
        (constraint: yuck.flatzinc.ast.Constraint, domain: Expr => Domain[V])
        (using valueTraits: ValueTraits[V]):
        Unit =
    {
        val Seq(IntConst(offset), b, as, c) =
            if (constraint.params.size == 4) constraint.params: @unchecked
            else IntConst(1) +: constraint.params: @unchecked
        if (b.isConst && ! c.isConst) {
            val IntConst(i) = b: @unchecked
            val a = getArrayElems(as)(i.toInt - offset.toInt)
            if (! a.isConst) {
                propagateEquality(a, c, domain(a).intersect(domain(c)))
                cc.impliedConstraints += constraint
            }
        }
    }

    private def propagateEquality
        [V <: Value[V]]
        (a: Expr, b: Expr, d: Domain[V])
        (using valueTraits: ValueTraits[V]):
        Unit =
    {
        if (cc.sigint.isSet) {
            throw new FlatZincCompilerInterruptedException
        }
        cc.logger.log("%s = %s".format(a, b))
        val e = cc.equalVars.getOrElseUpdate(a, mutable.TreeSet.from(List(a))(ProblemVariablesFirstOrdering))
        val f = cc.equalVars.getOrElseUpdate(b, mutable.TreeSet.from(List(b))(ProblemVariablesFirstOrdering))
        if (cc.domains(a) != d) {
            e.foreach(a => reduceDomain(a, d))
        }
        if (cc.domains(b) != d) {
            f.foreach(a => reduceDomain(a, d))
        }
        if (e.size > f.size) {
            e ++= f
            f.foreach(a => cc.equalVars += a -> e)
        } else {
            f ++= e
            e.foreach(a => cc.equalVars += a -> f)
        }
    }

    private def propagateEquality
        [V <: Value[V]]
        (a: Expr, d: Domain[V])
        (using valueTraits: ValueTraits[V]):
        Unit =
    {
        if (cc.sigint.isSet) {
            throw new FlatZincCompilerInterruptedException
        }
        cc.logger.log("%s = %s".format(a, d))
        val e = cc.equalVars(a)
        if (cc.domains(a) != d) {
            e.foreach(a => reduceDomain(a, d))
        }
    }

    private def reduceDomain
        [V <: Value[V]]
        (a: Expr, d: Domain[V])
        (using valueTraits: ValueTraits[V]):
        Unit =
    {
        if (cc.sigint.isSet) {
            throw new FlatZincCompilerInterruptedException
        }
        if (d.isEmpty) {
            throw new yuck.flatzinc.compiler.DomainWipeOutException(a)
        }
        assert(d.isSubsetOf(valueTraits.safeDowncast(cc.domains(a))))
        cc.domains += a -> d
    }

    private def getExprType(a: Expr): Type = a match {
        case BoolConst(_) => BoolType
        case IntConst(_) => IntType(None)
        case IntSetConst(_) => IntSetType(None)
        case FloatConst(_) => FloatType(None)
        case Term(id, Nil) => cc.ast.paramDeclsByName.getOrElse(id, cc.ast.varDeclsByName(id)).valueType
        case ArrayAccess(id, _) => cc.ast.paramDeclsByName.getOrElse(id, cc.ast.varDeclsByName(id)).valueType.asInstanceOf[ArrayType].baseType
    }

    private def checkTypeCompatibility(t: Type, u: Type) = (t, u) match {
        case (BoolType, BoolType) =>
        case (IntType(_), IntType(_)) =>
        case (IntSetType(_), IntSetType(_)) =>
        case (FloatType(_), FloatType(_)) =>
        case _ => assert(false, "Types %s and %s are incompatible".format(t, u))
    }

    private def createDomain(varType: Type): AnyDomain = varType match {
        case BoolType => CompleteBooleanDomain
        case IntType(None) => CompleteIntegerRange
        case IntType(Some(IntRange(lb, ub))) => IntegerRange(lb, ub)
        case IntType(Some(IntSet(set))) => IntegerDomain(set)
        case IntSetType(None) => CompleteIntegerSetDomain
        case IntSetType(Some(IntRange(lb, ub))) =>
            val d0 = IntegerRange(lb, ub)
            val d = if (d0.isSubsetOf(SixtyFourBitSet.ValueRange)) SixtyFourBitSet(lb, ub) else d0
            new IntegerPowersetDomain(d)
        case IntSetType(Some(IntSet(set))) =>
            val d0 = IntegerDomain(set)
            val d = if (d0.isSubsetOf(SixtyFourBitSet.ValueRange)) SixtyFourBitSet(d0) else d0
            new IntegerPowersetDomain(d)
        case other => throw new UnsupportedFlatZincTypeException(other)
    }

    // To avoid frequent conversions from bit sets, this method replaces bit-set based integer-set domains
    // by domains based on ranges or range lists if there is at least one integer-set domain based on a
    // range or a range list.
    private def normalizeIntegerSetDomains(): Unit = {
        val keysToIntegerSetDomains =
            cc.domains.keysIterator.filter(cc.domains(_).isInstanceOf[IntegerSetDomain]).toList
        val nonBitSetDomainExists =
            keysToIntegerSetDomains.exists(cc.domains(_) match {
                case _: EmptyIntegerSetDomain.type => false
                case d: SingletonIntegerSetDomain => ! d.base.isInstanceOf[SixtyFourBitSet]
                case d: IntegerPowersetDomain => ! d.base.isInstanceOf[SixtyFourBitSet]
            })
        if (nonBitSetDomainExists) {
            for (expr <- keysToIntegerSetDomains) cc.domains(expr) match {
                case _: EmptyIntegerSetDomain.type =>
                case d: SingletonIntegerSetDomain =>
                    if (d.base.isInstanceOf[SixtyFourBitSet]) {
                         cc.domains.put(expr, new SingletonIntegerSetDomain(IntegerDomain(d.base.values)))
                    }
                case d: IntegerPowersetDomain =>
                    if (d.base.isInstanceOf[SixtyFourBitSet]) {
                         cc.domains.put(expr, new IntegerPowersetDomain(IntegerDomain(d.base.values)))
                    }
            }
        }
    }

}
