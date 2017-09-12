package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._
import yuck.flatzinc.ast._

/**
 * Builds a map from variable declarations to domains.
 *
 * Propagates optional assignments by enriching the AST with appropriate equality constraints.
 *
 * @author Michael Marte
 */
final class DomainInitializer
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    private val declaredVars = cc.declaredVars
    private val equalVars = cc.equalVars
    private val domains = cc.domains

    private def boolDomain(a: Expr): BooleanDomain =
        domains(a).asInstanceOf[BooleanDomain]
    private def intDomain(a: Expr): IntegerDomain =
        domains(a).asInstanceOf[IntegerDomain]
    private def intSetDomain(a: Expr): IntegerSetDomain =
        domains(a).asInstanceOf[IntegerSetDomain]

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

    override def run {
        initializeDomains
        propagateAssignments
    }

    private def initializeDomains {
        for (decl <- cc.ast.varDecls) {
            decl.varType match {
                case ArrayType(Some(IntRange(1, n)), baseType) =>
                    val domain = createDomain(baseType)
                    for (idx <- 1 to n) {
                        val a = ArrayAccess(decl.id, IntConst(idx))
                        declaredVars += a
                        domains += a -> domain
                        val set = new mutable.TreeSet[Expr]()(ProblemVariablesFirstOrdering)
                        set += a
                        equalVars += a -> set
                    }
                case _ => {
                    val domain = createDomain(decl.varType)
                    val a = Term(decl.id, Nil)
                    declaredVars += a
                    domains += a -> domain
                    val set = new mutable.TreeSet[Expr]()(ProblemVariablesFirstOrdering)
                    set += a
                    equalVars += a -> set
                }
            }
        }
    }

    private def propagateAssignments {
        for (decl <- cc.ast.varDecls) {
            decl.varType match {
                case ArrayType(Some(IntRange(1, n)), _) =>
                    decl.optionalValue match {
                        case Some(Term(rhsId, Nil)) =>
                            val lhsId = decl.id
                            for (idx <- 1 to n) {
                                val a = ArrayAccess(lhsId, IntConst(idx))
                                val b = ArrayAccess(rhsId, IntConst(idx))
                                propagateAssignment(a, b)
                            }
                        case Some(ArrayConst(elems)) =>
                            assert(elems.size == n)
                            for ((idx, b) <- (1 to n).zip(elems)) {
                                val a = ArrayAccess(decl.id, IntConst(idx))
                                propagateAssignment(a, b)
                            }
                        case _ =>
                    }
                case _ => {
                    if (decl.optionalValue.isDefined) {
                        val a = Term(decl.id, Nil)
                        val b = decl.optionalValue.get
                        propagateAssignment(a, b)
                    }
                }
            }
        }
    }

    private def propagateAssignment(a: Expr, b: Expr) {
        val exprType = getExprType(a)
        checkTypeCompatibility(exprType, getExprType(b))
        exprType match {
            case BoolType => b match {
                case BoolConst(value) =>
                    val da1 = boolDomain(a)
                    val da2 = da1.intersect(new BooleanDomain(! value, value))
                    if (da1 != da2) equalVars(a).foreach(b => reduceDomain(b, da2))
                case b =>
                    val da = boolDomain(a)
                    val db = boolDomain(b)
                    val d = da.intersect(db)
                    propagateEquality(a, b, d)
            }
            case IntType(_) => b match {
                case IntConst(value) =>
                    val da1 = intDomain(a)
                    val da2 = da1.intersect(createIntegerDomain(value, value))
                    if (da1 != da2) equalVars(a).foreach(b => reduceDomain(b, da2))
                case b =>
                    val da = intDomain(a)
                    val db = intDomain(b)
                    val d = da.intersect(db)
                    propagateEquality(a, b, d)
            }
            case IntSetType(_) =>
                val da = intSetDomain(a)
                val db = intSetDomain(b)
                val d = da.intersect(db)
                propagateEquality(a, b, d)
        }
    }

    def propagateEquality
        [Value <: AnyValue]
        (a: Expr, b: Expr, d: Domain[Value])
        (implicit valueTraits: AnyValueTraits[Value])
    {
        val e = equalVars(a)
        val f = equalVars(b)
        if (domains(a) != d) {
            e.foreach(a => reduceDomain(a, d))
        }
        if (domains(b) != d) {
            f.foreach(a => reduceDomain(a, d))
        }
        if (e.size > f.size) {
            e ++= f
            f.foreach(a => equalVars += a -> e)
        } else {
            f ++= e
            e.foreach(a => equalVars += a -> f)
        }
    }

    private def reduceDomain
        [Value <: AnyValue]
        (a: Expr, d: Domain[Value])
        (implicit valueTraits: AnyValueTraits[Value])
    {
        if (d.isEmpty) {
            throw new DomainWipeOutException(a)
        }
        assert(d.isSubsetOf(valueTraits.safeDowncast(domains(a))))
        domains += a -> d
    }

    private def getExprType(a: Expr): Type = a match {
        case BoolConst(_) => BoolType
        case IntConst(_) => IntType(None)
        case IntSetConst(_) => IntSetType(None)
        case FloatConst(_) => FloatType(None)
        case Term(id, Nil) => cc.ast.varDeclsByName(id).varType
        case ArrayAccess(id, _) => cc.ast.varDeclsByName(id).varType.asInstanceOf[ArrayType].baseType
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
        case IntType(Some(IntRange(lb, ub))) => createIntegerDomain(lb, ub)
        case IntType(Some(IntSet(set))) => createIntegerDomain(set)
        case IntSetType(None) => CompleteIntegerSetDomain
        case IntSetType(Some(IntRange(lb, ub))) => new IntegerPowersetDomain(createIntegerDomain(lb, ub))
        case IntSetType(Some(IntSet(set))) => new IntegerPowersetDomain(createIntegerDomain(set))
        case other => throw new UnsupportedFlatZincTypeException(other)
    }

}
