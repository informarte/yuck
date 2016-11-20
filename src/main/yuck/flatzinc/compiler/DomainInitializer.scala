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
                    decl.optionalValue match {
                        case Some(Term(rhsId, Nil)) =>
                            val lhsId = decl.id
                            for (idx <- 1 to n) {
                                val a = ArrayAccess(lhsId, IntConst(idx))
                                val b = ArrayAccess(rhsId, IntConst(idx))
                                propagateEquality(a, b)
                            }
                        case Some(ArrayConst(elems)) =>
                            assert(elems.size == n)
                            for ((idx, b) <- (1 to n).zip(elems)) {
                                val a = ArrayAccess(decl.id, IntConst(idx))
                                propagateEquality(a, b)
                            }
                        case _ =>
                    }
                case _ => {
                    val domain = createDomain(decl.varType)
                    val a = Term(decl.id, Nil)
                    declaredVars += a
                    domains += a -> domain
                    val set = new mutable.TreeSet[Expr]()(ProblemVariablesFirstOrdering)
                    set += a
                    equalVars += a -> set
                    if (decl.optionalValue.isDefined) {
                        val b = decl.optionalValue.get
                        propagateEquality(a, b)
                    }
                }
            }
        }
    }

    private def propagateEquality(a: Expr, b: Expr) {
        val exprType = getExprType(a)
        checkTypeCompatibility(exprType, getExprType(b))
        val id = exprType match {
            case BoolType => "bool_eq"
            case IntType(_) => "int_eq"
            case IntSetType(_) => "set_eq"
        }
        cc.ast = cc.ast.copy(constraints = Constraint(id, List(a, b), Nil) :: cc.ast.constraints)
    }

    private def getExprType(a: Expr): Type = a match {
        case BoolConst(_) => BoolType
        case IntConst(_) => IntType(None)
        case IntSetConst(_) => IntSetType(None)
        case Term(id, Nil) => cc.ast.varDeclsByName(id).varType
        case ArrayAccess(id, _) => cc.ast.varDeclsByName(id).varType.asInstanceOf[ArrayType].baseType
    }

    private def checkTypeCompatibility(t: Type, u: Type) = (t, u) match {
        case (BoolType, BoolType) =>
        case (IntType(_), IntType(_)) =>
        case (IntSetType(_), IntSetType(_)) =>
        case _ => assert(false, "Types %s and %s are incompatible".format(t, u))
    }

    private def createDomain(varType: Type): AnyDomain = varType match {
        case BoolType => UnboundedBooleanDomain
        case IntType(None) => UnboundedIntegerDomain
        case IntType(Some(IntRange(lb, ub))) => createIntegerDomain(lb, ub)
        case IntType(Some(IntSet(set))) => createIntegerDomain(set)
        case IntSetType(None) => UnboundedIntegerSetDomain
        case IntSetType(Some(IntRange(lb, ub))) => new IntegerPowersetDomain(createIntegerDomain(lb, ub))
        case IntSetType(Some(IntSet(set))) => new IntegerPowersetDomain(createIntegerDomain(set))
    }

}
