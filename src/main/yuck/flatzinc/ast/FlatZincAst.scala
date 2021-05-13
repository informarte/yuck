package yuck.flatzinc.ast

import scala.collection._

trait ValueSet[Value]
final case class IntRange(val lb: Int, val ub: Int) extends ValueSet[Int] {
    override def toString = "%s..%s".format(lb, ub)
}
final case class IntSet(val value: Set[Int]) extends ValueSet[Int] {
    override def toString =
        "{%s%s}".format(
            value.toBuffer.sorted.iterator.take(10).map(_.toString).mkString(", "),
            if (value.size > 10) ", ..." else "")
}
// The representation of floats is left to the implementation by the Zinc specification.
final case class FloatRange(val lb: Double, val ub: Double) extends ValueSet[Double] {
    override def toString = "%s..%s".format(lb, ub)
}

trait Expr {
    val isConst = false
}
trait ConstExpr extends Expr {
    override val isConst = true
}
final case class BoolConst(val value: Boolean) extends ConstExpr {
    override def toString = value.toString
}
final case class IntConst(val value: Int) extends ConstExpr {
    override def toString = value.toString
}
final case class FloatConst(val value: Double) extends ConstExpr {
    override def toString = value.toString
}
final case class IntSetConst(value: ValueSet[Int]) extends ConstExpr
final case class ArrayConst(val value: List[Expr]) extends ConstExpr {
    override def toString = "[%s]".format(value.iterator.map(_.toString).mkString(", "))
}
final case class ArrayAccess(val id: String, val idx: Expr) extends Expr {
    override def toString = "%s[%s]".format(id, idx)
}
// In expressions, parameters and variables are represented as 0-ary terms.
final case class Term(val id: String, val params: List[Expr]) extends Expr {
    override def toString =
        if (params.isEmpty) id else "%s(%s)".format(id, params.iterator.map(_.toString).mkString(", "))
}

trait Type {
    val isArrayType = false
}
trait BaseType extends Type
final case object BoolType extends BaseType {
    override def toString = "bool"
}
final case class IntType(val optionalDomain: Option[ValueSet[Int]]) extends BaseType {
    override def toString = optionalDomain match {
        case Some(domain) => domain.toString
        case None => "int"
    }
}
final case class FloatType(val optionalDomain: Option[ValueSet[Double]]) extends BaseType {
    override def toString = optionalDomain match {
        case Some(domain) => domain.toString
        case None => "float"
    }
}
final case class IntSetType(val optionalDomain: Option[ValueSet[Int]]) extends BaseType {
    override def toString = optionalDomain match {
        case Some(domain) => "set of %s".format(domain.toString)
        case None => "set of int"
    }
}
final case class ArrayType(val optionalIndexSet: Option[ValueSet[Int]], baseType: BaseType) extends Type {
    override val isArrayType = true
    override def toString = optionalIndexSet match {
        case Some(indexSet) => "array [%s] of %s".format(indexSet, baseType)
        case None => "array of %s".format(baseType)
    }
}

final case class PredParam(val id: String, val paramType: Type) {}
final case class PredDecl(val id: String, val params: List[PredParam]) {}
final case class ParamDecl(val id: String, val paramType: Type, val value: Expr)
final case class VarDecl(val id: String, val varType: Type, val optionalValue: Option[Expr], annotations: List[Annotation]) {
    def isIntroduced: Boolean = annotations.contains(Annotation(Term("var_is_introduced", Nil)))
    def isDefined: Boolean = annotations.contains(Annotation(Term("is_defined_var", Nil)))
}

final case class Constraint(val id: String, params: List[Expr], annotations: List[Annotation]) {
    override def toString = {
        val tmp =
            if (params.isEmpty) id
            else "%s(%s)".format(id, params.iterator.map(_.toString).mkString(", "))
        if (annotations.isEmpty) tmp
        else "%s :: %s".format(tmp, annotations.iterator.map(_.toString).mkString(", "))
    }
}

final case class Annotation(val term: Term) {
    override def toString = term.toString
}

abstract class SolveGoal(val annotations: List[Annotation])
final case class Satisfy(override val annotations: List[Annotation]) extends SolveGoal(annotations)
final case class Minimize(val objective: Expr, override val annotations: List[Annotation]) extends SolveGoal(annotations)
final case class Maximize(val objective: Expr, override val annotations: List[Annotation]) extends SolveGoal(annotations)

final case class FlatZincAst(
    val predDecls: List[PredDecl],
    val predDeclsByName: Map[String, PredDecl],
    val paramDecls: List[ParamDecl],
    val paramDeclsByName: Map[String, ParamDecl],
    val varDecls: List[VarDecl],
    val varDeclsByName: Map[String, VarDecl],
    val constraints: List[Constraint],
    val solveGoal: SolveGoal)
{

    // Make the Intellij IDEA debugger usable
    override def toString = "AST"

    /** Returns the elements of the given array. */
    final def getArrayElems(expr: Expr): immutable.Iterable[Expr] = expr match {
        case ArrayConst(elems) =>
            elems
        case Term(id, Nil) if varDeclsByName.contains(id) =>
            val decl = varDeclsByName(id)
            if (decl.optionalValue.isDefined) {
                val ArrayConst(elems) = decl.optionalValue.get
                elems
            } else {
                val ArrayType(Some(IntRange(n, m)), _) = decl.varType
                (n to m).map(i => ArrayAccess(id, IntConst(i)))
            }
        case Term(id, Nil) if paramDeclsByName.contains(id) =>
            val ArrayConst(elems) = paramDeclsByName(id).value
            elems
        case _ =>
            throw new IllegalArgumentException("%s is not an array".format(expr))
    }

    /** Returns the variables involved in the given expression. */
    def involvedVariables(expr: Expr): immutable.Set[Expr] = expr match {
        case BoolConst(_) | IntConst(_) | FloatConst(_) | IntSetConst(_) =>
            immutable.Set()
        case ArrayConst(elems) =>
            elems.iterator.flatMap(involvedVariables).toSet
        case ArrayAccess(id, idx) if varDeclsByName.contains(id) =>
            val decl = varDeclsByName(id)
            if (decl.optionalValue.isDefined) {
                findIndex(idx) match {
                    case Some(i) =>
                        val ArrayType(Some(IntRange(n, m)), _) = decl.varType
                        val ArrayConst(elems) = decl.optionalValue.get
                        involvedVariables(elems(i - n))
                    case None =>
                        immutable.Set(expr)
                }
            } else {
                immutable.Set(expr)
            }
        case ArrayAccess(id, _) if paramDeclsByName.contains(id) =>
            immutable.Set()
        case Term(id, Nil) if varDeclsByName.contains(id) =>
            if (varDeclsByName(id).varType.isArrayType) getArrayElems(expr).iterator.flatMap(involvedVariables).toSet
            else immutable.Set(expr)
        case Term(id, Nil) if paramDeclsByName.contains(id) =>
            immutable.Set()
    }

    private def findIndex(expr: Expr): Option[Int] = expr match {
        case IntConst(i) =>
            Some(i)
        case ArrayAccess(id, idx) if varDeclsByName.contains(id) =>
            val decl = varDeclsByName(id)
            if (decl.optionalValue.isDefined) {
                findIndex(idx) match {
                    case Some(i) =>
                        val ArrayType(Some(IntRange(n, _)), _) = decl.varType
                        val ArrayConst(elems) = decl.optionalValue.get
                        findIndex(elems(i - n))
                    case None =>
                        None
                }
            } else {
                None
            }
        case ArrayAccess(id, idx) if paramDeclsByName.contains(id) =>
            findIndex(idx) match {
                case Some(i) =>
                    val decl = paramDeclsByName(id)
                    val ArrayType(Some(IntRange(n, _)), _) = decl.paramType
                    val ArrayConst(elems) = decl.value
                    findIndex(elems(i - n))
                case None =>
                    None
            }
        case Term(id, Nil) if varDeclsByName.contains(id) =>
            val decl = varDeclsByName(id)
            if (decl.optionalValue.isDefined) {
                findIndex(varDeclsByName(id).optionalValue.get)
            } else {
                None
            }
        case Term(id, Nil) if paramDeclsByName.contains(id) =>
            findIndex(paramDeclsByName(id).value)
    }

    final def involvedVariables(constraint: Constraint): immutable.Set[Expr] =
        constraint.params.iterator.flatMap(involvedVariables).toSet

}
