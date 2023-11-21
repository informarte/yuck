package yuck.flatzinc.ast

import scala.collection.immutable.LinearSeq

import yuck.core.safeToInt

abstract class ValueSet[V]
final case class IntRange(lb: Long, ub: Long) extends ValueSet[Long] {
    override def toString = "%s..%s".format(lb, ub)
}
final case class IntSet(value: Set[Long]) extends ValueSet[Long] {
    override def toString =
        "{%s%s}".format(
            value.toBuffer.sorted.iterator.take(10).map(_.toString).mkString(", "),
            if (value.size > 10) ", ..." else "")
}
// The representation of floats is left to the implementation by the Zinc specification.
final case class FloatRange(lb: Double, ub: Double) extends ValueSet[Double] {
    override def toString = "%s..%s".format(lb, ub)
}

abstract class Expr {
    val isConst = false
}
abstract class ConstExpr extends Expr {
    override val isConst = true
}
final case class BoolConst(value: Boolean) extends ConstExpr {
    override def toString = value.toString
}
final case class IntConst(value: Long) extends ConstExpr {
    override def toString = value.toString
}
final case class FloatConst(value: Double) extends ConstExpr {
    override def toString = value.toString
}
final case class IntSetConst(value: ValueSet[Long]) extends ConstExpr
final case class ArrayConst(value: IndexedSeq[Expr]) extends ConstExpr {
    override def toString = "[%s]".format(value.iterator.map(_.toString).mkString(", "))
}
final case class ArrayAccess(id: String, idx: Expr) extends Expr {
    override def toString = "%s[%s]".format(id, idx)
}
final case class StringConst(string: String) extends Expr {
    override def toString = string
}
// In expressions, parameters and variables are represented as 0-ary terms.
final case class Term(id: String, params: LinearSeq[Expr]) extends Expr {
    override def toString =
        if (params.isEmpty) id else "%s(%s)".format(id, params.iterator.map(_.toString).mkString(", "))
}

abstract class Type {
    val isArrayType = false
}
abstract class BaseType extends Type
case object BoolType extends BaseType {
    override def toString = "bool"
}
final case class IntType(optionalDomain: Option[ValueSet[Long]]) extends BaseType {
    override def toString = optionalDomain match {
        case Some(domain) => domain.toString
        case None => "int"
    }
}
final case class FloatType(optionalDomain: Option[ValueSet[Double]]) extends BaseType {
    override def toString = optionalDomain match {
        case Some(domain) => domain.toString
        case None => "float"
    }
}
final case class IntSetType(optionalDomain: Option[ValueSet[Long]]) extends BaseType {
    override def toString = optionalDomain match {
        case Some(domain) => "set of %s".format(domain.toString)
        case None => "set of int"
    }
}
final case class ArrayType(optionalIndexSet: Option[ValueSet[Long]], baseType: BaseType) extends Type {
    override val isArrayType = true
    override def toString = optionalIndexSet match {
        case Some(indexSet) => "array [%s] of %s".format(indexSet, baseType)
        case None => "array of %s".format(baseType)
    }
}

final case class PredParam(id: String, paramType: Type, annotations: LinearSeq[Annotation]) {}
final case class PredDecl(id: String, params: LinearSeq[PredParam]) {}

abstract class PlaceholderDecl {
    def id: String
    def valueType: Type
    def optionalValue: Option[Expr]
}
final case class ParamDecl(override val id: String, override val valueType: Type, value: Expr) extends PlaceholderDecl {
    override def optionalValue = Some(value)
}
final case class VarDecl(
    override val id: String, override val valueType: Type, override val optionalValue: Option[Expr],
    annotations: LinearSeq[Annotation])
    extends PlaceholderDecl
{
    def isIntroduced: Boolean = annotations.contains(Annotation(Term("var_is_introduced", Nil)))
    def isDefined: Boolean = annotations.contains(Annotation(Term("is_defined_var", Nil)))
}

final case class Constraint(id: String, params: LinearSeq[Expr], annotations: LinearSeq[Annotation]) {
    override def toString = {
        val tmp =
            if (params.isEmpty) id
            else "%s(%s)".format(id, params.iterator.map(_.toString).mkString(", "))
        if (annotations.isEmpty) tmp
        else "%s :: %s".format(tmp, annotations.iterator.map(_.toString).mkString(", "))
    }
}

final case class Annotation(term: Term) {
    override def toString = term.toString
}

abstract class SolveGoal(val annotations: LinearSeq[Annotation])
final case class Satisfy(override val annotations: LinearSeq[Annotation]) extends SolveGoal(annotations)
final case class Minimize(objective: Expr, override val annotations: LinearSeq[Annotation]) extends SolveGoal(annotations)
final case class Maximize(objective: Expr, override val annotations: LinearSeq[Annotation]) extends SolveGoal(annotations)

final case class FlatZincAst(
    predDecls: LinearSeq[PredDecl],
    predDeclsByName: Map[String, PredDecl],
    paramDecls: LinearSeq[ParamDecl],
    paramDeclsByName: Map[String, ParamDecl],
    varDecls: LinearSeq[VarDecl],
    varDeclsByName: Map[String, VarDecl],
    constraints: LinearSeq[Constraint],
    solveGoal: SolveGoal)
{

    // Make the Intellij IDEA debugger usable
    override def toString = "AST"

    /** Returns the elements of the given array. */
    final def getArrayElems(expr: Expr): IndexedSeq[Expr] = expr match {
        case ArrayConst(elems) =>
            elems
        case Term(id, Nil) if varDeclsByName.contains(id) =>
            val decl = varDeclsByName(id)
            if (decl.optionalValue.isDefined) {
                val ArrayConst(elems) = decl.optionalValue.get: @unchecked
                elems
            } else {
                val ArrayType(Some(IntRange(n, m)), _) = decl.valueType: @unchecked
                (n to m).map(i => ArrayAccess(id, IntConst(i)))
            }
        case Term(id, Nil) if paramDeclsByName.contains(id) =>
            val ArrayConst(elems) = paramDeclsByName(id).value: @unchecked
            elems
        case _ =>
            throw new IllegalArgumentException("%s is not an array".format(expr))
    }

    /** Returns the variables involved in the given expression. */
    def involvedVariables(expr: Expr): Set[Expr] = expr match {
        case BoolConst(_) | IntConst(_) | FloatConst(_) | IntSetConst(_) =>
            Set()
        case ArrayConst(elems) =>
            elems.iterator.flatMap(involvedVariables).toSet
        case ArrayAccess(id, idx) if varDeclsByName.contains(id) =>
            val decl = varDeclsByName(id)
            if (decl.optionalValue.isDefined) {
                findIndex(idx) match {
                    case Some(i) =>
                        val ArrayType(Some(IntRange(n, m)), _) = decl.valueType: @unchecked
                        val ArrayConst(elems) = decl.optionalValue.get: @unchecked
                        involvedVariables(elems(i - safeToInt(n)))
                    case None =>
                        Set(expr)
                }
            } else {
                Set(expr)
            }
        case ArrayAccess(id, _) if paramDeclsByName.contains(id) =>
            Set()
        case Term(id, Nil) if varDeclsByName.contains(id) =>
            if (varDeclsByName(id).valueType.isArrayType) getArrayElems(expr).iterator.flatMap(involvedVariables).toSet
            else Set(expr)
        case Term(id, Nil) if paramDeclsByName.contains(id) =>
            Set()
    }

    private def findIndex(expr: Expr): Option[Int] = expr match {
        case IntConst(i) =>
            Some(safeToInt(i))
        case ArrayAccess(id, idx) if varDeclsByName.contains(id) =>
            val decl = varDeclsByName(id)
            if (decl.optionalValue.isDefined) {
                findIndex(idx) match {
                    case Some(i) =>
                        val ArrayType(Some(IntRange(n, _)), _) = decl.valueType: @unchecked
                        val ArrayConst(elems) = decl.optionalValue.get: @unchecked
                        findIndex(elems(i - safeToInt(n)))
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
                    val ArrayType(Some(IntRange(n, _)), _) = decl.valueType: @unchecked
                    val ArrayConst(elems) = decl.value: @unchecked
                    findIndex(elems(i - safeToInt(n)))
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

    final def involvedVariables(constraint: Constraint): Set[Expr] =
        constraint.params.iterator.flatMap(involvedVariables).toSet

}
