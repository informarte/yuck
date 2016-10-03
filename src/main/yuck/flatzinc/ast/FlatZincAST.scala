package yuck.flatzinc.ast

trait ValueSet[Value]
final case class IntRange(val lb: Int, val ub: Int) extends ValueSet[Int] {
    override def toString = "[%s, %s]".format(lb, ub)
}
final case class IntSet(val value: Set[Int]) extends ValueSet[Int] {
    override def toString =
        "{%s%s}".format(
            value.toList.sorted.take(10).map(_.toString).mkString(", "),
            if (value.size > 10) ", ..." else "")
}
// The representation of floats is left to the implementation by the Zinc specification.
final case class FloatRange(val lb: Double, val ub: Double) extends ValueSet[Double]

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
    override def toString = "[%s]".format(value.map(_.toString).mkString(", "))
}
final case class ArrayAccess(val id: String, val idx: Expr) extends Expr {
    override def toString = "%s[%s]".format(id, idx)
}
// In expressions, parameters and variables are represented as 0-ary terms.
final case class Term(val id: String, val params: List[Expr]) extends Expr {
    override def toString =
        if (params.isEmpty) id else "%s(%s)".format(id, params.map(_.toString).mkString(", "))
}

trait Type {
    val isArrayType = false
}
trait BaseType extends Type
final case object BoolType extends BaseType
final case class IntType(val optionalDomain: Option[ValueSet[Int]]) extends BaseType
final case class FloatType(val optionalDomain: Option[ValueSet[Double]]) extends BaseType
final case class IntSetType(val optionalDomain: Option[ValueSet[Int]]) extends BaseType
final case class ArrayType(val optionalIndexSet: Option[ValueSet[Int]], baseType: BaseType) extends Type {
    override val isArrayType = true
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
            else "%s(%s)".format(id, params.map(_.toString).mkString(", "))
        if (annotations.isEmpty) tmp
        else "%s :: %s".format(tmp, annotations.map(_.toString).mkString(", "))
    }
}

final case class Annotation(val term: Term) {
    override def toString = term.toString
}

abstract class SolveGoal(val annotations: List[Annotation])
final case class Satisfy(override val annotations: List[Annotation]) extends SolveGoal(annotations)
final case class Minimize(val objective: Expr, override val annotations: List[Annotation]) extends SolveGoal(annotations)
final case class Maximize(val objective: Expr, override val annotations: List[Annotation]) extends SolveGoal(annotations)

final case class FlatZincAST(
    val predDecls: List[PredDecl],
    val predDeclsByName: Map[String, PredDecl],
    val paramDecls: List[ParamDecl],
    val paramDeclsByName: Map[String, ParamDecl],
    val varDecls: List[VarDecl],
    val varDeclsByName: Map[String, VarDecl],
    val constraints: List[Constraint],
    val solveGoal: SolveGoal)
