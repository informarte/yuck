package yuck.flatzinc.parser

import scala.annotation.{switch, tailrec}
import scala.collection.*
import scala.util.control.Exception

import fastparse.*
import fastparse.Implicits.Repeater

import yuck.flatzinc.ast.*

/**
 * @author Michael Marte
 *
 */
object FlatZincParser {

    // Whitespace handling (comments and whitespace)
    // (copied from ScriptWhitespace with '#' replaced by '%')
    // https://github.com/com-lihaoyi/fastparse/issues/336
    // https://github.com/com-lihaoyi/fastparse/pull/337
    implicit object whitespace extends Whitespace {

        import fastparse.internal.Msgs

        def apply(ctx: ParsingRun[?]) = {
            val input = ctx.input

            @tailrec def rec(current: Int, state: Int): ParsingRun[Unit] = {
                if !input.isReachable(current) then {
                    if ctx.verboseFailures then {
                        ctx.reportTerminalMsg(current, Msgs.empty)
                    }
                    ctx.freshSuccessUnit(current)
                }
                else {
                    val currentChar = input(current)
                    (state: @switch) match {
                        case 0 =>
                            (currentChar: @switch) match {
                                case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
                                case '%' => rec(current + 1, state = 1)
                                case _ =>
                                    if ctx.verboseFailures then {
                                        ctx.reportTerminalMsg(current, Msgs.empty)
                                    }
                                    ctx.freshSuccessUnit(current)
                            }
                        case 1 => rec(current + 1, state = if currentChar == '\n' then 0 else state)
                    }
                }
            }

            rec(current = ctx.index, state = 0)
        }
    }

    // The default builder of fastparse creates an immutable list.
    private val exprVectorBuilder = new Repeater[Expr, Vector[Expr]] {
        type Acc = mutable.Buffer[Expr]
        def initial = mutable.Buffer.empty[Expr]
        def accumulate(t: Expr, acc: mutable.Buffer[Expr]) = acc += t
        def result(acc: mutable.Buffer[Expr]) = acc.toVector
    }

    def bool_const[$: P]: P[BoolConst] =
        P("true".map(_ => BoolConst(true)) | "false".map(_ => BoolConst(false)))

    def identifier[$: P]: P[String] =
        P((CharIn("_").rep ~ CharIn("A-Za-z") ~ CharIn("A-Za-z0-9_").rep).!)

    // Throws when the integer literal cannot be represented as a Long.
    def int_const[$: P]: P[IntConst] =
        P((CharIn("+\\-").? ~ CharIn("0-9").rep(1)).!.map(s => IntConst(s.toLong)))

    // Rejects integer literals that cannot be represented as a Long.
    // Slower than int_const due the double boxing caused by Catch.opt.
    def int_const_strict[$: P]: P[IntConst] = P(
        (CharIn("+\\-").? ~ CharIn("0-9").rep(1)).!
            .map(s => Exception.catching(classOf[NumberFormatException]).opt(s.toLong))
            .filter(maybeLong => maybeLong.isDefined)
            .map(maybeLong => IntConst(maybeLong.get))
    )

    def float_const_with_fractional_part[$: P]: P[String] =
        P((CharIn("+\\-").? ~ CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1) ~
            (CharIn("eE") ~ CharIn("+\\-").? ~ CharIn("0-9").rep(1)).?).!)

    def float_const_without_fractional_part[$: P]: P[String] =
        P((CharIn("+\\-").? ~ CharIn("0-9").rep(1) ~ CharIn("eE") ~ CharIn("+\\-").? ~ CharIn("0-9").rep(1)).!)

    // Throws when the float literal cannot be represented as a Double.
    def float_const[$: P]: P[FloatConst] = P(
        (float_const_without_fractional_part | float_const_with_fractional_part).map(s => {
            val n = s.toDouble
            if n.isInfinity then throw new NumberFormatException("%s is out of range")
            else FloatConst(n)
        })
    )

    // Rejects float literals that cannot be represented as a Double.
    // Slower than float_const due the double boxing caused by Catch.opt.
    def float_const_strict[$: P]: P[FloatConst] = P(
        (float_const_without_fractional_part | float_const_with_fractional_part)
            .map(s => Exception.catching(classOf[NumberFormatException]).opt(s.toDouble))
            .filter(maybeDouble => maybeDouble.filterNot(_.isInfinity).isDefined)
            .map(maybeDouble => FloatConst(maybeDouble.get))
    )

    def int_range[$: P]: P[IntRange] = P(
        (int_const ~ ".." ~ int_const).map {
            case (IntConst(lb), IntConst(ub)) => IntRange(lb, ub)
        }
    )

    def index_set[$: P]: P[Option[IntRange]] =
        P(("[" ~ int_range ~ "]").map(Some(_)) | P("[" ~ "int".rep(sep = ",") ~ "]").map(_ => None))

    def int_set[$: P]: P[IntSet] =
        P(("{" ~ int_const.rep(sep = ",") ~ "}").map(l => IntSet(l.view.map(_.value).toSet)))

    def float_range[$: P]: P[FloatRange] = P(
        (float_const ~ ".." ~ float_const).map {
            case (FloatConst(lb), FloatConst(ub)) => FloatRange(lb, ub)
        }
    )

    def int_set_const[$: P]: P[IntSetConst] =
        P((int_range.map(r => IntSetConst(r)) | int_set.map(s => IntSetConst(s))))

    def array_const[$: P]: P[ArrayConst] =
        P(("[" ~ expr.rep(sep = ",")(using exprVectorBuilder) ~ "]").map(ArrayConst.apply))

    def array_access[$: P]: P[ArrayAccess] = P(
        (identifier ~ "[" ~ expr ~ "]").map {
            case (id, idx) => ArrayAccess(id, idx)
        }
    )

    def string_const[$: P]: P[StringConst] =
        P(("\"" ~ identifier ~ "\"").map(StringConst.apply))

    def term[$: P]: P[Term] = P(
        (identifier ~ ("(" ~ expr.rep(1, sep = ",") ~ ")").?).map {
            case (id, optionalParams) => Term(id, optionalParams.getOrElse(Nil).toList)
        }
    )

    def expr[$: P]: P[Expr] =
        P(bool_const | float_const | int_set_const | int_const | array_const | array_access | string_const | term)

    def param_base_type[$: P]: P[BaseType] = P(
        "bool".map(_ => BoolType) |
        "int".map(_ => IntType(None)) |
        "float".map(_ => FloatType(None)) |
        int_range.map(r => IntType(Some(r))) |
        int_set.map(s => IntType(Some(s))) |
        float_range.map(r => FloatType(Some(r))) |
        ("set" ~ "of" ~ (
            int_range.map(r => IntSetType(Some(r))) |
                int_set.map(s => IntSetType(Some(s))) |
                "int".map(_ => IntSetType(None))
            )
        )
    )

    def param_array_type[$: P]: P[ArrayType] =
        P(array_type(param_base_type))

    def param_type[$: P]: P[Type] =
        P(param_base_type | param_array_type)

    def var_base_type[$: P]: P[BaseType] =
        P("var" ~ param_base_type)

    def var_array_type[$: P]: P[ArrayType] =
        P(array_type(var_base_type))

    def var_type[$: P]: P[Type] =
        P(var_base_type | var_array_type)

    def array_type[$: P](baseTypeParser: => P[BaseType]): P[ArrayType] = P(
        ("array" ~ index_set ~ "of" ~ baseTypeParser).map {
            case (indexSet, baseType) => ArrayType(indexSet, baseType)
        }
    )

    def pred_param_type[$: P]: P[Type] =
        P(param_type | var_type)

    def pred_param[$: P]: P[PredParam] = P(
        (pred_param_type ~ ":" ~ identifier ~ annotation.rep).map {
            case (paramType, id, annotations) => PredParam(id, paramType, annotations.toList)
        }
    )

    def pred_decl[$: P]: P[PredDecl] = P(
        ("predicate" ~/ identifier ~ "(" ~ pred_param.rep(sep = ",") ~ ")" ~ ";").map {
            case (id, params) => PredDecl(id, params.toList)
        }
    )

    def param_decl[$: P]: P[ParamDecl] = P(
        (param_type ~ ":" ~/ identifier ~ "=" ~ expr ~ ";").map {
            case (paramType, id, value) => ParamDecl(id, paramType, value)
        }
    )

    def var_decl[$: P]: P[VarDecl] = P(
        (var_type ~ ":" ~/ identifier ~ annotation.rep ~ ("=" ~ expr).? ~ ";").map {
            case (paramType, id, annotations, optionalValue) =>
                VarDecl(id, paramType, optionalValue, annotations.toList)
        }
    )

    def annotation[$: P]: P[Annotation] =
        P(("::" ~ term).map(Annotation.apply))

    def constraint[$: P]: P[Constraint] = P(
        ("constraint" ~/ identifier ~ "(" ~ expr.rep(1, sep = ",") ~ ")" ~ annotation.rep ~ ";").map {
            case (id, params, annotations) => Constraint(id, params.toList, annotations.toList)
        }
    )

    def solve_goal[$: P]: P[SolveGoal] = P(
        "solve" ~/ (
            (annotation.rep ~ "satisfy")./.map {
                case annotations => Satisfy(annotations.toList)
            } |
            (annotation.rep ~ "minimize" ~/ expr).map {
                case (annotations, expr) => Minimize(expr, annotations.toList)
            } |
            (annotation.rep ~ "maximize" ~/ expr).map {
                case (annotations, expr) => Maximize(expr, annotations.toList)
            }
        ) ~ ";"
    )

    def flatzinc_model[$: P]: P[FlatZincAst] = P(
        // Whitespace is only skipped by ~, so we begin parsing with Start.
        (Start ~/ pred_decl.rep ~/ param_decl.rep ~/ var_decl.rep ~/ constraint.rep ~/ solve_goal).map {
            case (predDecls, paramDecls, varDecls, constraints, solveGoal) =>
                FlatZincAst(
                    predDecls.toList,
                    predDecls.map(decl => decl.id -> decl).toMap,
                    paramDecls.toList,
                    paramDecls.map(decl => decl.id -> decl).toMap,
                    varDecls.toList,
                    varDecls.map(decl => decl.id -> decl).toMap,
                    constraints.toList,
                    solveGoal)
        }
    )

    // Reports error location in terms of line and column.
    def parse(input: String): FlatZincAst =
        fastparse.parse(input, flatzinc_model(using _)) match {
            case Parsed.Success(ast, successIndex) => ast
            case failure @ Parsed.Failure(label, index, extra) =>
                throw new FlatZincParserException(failure.msg)
        }

    // Reports error location in terms of its index.
    def parse(input: java.io.FileInputStream): FlatZincAst =
        fastparse.parse(input, flatzinc_model(using _)) match {
            case Parsed.Success(ast, successIndex) => ast
            case failure @ Parsed.Failure(label, index, extra) =>
                throw new FlatZincParserException(failure.msg)
        }

}
