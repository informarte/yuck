package yuck.flatzinc.parser

import scala.language.postfixOps
import scala.util.control.Exception
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

import yuck.flatzinc.ast.*

/**
 * @author Michael Marte
 *
 */
object FlatZincParser extends RegexParsers {

    lazy val bool_const: Parser[BoolConst] =
        "true" ^^^ BoolConst(true) | "false" ^^^ BoolConst(false)
    lazy val identifier: Parser[String] =
        regex(new Regex("_*[A-Za-z][A-Za-z0-9_]*"))
    lazy val int_const: Parser[IntConst] =
        regex(new Regex("(\\+|-)?[0-9]+")) ^? (
            new PartialFunction[String, IntConst]() {
                override def isDefinedAt(s: String) =
                    Exception.catching[Long](classOf[NumberFormatException]).opt{s.toLong}.isDefined
                override def apply(s: String) =
                    IntConst(s.toLong)
            },
            s => String.format("Invalid integer literal %s", s))
    lazy val float_const_with_fractional_part: Parser[String] =
        regex(new Regex("(\\+|-)?[0-9]+\\.[0-9]+((e|E)(\\+|-)?[0-9]+)?"))
    lazy val float_const_without_fractional_part: Parser[String] =
        regex(new Regex("(\\+|-)?[0-9]+(e|E)(\\+|-)?[0-9]+"))
    lazy val float_const: Parser[FloatConst] =
        (float_const_without_fractional_part | float_const_with_fractional_part) ^? (
            new PartialFunction[String, FloatConst]() {
                override def isDefinedAt(s: String) =
                    Exception.catching[Double](classOf[NumberFormatException]).opt{s.toDouble}
                        .filterNot(_.isInfinity).isDefined
                override def apply(s: String) =
                    FloatConst(s.toDouble)
            },
            s => String.format("Invalid float literal %s", s))
    lazy val int_range: Parser[IntRange] =
        int_const ~ ".." ~ int_const ^^ {
            case IntConst(lb) ~ _ ~ IntConst(ub) => IntRange(lb, ub)
        }
    lazy val index_set: Parser[Option[IntRange]] =
        "[" ~> int_range <~ "]" ^^ (range => Some(range)) |
        "[" ~ repsep("int", ",") ~ "]" ^^^ None
    lazy val int_set: Parser[IntSet] =
        "{" ~> repsep(int_const, ",") <~ "}" ^^ {
            l => IntSet((for (IntConst(e) <- l) yield e).toSet)
        }
    lazy val float_range: Parser[FloatRange] =
        float_const ~ ".." ~ float_const ^^ {
            case FloatConst(lb) ~ _ ~ FloatConst(ub) => FloatRange(lb, ub)
        }
    lazy val int_set_const: Parser[IntSetConst] =
        int_range ^^ (r => IntSetConst(r)) |
        int_set ^^ (s => IntSetConst(s))
    lazy val array_const: Parser[ArrayConst] =
        "[" ~> repsep(expr, ",") <~ "]" ^^ (elems => ArrayConst(elems.toIndexedSeq))
    lazy val array_access: Parser[ArrayAccess] =
        identifier ~ ("[" ~> expr <~ "]") ^^ {
            case id ~ idx => ArrayAccess(id, idx)
        }
    // limited string parsing only
    lazy val string_const: Parser[StringConst] =
        "\"" ~> identifier <~ "\"" ^^ StringConst.apply
    lazy val term: Parser[Term] =
        identifier ~ (("(" ~> rep1sep(expr, ",") <~ ")")?) ^^ {
            case id ~ optionalParams => Term(id, optionalParams.getOrElse(Nil))
        }
    lazy val expr: Parser[Expr] =
        (bool_const | float_const | int_set_const | int_const | array_const | array_access | string_const | term)

    // type parsing
    // According to the FlatZinc grammar, not every type applies in every context.
    // I ignore these restrictions here because I think that the type checker is the
    // better place to implement them.
    lazy val param_base_type: Parser[BaseType] =
        "bool" ^^^ BoolType |||
        "int" ^^^ IntType(None) |||
        "float" ^^^ FloatType(None) |||
        int_range ^^ (r => IntType(Some(r))) |||
        int_set ^^ (s => IntType(Some(s))) |||
        float_range ^^ (r => FloatType(Some(r))) |||
        "set" ~> "of" ~>
            (int_range ^^ (r => IntSetType(Some(r))) |||
             int_set ^^ (s => IntSetType(Some(s))) |||
             "int" ^^^ IntSetType(None))
    lazy val param_array_type: Parser[ArrayType] =
        array_type(param_base_type)
    lazy val param_type: Parser[Type] =
        param_base_type | param_array_type
    lazy val var_base_type: Parser[BaseType] =
        "var" ~> param_base_type
    lazy val var_array_type: Parser[ArrayType] =
        array_type(var_base_type)
    lazy val var_type: Parser[Type] =
        var_base_type | var_array_type
    def array_type(baseTypeParser: Parser[BaseType]): Parser[ArrayType] =
        ("array" ~> index_set) ~ ("of" ~> baseTypeParser) ^^ {
            case indexSet ~ baseType => ArrayType(indexSet, baseType)
        }

    lazy val pred_param_type: Parser[Type] =
        param_type | var_type
    lazy val pred_param: Parser[PredParam] =
        (pred_param_type <~ ":") ~ identifier ^^ {
            case paramType ~ id => PredParam(id, paramType)
        }
    lazy val pred_decl: Parser[PredDecl] =
        "predicate" ~> identifier ~ ("(" ~> repsep(pred_param, ",") <~ ")") <~ ";" ^^ {
            case id ~ params=> PredDecl(id, params)
        }

    lazy val param_decl: Parser[ParamDecl] =
        (param_type <~ ":") ~ identifier ~ ("=" ~> expr) <~ ";" ^^ {
            case paramType ~ id ~ value => ParamDecl(id, paramType, value)
        }

    lazy val var_decl: Parser[VarDecl] =
        (var_type <~ ":") ~ identifier ~ (annotation*) ~ (("=" ~> expr)?) <~ ";" ^^ {
            case paramType ~ id ~ annotations ~ optionalValue => VarDecl(id, paramType, optionalValue, annotations)
        }

    lazy val annotation: Parser[Annotation] =
        "::" ~> term ^^ Annotation.apply

    lazy val constraint: Parser[Constraint] =
        "constraint" ~> identifier ~ ("(" ~> rep1sep(expr, ",") <~ ")") ~ (annotation*) <~ ";" ^^ {
            case id ~ params ~ annotations => Constraint(id, params, annotations)
        }

    lazy val solve_goal: Parser[SolveGoal] =
        "solve" ~> (
            (annotation*) <~ "satisfy" ^^ Satisfy.apply |
            (annotation*) ~ ("minimize" ~> expr) ^^ {
                case annotations ~ expr => Minimize(expr, annotations)
            } |
            (annotation*) ~ ("maximize" ~> expr) ^^ {
                case annotations ~ expr => Maximize(expr, annotations)
            }) <~ ";"

    lazy val flatzinc_model: Parser[FlatZincAst] =
        (pred_decl*)  ~ (param_decl*) ~ (var_decl*) ~ (constraint*)  ~ solve_goal ^^ {
            case predDecls ~ paramDecls ~ varDecls ~ constraints ~ solveGoal =>
                FlatZincAst(
                    predDecls,
                    predDecls.map(decl => (decl.id -> decl)).toMap,
                    paramDecls,
                    paramDecls.map(decl => (decl.id -> decl)).toMap,
                    varDecls,
                    varDecls.map(decl => (decl.id -> decl)).toMap,
                    constraints,
                    solveGoal)
        }

    def parse(input: java.lang.CharSequence): FlatZincAst =
        processParsingResult(FlatZincParser.parseAll(FlatZincParser.flatzinc_model, input))

    def parse(reader: java.io.InputStreamReader): FlatZincAst =
        processParsingResult(FlatZincParser.parseAll(FlatZincParser.flatzinc_model, reader))

    private def processParsingResult(result: ParseResult[FlatZincAst]): FlatZincAst = (result: @unchecked) match {
        case FlatZincParser.Success(ast, _) => ast
        case FlatZincParser.NoSuccess(msg, rest) =>
            throw new FlatZincParserException(rest.pos.line, rest.pos.column, msg)
    }

}
