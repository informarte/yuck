package yuck.flatzinc.parser.test

import java.io.{File, FileInputStream, InputStreamReader}

import org.junit.{Assert, Test}

import yuck.flatzinc.ast.*
import yuck.flatzinc.parser.*
import yuck.test.util.UnitTest

final class FlatZincParserTest extends UnitTest {

    import FlatZincParser.*

    private def expectSuccess[Result](parser: Parser[Result], input: String, expectation: Result): Unit = {
        parseAll(parser, input) match {
            case FlatZincParser.Success(result, rest) =>
                Assert.assertEquals(expectation, result)
            case _ =>
                Assert.fail("Failed to parse '%s'".format(input))
        }
    }

    private def expectFailure[Result](parser: Parser[Result], input: String): Unit = {
        parseAll(parser, input) match {
            case FlatZincParser.Success(result, rest) =>
                Assert.fail("'%s' was parsed unexpectedly".format(input))
            case _ =>
        }
    }

    @Test
    def testBool(): Unit = {
        expectSuccess(expr, "true", BoolConst(true))
        expectSuccess(expr, "false", BoolConst(false))
    }

    @Test
    def testInt(): Unit = {
        expectSuccess(expr, "1", IntConst(1))
        expectSuccess(expr, "-1", IntConst(-1))
        expectFailure(expr, "++1")
        expectFailure(expr, "+-1")
        // fails on Java 6
        expectSuccess(expr, "+1", IntConst(1))
        expectFailure(expr, "3486784401")
    }

    @Test
    def testFloat(): Unit = {
        expectSuccess(expr, "1.5", FloatConst(1.5))
        expectSuccess(expr, "+1.5", FloatConst(1.5))
        expectSuccess(expr, "-1.5", FloatConst(-1.5))
        expectSuccess(expr, "1.5e2", FloatConst(150))
        expectSuccess(expr, "+1.5e2", FloatConst(150))
        expectSuccess(expr, "1.5e+2", FloatConst(150))
        expectSuccess(expr, "+1.5e+2", FloatConst(150))
        expectSuccess(expr, "-1.5e2", FloatConst(-150))
        expectSuccess(expr, "-1.5e+2", FloatConst(-150))
        expectSuccess(expr, "1.5e-2", FloatConst(0.015))
        expectSuccess(expr, "+1.5e-2", FloatConst(0.015))
        expectSuccess(expr, "-1.5e-2", FloatConst(-0.015))
        expectSuccess(expr, "1e2", FloatConst(100))
        expectSuccess(expr, "+1e2", FloatConst(100))
        expectSuccess(expr, "1e+2", FloatConst(100))
        expectSuccess(expr, "+1e+2", FloatConst(100))
        expectSuccess(expr, "-1e2", FloatConst(-100))
        expectSuccess(expr, "-1e+2", FloatConst(-100))
        expectSuccess(expr, "1e-2", FloatConst(0.01))
        expectSuccess(expr, "+1e-2", FloatConst(0.01))
        expectSuccess(expr, "-1e-2", FloatConst(-0.01))
        expectSuccess(expr, "-1.56734454885781264827637856876e-178", FloatConst(-1.56734454885781264827637856876e-178))
        expectFailure(expr, "1e+309")
    }

    @Test
    def testIntRange(): Unit = {
        expectSuccess(expr, "1..5", IntSetConst(IntRange(1, 5)))
        expectSuccess(expr, "1 .. 5", IntSetConst(IntRange(1, 5)))
    }

    @Test
    def testIntSet(): Unit = {
        expectSuccess(expr, "{}", IntSetConst(IntSet(Set())))
        expectSuccess(expr, "{1}", IntSetConst(IntSet(Set(1))))
        expectSuccess(expr, "{-2, 1, 1, 3, 5}", IntSetConst(IntSet(Set(-2, 1, 3, 5))))
        expectFailure(expr, "{true}")
        expectFailure(expr, "{1.5}")
        expectFailure(expr, "{[]}")
    }

    @Test
    def testArray(): Unit = {
        expectSuccess(expr, "[]", ArrayConst(Nil))
        expectSuccess(expr, "[1]", ArrayConst(List(IntConst(1))))
        expectSuccess(expr, "[2, 1]", ArrayConst(List(IntConst(2), IntConst(1))))
        expectSuccess(expr, "[a[2], 1, a[1]]", ArrayConst(List(ArrayAccess("a", IntConst(2)), IntConst(1), ArrayAccess("a", IntConst(1)))))
        expectSuccess(expr, "[true, false]", ArrayConst(List(BoolConst(true), BoolConst(false))))
        expectSuccess(expr, "[[]]", ArrayConst(List(ArrayConst(List()))))
        expectFailure(expr, "a[]")
        expectSuccess(expr, "a[1]", ArrayAccess("a", IntConst(1)))
        expectSuccess(expr, "a[b[1]]", ArrayAccess("a", ArrayAccess("b", IntConst(1))))
        expectFailure(expr, "a[1, 2]")
    }

    @Test
    def testString(): Unit = {
        expectSuccess(expr, "\"foo\"", StringConst("foo"))
    }

    @Test
    def testTypes(): Unit = {
        val map = Map(
            "bool" -> BoolType,
            "float" -> FloatType(None),
            "1.0 .. 2.0" -> FloatType(Some(FloatRange(1, 2))),
            "int" -> IntType(None),
            "1 .. 2" -> IntType(Some(IntRange(1, 2))),
            "{1, 3, 5}" ->  IntType(Some(IntSet(Set(1, 3, 5)))),
            "set of int" -> IntSetType(None),
            "set of 1 .. 2" -> IntSetType(Some(IntRange(1, 2))),
            "set of {1, 3, 5}" -> IntSetType(Some(IntSet(Set(1, 3, 5)))))
        map.foreach(entry => expectSuccess(param_type, entry._1, entry._2))
        map.foreach(entry => expectSuccess(param_type, "array [1 .. 5] of %s".format(entry._1), ArrayType(Some(IntRange(1, 5)), entry._2)))
        map.foreach(entry => expectSuccess(var_type, "var %s".format(entry._1), entry._2))
        map.foreach(entry => expectSuccess(var_type, "array [1 .. 5] of var %s".format(entry._1), ArrayType(Some(IntRange(1, 5)), entry._2)))
    }

    @Test
    def testPredDecl(): Unit = {
        expectSuccess(
            pred_decl,
            "predicate all_different_int(array [int] of var int: x);",
            PredDecl("all_different_int", List(PredParam("x", ArrayType(None, IntType(None))))))
    }

    @Test
    def testParamDecl(): Unit = {
        expectSuccess(
            param_decl,
            "array [1..4] of int: distance = [5, 5, 5, 5];",
            ParamDecl(
                "distance",
                ArrayType(Some(IntRange(1, 4)), IntType(None)),
                ArrayConst(List(IntConst(5), IntConst(5), IntConst(5), IntConst(5)))))
    }

    @Test
    def testVarDecl(): Unit = {
        expectSuccess(
            var_decl,
            "var 0..1: INT____00091 :: is_defined_var :: var_is_introduced;",
            VarDecl(
                "INT____00091",
                IntType(Some(IntRange(0,  1))),
                None,
                List(Annotation(Term("is_defined_var", Nil)), Annotation(Term("var_is_introduced", Nil)))))
        expectSuccess(
            var_decl,
            "array [1..25] of var 0..1140: arrival;",
            VarDecl(
                "arrival",
                ArrayType(Some(IntRange(1, 25)), IntType(Some(IntRange(0, 1140)))),
                None,
                Nil))
        expectSuccess(
            var_decl,
            "var 0..2850000: cost :: output_var = INT____00006;",
            VarDecl(
                "cost",
                IntType(Some(IntRange(0, 2850000))),
                Some(Term("INT____00006", Nil)),
                List(Annotation(Term("output_var", Nil)))))
    }

    @Test
    def testConstraint(): Unit = {
        expectSuccess(
            constraint,
            "constraint bool2int(BOOL____00090, BOOL____00091) :: defines_var(INT____00091);",
            Constraint(
                "bool2int",
                List(Term("BOOL____00090", Nil), Term("BOOL____00091", Nil)),
                List(Annotation(Term("defines_var", List(Term("INT____00091", Nil)))))))
    }

    @Test
    def testSolveGoal(): Unit = {
        expectSuccess(solve_goal, "solve satisfy;", Satisfy(Nil))
        expectSuccess(solve_goal, "solve minimize x;", Minimize(Term("x", Nil), Nil))
        expectSuccess(solve_goal, "solve maximize x;", Maximize(Term("x", Nil), Nil))
        expectSuccess(
            solve_goal,
            "solve :: int_search(first_fail, indomain_max) satisfy;",
            Satisfy(List(Annotation(Term("int_search", List(Term("first_fail", Nil), Term("indomain_max", Nil)))))))
    }

    private def parseFlatZincFile(file: File): Boolean = {
          logger.log("Parsing %s".format(file))
          val reader = new InputStreamReader(new FileInputStream(file))
          FlatZincParser.parse(FlatZincParser.flatzinc_model, reader) match {
              case FlatZincParser.Success(f, rest) => true
              case f@_ => logger.log(f.toString); false
          }
    }

}
