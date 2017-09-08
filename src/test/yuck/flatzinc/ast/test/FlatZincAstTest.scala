package yuck.flatzinc.ast.test

import org.junit.Assert
import org.junit.Test

import yuck.flatzinc.ast._
import yuck.util.testing.UnitTest

final class FlatZincAstTest extends UnitTest {

    private val a = ParamDecl("a", ArrayType(Some(IntRange(0, 1)), IntType(None)), ArrayConst(List(IntConst(2), IntConst(3))))
    private val c = ParamDecl("c", IntType(None), IntConst(2))
    private val x = VarDecl("x", IntType(None), Some(IntConst(0)), Nil)
    private val u = VarDecl("u", ArrayType(Some(IntRange(3, 4)), IntType(None)), None, Nil)
    private val v = VarDecl("v", ArrayType(Some(IntRange(1, 4)), IntType(None)), Some(ArrayConst(List(IntConst(0), Term(c.id, Nil), Term(x.id, Nil), ArrayAccess(u.id, IntConst(3))))), Nil)
    private val w = VarDecl("w", ArrayType(Some(IntRange(0, 2)), IntType(None)), Some(ArrayConst(List(ArrayAccess(v.id, Term(c.id, Nil)), ArrayAccess(v.id, ArrayAccess(a.id, IntConst(0))), ArrayAccess(v.id, ArrayAccess(a.id, IntConst(1)))))), Nil)

    private val ast =
        FlatZincAst(
            predDecls = Nil,
            predDeclsByName = Map(),
            paramDecls = List(a),
            paramDeclsByName = Map(a.id -> a, c.id -> c),
            varDecls = List(u, v, x),
            varDeclsByName = Map(u.id -> u, v.id -> v, w.id -> w, x.id -> x),
            constraints = Nil,
            solveGoal = Satisfy(Nil))

    @Test
    def testGetArrayElems {
        assertEq(
            ast.getArrayElems(ArrayConst(List(BoolConst(true), BoolConst(false)))).toList,
            List(BoolConst(true), BoolConst(false)))
        assertEq(
            ast.getArrayElems(Term(a.id, Nil)).toList,
            List(IntConst(2), IntConst(3)))
        assertEq(
            ast.getArrayElems(Term(u.id, Nil)).toList,
            List(ArrayAccess("u", IntConst(3)), ArrayAccess("u", IntConst(4))))
        assertEq(
            ast.getArrayElems(Term(v.id, Nil)).toList,
            List(IntConst(0), Term(c.id, Nil), Term(x.id, Nil), ArrayAccess(u.id, IntConst(3))))
        assertEx(
            ast.getArrayElems(Term("blah", Nil)))
    }

    @Test
    def testInvolvedVariables {

        assertEq(
            ast.involvedVariables(BoolConst(false)),
            Set())
        assertEq(
            ast.involvedVariables(IntConst(0)),
            Set())
        assertEq(
            ast.involvedVariables(FloatConst(0)),
            Set())
        assertEq(
            ast.involvedVariables(IntSetConst(IntRange(1, 10))),
            Set())
        assertEq(
            ast.involvedVariables(ArrayConst(List(IntConst(0), Term(x.id, Nil)))),
            Set(Term(x.id, Nil)))

        assertEq(
            ast.involvedVariables(Term(a.id, Nil)),
            Set())
        assertEq(
            ast.involvedVariables(Term(c.id, Nil)),
            Set())
        assertEq(
            ast.involvedVariables(Term(x.id, Nil)),
            Set(Term(x.id, Nil)))

        assertEq(
            ast.involvedVariables(ArrayAccess(u.id, IntConst(3))),
            Set(ArrayAccess(u.id, IntConst(3))))
        assertEq(
            ast.involvedVariables(ArrayAccess(u.id, IntConst(4))),
            Set(ArrayAccess(u.id, IntConst(4))))

        assertEq(
            ast.involvedVariables(ArrayAccess(v.id, IntConst(1))),
            Set())
        assertEq(
            ast.involvedVariables(ArrayAccess(v.id, IntConst(2))),
            Set())
        assertEq(
            ast.involvedVariables(ArrayAccess(v.id, IntConst(3))),
            Set(Term(x.id, Nil)))
        assertEq(
            ast.involvedVariables(ArrayAccess(v.id, IntConst(4))),
            Set(ArrayAccess(u.id, IntConst(3))))

        assertEq(
            ast.involvedVariables(ArrayAccess(w.id, IntConst(0))),
            Set())
        assertEq(
            ast.involvedVariables(ArrayAccess(w.id, IntConst(1))),
            Set())
        assertEq(
            ast.involvedVariables(ArrayAccess(w.id, IntConst(2))),
            Set(Term(x.id, Nil)))

        assertEq(
            ast.involvedVariables(Term(u.id, Nil)),
            Set(ArrayAccess(u.id, IntConst(3)), ArrayAccess(u.id, IntConst(4))))
        assertEq(
            ast.involvedVariables(Term(v.id, Nil)),
            Set(Term(x.id, Nil), ArrayAccess(u.id, IntConst(3))))
        assertEq(
            ast.involvedVariables(Term(w.id, Nil)),
            Set(Term(x.id, Nil)))

        assertEq(
            ast.involvedVariables(Constraint("blah", List(Term(u.id, Nil), Term(v.id, Nil), Term(w.id, Nil)), Nil)),
            Set(Term(x.id, Nil), ArrayAccess(u.id, IntConst(3)), ArrayAccess(u.id, IntConst(4))))

    }

}
