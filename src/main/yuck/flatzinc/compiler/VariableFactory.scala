package yuck.flatzinc.compiler

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.ast.*

/**
 * Generates Yuck variables from FlatZinc parameter and variable declarations.
 *
 * For each class of FlatZinc parameters and variables that, in earlier phases, were identified to
 * be equivalent, a representative is chosen and only for this representative a Yuck
 * variable is introduced.
 *
 * Notice that other phases may introduce additional variables on-the-fly as needed.
 */
final class VariableFactory
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    override def run() = {
        cc.ast.paramDecls.iterator.filterNot(_.valueType.isArrayType).foreach(createVariable)
        cc.ast.paramDecls.iterator.filter(_.valueType.isArrayType).foreach(createVariables)
        cc.ast.varDecls.iterator.filterNot(_.valueType.isArrayType).foreach(createVariable)
        cc.ast.varDecls.iterator.filter(_.valueType.isArrayType).foreach(createVariables)
    }

    private def createVariable(decl: PlaceholderDecl): Unit = {
        decl.valueType match {
            case BoolType =>
                createVariable(Term(decl.id, Nil))(using BooleanTypeTraits)
            case IntType(_) =>
                createVariable(Term(decl.id, Nil))(using IntegerTypeTraits)
            case IntSetType(_) =>
                createVariable(Term(decl.id, Nil))(using IntegerSetTypeTraits)
            case other =>
                throw new UnsupportedFlatZincTypeException(other)
        }
    }

    private def createVariables(decl: PlaceholderDecl): Unit = {
        decl.valueType match {
            case ArrayType(Some(IntRange(1, n)), BoolType) =>
                cc.arrays += Term(decl.id, Nil) -> createArray(decl, n.toInt)(using BooleanTypeTraits)
            case ArrayType(Some(IntRange(1, n)), IntType(_)) =>
                cc.arrays += Term(decl.id, Nil) -> createArray(decl, n.toInt)(using IntegerTypeTraits)
            case ArrayType(Some(IntRange(1, n)), IntSetType(_)) =>
                cc.arrays += Term(decl.id, Nil) -> createArray(decl, n.toInt)(using IntegerSetTypeTraits)
            case other =>
                throw new UnsupportedFlatZincTypeException(other)
        }
    }

    private def createVariable
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
        (key: Expr)
        (using typeTraits: TypeTraits[A, D, X]):
        X =
    {
        if cc.sigint.isSet then {
            throw new FlatZincCompilerInterruptedException
        }
        def factory(key: Expr) =
            typeTraits.createVariable(cc.space, key.toString, typeTraits.safeDowncast(cc.domains(key)))
        val maybeEqualVars = cc.equalVars.get(key)
        if maybeEqualVars.isDefined then {
            val representative = maybeEqualVars.get.head
            if ! cc.vars.contains(representative) then {
                cc.vars += representative -> factory(representative)
            }
            val x = typeTraits.safeDowncast(cc.vars(representative))
            if key != representative then {
                cc.vars += key -> x
            }
            x
        }
        else {
            val x = factory(key)
            cc.vars += key -> x
            x
        }
    }

    private def createArray
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
        (decl: PlaceholderDecl, n: Int)
        (using typeTraits: TypeTraits[A, D, X]):
        immutable.IndexedSeq[X] =
    {
        Vector.tabulate(n)(idx => createVariable(ArrayAccess(decl.id, IntConst(idx + 1))))
    }

}
