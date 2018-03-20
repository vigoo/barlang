package io.github.vigoo.barlang

import io.github.vigoo.barlang.compiler.Compiler._
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements.VariableDeclaration
import io.github.vigoo.barlang.language.{SingleStatement, SymbolName, Type, Types}
import io.github.vigoo.bash.language._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.matcher.Matcher

class StatementCompilerSpecs extends Specification { def is = s2"""
  The compiler is able to
    variable declaration with string literal   $variableDeclStringLit
    variable declaration with bool literal     $variableDeclBoolLit
    variable declaration from another variable $variableDeclOtherVariable
    variable declaration from a function ref   $variableDeclFunctionRef
    variable declaration from an array elem    $variableDeclArrayElem
  """

  val exampleContext: Map[SymbolName, Type] = Map(
    SymbolName("x") -> Types.Int(),
    SymbolName("f") -> Types.Function(List.empty, List(Types.Int(), Types.String()), Types.String()),
    SymbolName("g") -> Types.Function(List.empty, List(Types.String()), Types.Unit()),
    SymbolName("strLst") -> Types.Array(Types.String())
  )

  def variableDeclStringLit =
    VariableDeclaration(SymbolName("test"), StringLiteral("hello world")) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("hello world"))
    )

  def variableDeclBoolLit =
    (VariableDeclaration(SymbolName("test"), BoolLiteral(true)) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("0"))
    )) and (VariableDeclaration(SymbolName("test"), BoolLiteral(false)) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("1"))
    ))

  def variableDeclOtherVariable =
    VariableDeclaration(SymbolName("y"), Variable(SymbolName("x"))) must compileTo(
      BashStatements.Assign(BashIdentifier("y"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("x"))))
    )

  def variableDeclFunctionRef =
    VariableDeclaration(SymbolName("fn"), Variable(SymbolName("f"))) must compileTo(
      BashStatements.Assign(BashIdentifier("fn"), BashExpressions.Literal("f"))
    )

  def variableDeclArrayElem =
    VariableDeclaration(SymbolName("second"), ArrayAccess(SymbolName("strLst"), IntLiteral(1))) must compileTo(
      BashStatements.Sequence(List(
        BashStatements.Declare(
          Set(BashDeclareOptions.Array, BashDeclareOptions.ReadOnly),
          BashIdentifier("__tmp1"),
          Some(BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("strLst")), BashArrayIndices.All))
        ),
        BashStatements.Assign(
          BashIdentifier("second"),
          BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("__tmp1")),
          BashArrayIndices.Index(BashExpressions.Literal("1"))))
      ))
    )

  def compileTo(bashStatement: BashStatement): Matcher[SingleStatement] =
    beRight(bashStatement) ^^ { (singleStatement: SingleStatement) =>
      val context = createCustomContext(exampleContext)
      compileSingleStatement(singleStatement).runEither.evalState(context).run
    }

  private def createCustomContext(types: Map[SymbolName, Type]): Context = {
    val customContext = Context(
      scope = GlobalScope,
      symbols = types.map { case (name, _) => name -> AssignedSymbol(name, GlobalScope.identifierPrefix + name.name) },
      symbolTypes = types.mapValues(SimpleType.apply),
      lastTmp = 0
    )
    customContext
  }
}
