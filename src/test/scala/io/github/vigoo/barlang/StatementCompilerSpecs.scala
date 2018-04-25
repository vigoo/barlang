package io.github.vigoo.barlang

import io.github.vigoo.barlang.compiler.Compiler._
import io.github.vigoo.barlang.language.BinaryOperators
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements.VariableDeclaration
import io.github.vigoo.barlang.language.{SingleStatement, SymbolName, Type, Types}
import io.github.vigoo.bash.language._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.matcher.Matcher

class StatementCompilerSpecs extends Specification {
  def is =
    s2"""
  The compiler is able to
    variable declaration with string literal         $variableDeclStringLit
    variable declaration with bool literal           $variableDeclBoolLit
    variable declaration from another variable       $variableDeclOtherVariable
    variable declaration from a function ref         $variableDeclFunctionRef
    variable declaration from an array elem          $variableDeclArrayElem
    variable declaration from a double literal       $variableDeclDoubleLit
    variable declaration from a int literal          $variableDeclIntLit
    variable declaration from a double expression    $variableDeclDoubleExpr
    variable declaration from a sys variable         $variableDeclSysVar
    variable declaration from a predefined constant  $variableDeclPredefConst
    variable declaration from an integer expression  $variableDeclIntegerExpr
    variable declaration from a function call        $variableDeclFunCall
    variable declaration from a predefined funcall   $variableDeclPredefinedFunCall
    variable declaration from a string expression    $variableDeclStringExpr
    variable declaration from a boolean expression   $variableDeclBoolExpr
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

  def variableDeclSysVar =
    VariableDeclaration(SymbolName("path2"), SystemVariable(SymbolName("PATH"))) must compileTo(
      BashStatements.Assign(BashIdentifier("path2"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("PATH"))))
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

  def variableDeclIntLit =
    VariableDeclaration(SymbolName("test"), IntLiteral(10)) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("10"))
    )

  def variableDeclDoubleLit =
    VariableDeclaration(SymbolName("test"), DoubleLiteral(0.1)) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("0.1"))
    )

  def variableDeclDoubleExpr =
    VariableDeclaration(SymbolName("test"),
      BinaryOp(BinaryOperators.Add,
        DoubleLiteral(1.0),
        BinaryOp(BinaryOperators.Div,
          Variable(SymbolName("x")),
          IntLiteral(2)))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("__tmp1"), BashExpressions.Eval(
          BashStatements.Command(
            BashExpressions.Literal("bc"),
            List(BashExpressions.Literal("-l")),
            Some(BashExpressions.Literal("(1.0) + (($x) / (2.0))")))
        )),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("__tmp1"))))))
    )

  def variableDeclIntegerExpr =
    VariableDeclaration(SymbolName("test"),
      BinaryOp(BinaryOperators.Add,
        IntLiteral(1),
        BinaryOp(BinaryOperators.Div,
          Variable(SymbolName("x")),
          IntLiteral(2)))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("__tmp1"), BashExpressions.EvalArithmetic(
          BashArithmeticExpressions.Add(
            BashArithmeticExpressions.Number(1),
            BashArithmeticExpressions.Div(
              BashArithmeticExpressions.Variable(BashVariables.Variable(BashIdentifier("x"))),
              BashArithmeticExpressions.Number(2)
            )
          )
        )),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("__tmp1"))))))
    )

  def variableDeclPredefConst =
    VariableDeclaration(SymbolName("test"), Predefined(SymbolName("pi"))) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal(Math.PI.toString))
    )

  def variableDeclFunCall =
    VariableDeclaration(
      SymbolName("test"),
      Apply(Variable(SymbolName("f")), List(Variable(SymbolName("x")), StringLiteral("abc")))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("__tmp1"), BashExpressions.Literal("")),
        BashStatements.Command(BashExpressions.Literal("f"), List(
          BashExpressions.Literal("__tmp1"),
          BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("x"))),
          BashExpressions.Literal("abc")
        )),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("__tmp1"))))
      ))
    )

  def variableDeclPredefinedFunCall =
    VariableDeclaration(
      SymbolName("test"),
      Apply(Predefined(SymbolName("toInt")), List(Predefined(SymbolName("pi"))))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("__tmp1"), BashExpressions.Literal("3.141592653589793")),
        BashStatements.Assign(BashIdentifier("__tmp2"), BashExpressions.Eval(
          BashStatements.Command(BashExpressions.Literal("printf"), List(
            BashExpressions.Literal("%.0f"),
            BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("__tmp1")))
          )))),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("__tmp2"))))
      ))
    )

  def variableDeclStringExpr =
    VariableDeclaration(SymbolName("test"),
      BinaryOp(BinaryOperators.Add,
        StringLiteral("TEST"),
        ArrayAccess(SymbolName("strLst"), IntLiteral(2)))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Declare(Set(BashDeclareOptions.Array, BashDeclareOptions.ReadOnly), BashIdentifier("__tmp1"),
          Some(BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("strLst")), BashArrayIndices.All))),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.Interpolated(List(
          BashExpressions.Literal("TEST"),
          BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("__tmp1")), BashArrayIndices.Index(BashExpressions.Literal("2"))))))))
    )

  def variableDeclBoolExpr =
    VariableDeclaration(SymbolName("test"),
      BinaryOp(BinaryOperators.Or,
        BinaryOp(BinaryOperators.Less,
          Variable(SymbolName("x")),
          IntLiteral(10)),
        BinaryOp(BinaryOperators.Neq,
          Apply(Variable(SymbolName("f")), List(Variable(SymbolName("x")), StringLiteral("abc"))),
          StringLiteral("hello")
        )
      )) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("__tmp1"),
          BashExpressions.Literal("")),
        BashStatements.Command(BashExpressions.Literal("f"),List(
          BashExpressions.Literal("__tmp1"),
          BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("x"))),
          BashExpressions.Literal("abc")),None),
        BashStatements.Assign(BashIdentifier("__tmp2"),
          BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("__tmp1")))),
        BashStatements.Assign(BashIdentifier("__tmp3"),
          BashExpressions.Or(
            BashExpressions.Conditional(BashConditions.Less(BashConditions.Variable(BashVariables.Variable(BashIdentifier("x"))),BashConditions.Literal("10"))),
            BashExpressions.Conditional(BashConditions.NotEquals(BashConditions.Variable(BashVariables.Variable(BashIdentifier("__tmp2"))),BashConditions.Literal("hello"))))),
        BashStatements.Assign(BashIdentifier("test"),BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("__tmp3"))))))
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
