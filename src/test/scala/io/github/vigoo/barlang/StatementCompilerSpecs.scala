package io.github.vigoo.barlang

import io.github.vigoo.barlang.compiler.Compiler._
import io.github.vigoo.barlang.language.BinaryOperators
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import io.github.vigoo.barlang.language.{FunctionProperties, ParamDef, SingleStatement, Statement, Statements, SymbolName, Type, Types, VariableProperties}
import io.github.vigoo.bash.language._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.matcher.Matcher

class StatementCompilerSpecs extends Specification {
  def is =
    s2"""
  The compiler compiles
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

    function definition with zero parameters           $funDefZeroParams
    function definition without params returning array $funDefReturnArray
    function definition with multiple parameters       $funDefMultiParams

    function call in statement position                $funCallStatement

    simple command execution                           $runStatement
  """

  val exampleContext: Map[SymbolName, Type] = Map(
    SymbolName("x") -> Types.Int(),
    SymbolName("f") -> Types.Function(List.empty, List(Types.Int(), Types.String()), Types.String()),
    SymbolName("g") -> Types.Function(List.empty, List(Types.String()), Types.Unit()),
    SymbolName("strLst") -> Types.Array(Types.String())
  )

  def variableDeclStringLit =
    VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false), StringLiteral("hello world")) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("hello world"))
    )

  def variableDeclBoolLit =
    (VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false), BoolLiteral(true)) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("0"))
    )) and (VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false), BoolLiteral(false)) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("1"))
    ))

  def variableDeclOtherVariable =
    VariableDeclaration(SymbolName("y"), VariableProperties(mutable = false), Variable(SymbolName("x"))) must compileTo(
      BashStatements.Assign(BashIdentifier("y"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("x"))))
    )

  def variableDeclSysVar =
    VariableDeclaration(SymbolName("path2"), VariableProperties(mutable = false), SystemVariable(SymbolName("PATH"))) must compileTo(
      BashStatements.Assign(BashIdentifier("path2"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("PATH"))))
    )

  def variableDeclFunctionRef =
    VariableDeclaration(SymbolName("fn"), VariableProperties(mutable = false), Variable(SymbolName("f"))) must compileTo(
      BashStatements.Assign(BashIdentifier("fn"), BashExpressions.Literal("f"))
    )

  def variableDeclArrayElem =
    VariableDeclaration(SymbolName("second"), VariableProperties(mutable = false), ArrayAccess(SymbolName("strLst"), IntLiteral(1))) must compileTo(
      BashStatements.Sequence(List(
        BashStatements.Declare(
          Set(BashDeclareOptions.Array, BashDeclareOptions.ReadOnly),
          BashIdentifier("_tmp1"),
          Some(BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("strLst")), BashArrayIndices.All))
        ),
        BashStatements.Assign(
          BashIdentifier("second"),
          BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("_tmp1")),
            BashArrayIndices.Index(BashExpressions.Literal("1"))))
      ))
    )

  def variableDeclIntLit =
    VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false), IntLiteral(10)) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("10"))
    )

  def variableDeclDoubleLit =
    VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false), DoubleLiteral(0.1)) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal("0.1"))
    )

  def variableDeclDoubleExpr =
    VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false),
      BinaryOp(BinaryOperators.Add,
        DoubleLiteral(1.0),
        BinaryOp(BinaryOperators.Div,
          Variable(SymbolName("x")),
          IntLiteral(2)))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("_tmp1"), BashExpressions.Eval(
          BashStatements.Command(
            BashExpressions.Literal("bc"),
            List(BashExpressions.Literal("-l")),
            Some(BashExpressions.Literal("(1.0) + (($x) / (2.0))")))
        )),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("_tmp1"))))))
    )

  def variableDeclIntegerExpr =
    VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false),
      BinaryOp(BinaryOperators.Add,
        IntLiteral(1),
        BinaryOp(BinaryOperators.Div,
          Variable(SymbolName("x")),
          IntLiteral(2)))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("_tmp1"), BashExpressions.EvalArithmetic(
          BashArithmeticExpressions.Add(
            BashArithmeticExpressions.Number(1),
            BashArithmeticExpressions.Div(
              BashArithmeticExpressions.Variable(BashVariables.Variable(BashIdentifier("x"))),
              BashArithmeticExpressions.Number(2)
            )
          )
        )),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("_tmp1"))))))
    )

  def variableDeclPredefConst =
    VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false), Predefined(SymbolName("pi"))) must compileTo(
      BashStatements.Assign(BashIdentifier("test"), BashExpressions.Literal(Math.PI.toString))
    )

  def variableDeclFunCall =
    VariableDeclaration(
      SymbolName("test"), VariableProperties(mutable = false),
      Apply(Variable(SymbolName("f")), List(Variable(SymbolName("x")), StringLiteral("abc")))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("_tmp1"), BashExpressions.Literal("")),
        BashStatements.Command(BashExpressions.Literal("f"), List(
          BashExpressions.Literal("_tmp1"),
          BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("x"))),
          BashExpressions.Literal("abc")
        )),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("_tmp1"))))
      ))
    )

  def variableDeclPredefinedFunCall =
    VariableDeclaration(
      SymbolName("test"), VariableProperties(mutable = false),
      Apply(Predefined(SymbolName("toInt")), List(Predefined(SymbolName("pi"))))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Assign(BashIdentifier("_tmp1"), BashExpressions.Literal("3.141592653589793")),
        BashStatements.Assign(BashIdentifier("_tmp2"), BashExpressions.Eval(
          BashStatements.Command(BashExpressions.Literal("printf"), List(
            BashExpressions.Literal("%.0f"),
            BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("_tmp1")))
          )))),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("_tmp2"))))
      ))
    )

  def variableDeclStringExpr =
    VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false),
      BinaryOp(BinaryOperators.Add,
        StringLiteral("TEST"),
        ArrayAccess(SymbolName("strLst"), IntLiteral(2)))) must compileTo(

      BashStatements.Sequence(List(
        BashStatements.Declare(Set(BashDeclareOptions.Array, BashDeclareOptions.ReadOnly), BashIdentifier("_tmp1"),
          Some(BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("strLst")), BashArrayIndices.All))),
        BashStatements.Assign(BashIdentifier("test"), BashExpressions.Interpolated(List(
          BashExpressions.Literal("TEST"),
          BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("_tmp1")), BashArrayIndices.Index(BashExpressions.Literal("2"))))))))
    )

  def variableDeclBoolExpr =
    VariableDeclaration(SymbolName("test"), VariableProperties(mutable = false),
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
        BashStatements.Assign(BashIdentifier("_tmp1"),
          BashExpressions.Literal("")),
        BashStatements.Command(BashExpressions.Literal("f"),List(
          BashExpressions.Literal("_tmp1"),
          BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("x"))),
          BashExpressions.Literal("abc")),None),
        BashStatements.Assign(BashIdentifier("_tmp2"),
          BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("_tmp1")))),
        BashStatements.Assign(BashIdentifier("_tmp3"),
          BashExpressions.Or(
            BashExpressions.Conditional(BashConditions.Less(BashConditions.Variable(BashVariables.Variable(BashIdentifier("x"))),BashConditions.Literal("10"))),
            BashExpressions.Conditional(BashConditions.StringNotEquals(BashConditions.Variable(BashVariables.Variable(BashIdentifier("_tmp2"))),BashConditions.Literal("hello"))))),
        BashStatements.Assign(BashIdentifier("test"),BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("_tmp3"))))))
    )

  def funDefZeroParams =
    FunctionDefinition(
      SymbolName("testfn0"),
      FunctionProperties(inline = false),
      List.empty,
      List.empty,
      Types.Int(),
      Statements.Single(
        Return(IntLiteral(42))
      )
    ) must compileTo(
      BashStatements.Function(
        BashIdentifier("testfn0"),
        BashStatements.Sequence(List(
          BashStatements.Local(Set.empty, BashIdentifier("testfn0__retvar"), Some(BashExpressions.ReadVariable(BashVariables.Positional(1)))),
          BashStatements.Assign(BashIdentifier("testfn0__tmp1"), BashExpressions.Literal("42")),
          BashStatements.Eval(
            BashStatements.Assign(BashIdentifier("testfn0__retvar"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("testfn0__tmp1")))))
        ))
      )
    )

  def funDefReturnArray =
    FunctionDefinition(
      SymbolName("testfn1"),
      FunctionProperties(inline = false),
      List.empty,
      List.empty,
      Types.Array(Types.String()),
      Statement.fromSingleStatements(List(
        ArrayDeclaration(SymbolName("result"), Types.String()),
        UpdateCell(SymbolName("result"), IntLiteral(0), StringLiteral("hello")),
        Return(Variable(SymbolName("result")))
      ))
    ) must compileTo(
      BashStatements.Function(
        BashIdentifier("testfn1"),
        BashStatements.Sequence(List(
          BashStatements.Local(Set.empty, BashIdentifier("testfn1__retvar"), Some(BashExpressions.ReadVariable(BashVariables.Positional(1)))),
          BashStatements.Declare(Set(BashDeclareOptions.Array), BashIdentifier("testfn1_result"), None),
          BashStatements.ArrayUpdate(BashIdentifier("testfn1_result"), BashExpressions.Literal("0"), BashExpressions.Literal("hello")),
          BashStatements.Eval(
            BashStatements.Assign(BashIdentifier("testfn1__retvar"), BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("testfn1_result")), BashArrayIndices.All)))
        ))
      )
    )

  def funDefMultiParams =
    FunctionDefinition(
      SymbolName("testfn2"),
      FunctionProperties(inline = true),
      List.empty,
      List(
        ParamDef(SymbolName("x"), Types.Int()),
        ParamDef(SymbolName("y"), Types.Int())),
      Types.Int(),
      Statements.Single(
        Return(BinaryOp(BinaryOperators.Mul, Variable(SymbolName("x")), Variable(SymbolName("y"))))
      )
    ) must compileTo(
      BashStatements.Function(
        BashIdentifier("testfn2"),
        BashStatements.Sequence(List(
          BashStatements.Local(Set.empty, BashIdentifier("testfn2__retvar"), Some(BashExpressions.ReadVariable(BashVariables.Positional(1)))),
          BashStatements.Local(Set(BashDeclareOptions.ReadOnly), BashIdentifier("testfn2_x"), Some(BashExpressions.ReadVariable(BashVariables.Positional(2)))),
          BashStatements.Local(Set(BashDeclareOptions.ReadOnly), BashIdentifier("testfn2_y"), Some(BashExpressions.ReadVariable(BashVariables.Positional(3)))),
          BashStatements.Assign(BashIdentifier("testfn2__tmp1"), BashExpressions.EvalArithmetic(
            BashArithmeticExpressions.Mul(
              BashArithmeticExpressions.Variable(BashVariables.Variable(BashIdentifier("testfn2_x"))),
              BashArithmeticExpressions.Variable(BashVariables.Variable(BashIdentifier("testfn2_y")))))),
          BashStatements.Assign(BashIdentifier("testfn2__tmp2"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("testfn2__tmp1")))),
          BashStatements.Eval(
            BashStatements.Assign(BashIdentifier("testfn2__retvar"), BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("testfn2__tmp2")))))
        ))
      )
    )

  def funCallStatement =
    Call(
      StringLiteral("g"),
      List(StringLiteral("abc"))) must compileTo(

      BashStatements.Command(BashExpressions.Literal("g"), List(
          BashExpressions.Literal("abc")
        )))

  def runStatement =
    Run(
      StringLiteral("grep"),
      List(StringLiteral("x"), StringLiteral("y.log"))) must compileTo(

      BashStatements.Command(
        BashExpressions.Literal("grep"), List(
          BashExpressions.Literal("x"),
          BashExpressions.Literal("y.log")
      )))

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
      mutability = types.map { case (name, _) => name -> false },
      lastTmp = 0
    )
    customContext
  }
}
