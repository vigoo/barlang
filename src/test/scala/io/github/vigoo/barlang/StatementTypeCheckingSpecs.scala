package io.github.vigoo.barlang

import io.github.vigoo.barlang.compiler.Compiler
import io.github.vigoo.barlang.compiler.Compiler._
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import io.github.vigoo.barlang.language._
import org.specs2.Specification
import org.specs2.matcher.Matcher

class StatementTypeCheckingSpecs extends Specification { def is = s2"""
  Statement type checking works on
    variable declaration   ${VariableDeclaration(SymbolName("y"), VariableProperties(mutable = false), StringLiteral("z")) should haveType(Types.Unit())}
    function definition    ${emptyFunDef should haveType(Types.Unit())}
    function call          ${complexCall should haveTypeWith(exampleContext, Types.Unit())}
    run external command   ${Run(StringLiteral("echo"), List(StringLiteral("hello world"))) should haveType(Types.Unit())}
    if-then-else           ${If(BoolLiteral(true), Statements.NoOp, Statements.NoOp) should haveType(Types.Unit())}
    while                  ${While(BoolLiteral(true), Statements.NoOp) should haveType(Types.Unit())}
    update variable        ${UpdateVariable(SymbolName("x"), IntLiteral(10)) should haveTypeWith(exampleContext, Types.Unit())}
    update cell            ${UpdateCell(SymbolName("strLst"), Variable(SymbolName("x")), StringLiteral("y")) should haveTypeWith(exampleContext, Types.Unit())}
    array declaration      ${ArrayDeclaration(SymbolName("intLst"), Types.Int()) should haveType(Types.Unit())}
    return                 ${Return(IntLiteral(0)) should haveType(Types.Int())}

  Statement type checking reports error on
    function call with invalid function refernece    ${invalidFunctionRef should notTypeCheck}
    function call with undefined function refernece  ${undefinedFunctionRef should notTypeCheck}
    function call with wrong parameter count         ${wrongParameterCount should notTypeCheckWith(exampleContext)}
    function call with wrong parameter types         ${wrongParameterTypes should notTypeCheckWith(exampleContext)}
    run external command with non-string expressions ${Run(StringLiteral("echo"), List(IntLiteral(0))) should notTypeCheck}
    if-then-else with non-boolean condition          ${If(StringLiteral("true"), Statements.NoOp, Statements.NoOp) should notTypeCheck}
    while with non-boolean condition                 ${While(IntLiteral(0), Statements.NoOp) should notTypeCheck}
    update variable with mismatching type            ${UpdateVariable(SymbolName("x"), DoubleLiteral(0.5)) should notTypeCheckWith(exampleContext)}
    update cell with non-integer index               ${UpdateCell(SymbolName("strLst"), StringLiteral("a"), StringLiteral("b")) should notTypeCheckWith(exampleContext)}
    update cell of non-array type                    ${UpdateCell(SymbolName("x"), IntLiteral(0), StringLiteral("c")) should notTypeCheckWith(exampleContext)}
    update cell with non-matching elem type          ${UpdateCell(SymbolName("strLst"), IntLiteral(0), DoubleLiteral(0.1)) should notTypeCheckWith(exampleContext)}
  """

  val exampleContext: Map[SymbolName, Type] = Map(
    SymbolName("x") -> Types.Int(),
    SymbolName("f") -> Types.Function(List.empty, List(Types.Int(), Types.String()), Types.String()),
    SymbolName("g") -> Types.Function(List.empty, List(Types.String()), Types.Unit()),
    SymbolName("strLst") -> Types.Array(Types.String())
  )

  private val emptyFunDef = FunctionDefinition(
    SymbolName("emptyFn"),
    FunctionProperties(inline = false),
    typeParams = List.empty,
    paramDefs = List.empty,
    returnType = Types.String(),
    body = Statements.Single(Return(StringLiteral("test")))
  )

  private val invalidFunctionRef = Call(IntLiteral(10), List.empty)
  private val undefinedFunctionRef = Call(Variable(SymbolName("unknown")), List.empty)
  private val wrongParameterCount = Call(Variable(SymbolName("g")), List(StringLiteral("x"), IntLiteral(0)))
  private val wrongParameterTypes = Call(Variable(SymbolName("g")), List(DoubleLiteral(0.5)))

  private val complexCall = Call(Variable(SymbolName("g")), List(Apply(Variable(SymbolName("f")), List(Variable(SymbolName("x")), StringLiteral("test")))))

  protected def notTypeCheck: Matcher[SingleStatement] = { (statement: SingleStatement) =>
    Compiler.typeCheck(statement, Compiler.initialContext).should(beLeft)
  }

  protected def notTypeCheckWith(types: Map[SymbolName, Type]): Matcher[SingleStatement] = { (statement: SingleStatement) =>
    Compiler.typeCheck(statement, createCustomContext(types)).should(beLeft)
  }

  protected def haveType(expected: Type): Matcher[SingleStatement] = { (statement: SingleStatement) =>
    Compiler.typeCheck(statement, Compiler.initialContext).should(beRight(SimpleType(expected)))
  }

  protected def haveTypeWith(types: Map[SymbolName, Type], expected: Type): Matcher[SingleStatement] = { (statement: SingleStatement) =>
    val customContext: compiler.Compiler.Context = createCustomContext(types)
    Compiler.typeCheck(statement, customContext).should(beRight(SimpleType(expected)))
  }

  private def createCustomContext(types: Map[SymbolName, Type]): compiler.Compiler.Context = {
    val customContext = Context(
      scope = GlobalScope,
      symbols = types.map { case (name, _) => name -> AssignedSymbol(name, name.name) },
      symbolTypes = types.mapValues(SimpleType.apply),
      mutability = types.map { case (name, _) => name -> false },
      lastTmp = 0
    )
    customContext
  }
}
