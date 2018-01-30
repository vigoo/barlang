package io.github.vigoo.barlang

import org.specs2._
import _root_.io.github.vigoo.barlang.language.{Type, _}
import _root_.io.github.vigoo.barlang.language.Expressions._
import _root_.io.github.vigoo.barlang.language.SingleStatements._
import _root_.io.github.vigoo.barlang.language.BinaryOperators._
import _root_.io.github.vigoo.barlang.language.UnaryOperators._
import _root_.io.github.vigoo.barlang.compiler.Compiler
import _root_.io.github.vigoo.barlang.compiler.Compiler.{AssignedSymbol, Context, GlobalScope, SimpleType}
import org.specs2.matcher.Matcher


class ExpressionTypeCheckingSpecs extends Specification { def is = s2"""
  Expression type checking works on
    String literal          ${StringLiteral("x") should haveType(Types.String())}
    Bool literal            ${BoolLiteral(true)  should haveType(Types.Bool())}
    Int literal             ${IntLiteral(11) should haveType(Types.Int())}
    Double literal          ${DoubleLiteral(0.11) should haveType(Types.Double())}
    Variable reference      ${Variable(SymbolName("x")) should haveTypeWith(exampleContext, Types.Int())}
    System variable         ${SystemVariable(SymbolName("PATH")) should haveType(Types.String())}
    String array indexing   ${ArrayAccess(SymbolName("stringArray"), IntLiteral(0)) should haveTypeWith(exampleContext, Types.String())}
    Int array indexing      ${ArrayAccess(SymbolName("intArray"), IntLiteral(0)) should haveTypeWith(exampleContext, Types.Int())}
    Function application    ${Apply(Variable(SymbolName("toInt")), List(DoubleLiteral(11))) should haveType(Types.Int())}

    Not operator            ${UnaryOp(Not, BoolLiteral(false)) should haveType(Types.Bool())}

    And operator            ${BinaryOp(And, BoolLiteral(true), BoolLiteral(true)) should haveType(Types.Bool())}
    Or operator             ${BinaryOp(Or, BoolLiteral(true), BoolLiteral(true)) should haveType(Types.Bool())}

    Add operator on ints    ${BinaryOp(Add, IntLiteral(1), IntLiteral(2)) should haveType(Types.Int())}
    Add operator on mixed 1 ${BinaryOp(Add, IntLiteral(1), DoubleLiteral(2.5)) should haveType(Types.Double())}
    Add operator on mixed 2 ${BinaryOp(Add, DoubleLiteral(2.5), IntLiteral(1)) should haveType(Types.Double())}
    Add operator on doubles ${BinaryOp(Add, DoubleLiteral(1), DoubleLiteral(2)) should haveType(Types.Double())}
    Add operator on strings ${BinaryOp(Add, StringLiteral("a"), StringLiteral("b")) should haveType(Types.String())}
    Sub operator on ints    ${BinaryOp(Sub, IntLiteral(1), IntLiteral(2)) should haveType(Types.Int())}
    Sub operator on mixed 1 ${BinaryOp(Sub, IntLiteral(1), DoubleLiteral(2.5)) should haveType(Types.Double())}
    Sub operator on mixed 2 ${BinaryOp(Sub, DoubleLiteral(2.5), IntLiteral(1)) should haveType(Types.Double())}
    Sub operator on doubles ${BinaryOp(Sub, DoubleLiteral(1), DoubleLiteral(2)) should haveType(Types.Double())}
    Mul operator on ints    ${BinaryOp(Mul, IntLiteral(1), IntLiteral(2)) should haveType(Types.Int())}
    Mul operator on mixed 1 ${BinaryOp(Mul, IntLiteral(1), DoubleLiteral(2.5)) should haveType(Types.Double())}
    Mul operator on mixed 2 ${BinaryOp(Mul, DoubleLiteral(2.5), IntLiteral(1)) should haveType(Types.Double())}
    Mul operator on doubles ${BinaryOp(Mul, DoubleLiteral(1), DoubleLiteral(2)) should haveType(Types.Double())}
    Div operator on ints    ${BinaryOp(Div, IntLiteral(1), IntLiteral(2)) should haveType(Types.Int())}
    Div operator on doubles ${BinaryOp(Div, DoubleLiteral(1), DoubleLiteral(2)) should haveType(Types.Double())}
    Mod operator on ints    ${BinaryOp(Mod, IntLiteral(1), IntLiteral(2)) should haveType(Types.Int())}

    Eq operator on bools    ${BinaryOp(Eq, BoolLiteral(true), BoolLiteral(true)) should haveType(Types.Bool())}
    Eq operator on ints     ${BinaryOp(Eq, IntLiteral(0), IntLiteral(0)) should haveType(Types.Bool())}
    Eq operator on doubles  ${BinaryOp(Eq, DoubleLiteral(0), DoubleLiteral(0)) should haveType(Types.Bool())}
    Eq operator on strings  ${BinaryOp(Eq, StringLiteral("x"), StringLiteral("y")) should haveType(Types.Bool())}

    Neq operator on bools   ${BinaryOp(Neq, BoolLiteral(true), BoolLiteral(true)) should haveType(Types.Bool())}
    Neq operator on ints    ${BinaryOp(Neq, IntLiteral(0), IntLiteral(0)) should haveType(Types.Bool())}
    Neq operator on doubles ${BinaryOp(Neq, DoubleLiteral(0), DoubleLiteral(0)) should haveType(Types.Bool())}
    Neq operator on strings ${BinaryOp(Neq, StringLiteral("x"), StringLiteral("y")) should haveType(Types.Bool())}

    LessEq op on ints       ${BinaryOp(LessEq, IntLiteral(0), IntLiteral(0)) should haveType(Types.Bool())}
    LessEq op on doubles    ${BinaryOp(LessEq, DoubleLiteral(0), DoubleLiteral(0)) should haveType(Types.Bool())}
    Less op on ints         ${BinaryOp(Less, IntLiteral(0), IntLiteral(0)) should haveType(Types.Bool())}
    Less op on doubles      ${BinaryOp(Less, DoubleLiteral(0), DoubleLiteral(0)) should haveType(Types.Bool())}
    GreaterEq op on ints    ${BinaryOp(GreaterEq, IntLiteral(0), IntLiteral(0)) should haveType(Types.Bool())}
    GreaterEq op on doubles ${BinaryOp(GreaterEq, DoubleLiteral(0), DoubleLiteral(0)) should haveType(Types.Bool())}
    Greater op on ints      ${BinaryOp(Greater, IntLiteral(0), IntLiteral(0)) should haveType(Types.Bool())}
    Greater op on doubles   ${BinaryOp(Greater, DoubleLiteral(0), DoubleLiteral(0)) should haveType(Types.Bool())}

    Lambda                  $lambdaTypeChecks

  Predefined values type check with
    pi is double           ${Predefined(SymbolName("pi")) should haveType(Types.Double())}
    toInt is double->int   ${Predefined(SymbolName("toInt")) should haveType(Types.Function(List.empty, List(Types.Double()), Types.Int()))}
    str is T->string       ${Predefined(SymbolName("str")) should haveType(Types.Function(List(TypeParam(SymbolName("T"))), List(Types.TypeVariable(SymbolName("T"))), Types.String()))}
    sin is double->double  ${Predefined(SymbolName("sin")) should haveType(Types.Function(List.empty, List(Types.Double()), Types.Double()))}
    cos is double->double  ${Predefined(SymbolName("cos")) should haveType(Types.Function(List.empty, List(Types.Double()), Types.Double()))}

  Expression type checking reports error on
    array indexing with non-integer                      ${ArrayAccess(SymbolName("stringArray"), StringLiteral("x")) should notTypeCheckWith(exampleContext)}
    function application of wrong function reference
    undefined function application
    function application with wrong number of parameters
    function application with wrong type of a parameter
    not operator called on non-boolean
    and operator called on non-boolean
    or operator called on non-boolean
  """

  // TODO: test type checking failure cases

  val exampleContext: Map[SymbolName, Type] = Map(
    SymbolName("x") -> Types.Int(),
    SymbolName("stringArray") -> Types.Array(Types.String()),
    SymbolName("intArray") -> Types.Array(Types.Int())
  )

  def lambdaTypeChecks =
    Lambda(List(TypeParam(SymbolName("T"))), List(ParamDef(SymbolName("x"), Types.TypeVariable(SymbolName("T")))), Types.Unit(), Statements.NoOp).should(haveType(
      Types.Function(List(TypeParam(SymbolName("T"))), List(Types.TypeVariable(SymbolName("T"))), Types.Unit())))

  protected def notTypeCheck: Matcher[Expression] = { (expression: Expression) =>
    Compiler.typeCheck(expression, Compiler.initialContext).should(beLeft)
  }

  protected def notTypeCheckWith(types: Map[SymbolName, Type]): Matcher[Expression] = { (expression: Expression) =>
    Compiler.typeCheck(expression, createCustomContext(types)).should(beLeft)
  }

  protected def haveType(expected: Type): Matcher[Expression] = { (expression: Expression) =>
    Compiler.typeCheck(expression, Compiler.initialContext).should(beRight(SimpleType(expected)))
  }

  protected def haveTypeWith(types: Map[SymbolName, Type], expected: Type): Matcher[Expression] = { (expression: Expression) =>
    val customContext: compiler.Compiler.Context = createCustomContext(types)
    Compiler.typeCheck(expression, customContext).should(beRight(SimpleType(expected)))
  }

  private def createCustomContext(types: Map[SymbolName, Type]): compiler.Compiler.Context = {
    val customContext = Context(
      scope = GlobalScope,
      symbols = types.map { case (name, _) => name -> AssignedSymbol(name, name.name) },
      symbolTypes = types.mapValues(SimpleType.apply),
      lastTmp = 0
    )
    customContext
  }
}
