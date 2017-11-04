package io.github.vigoo.barlang.language

import scala.annotation.tailrec
import scala.util.parsing.input.Positional

case class SymbolName(name: String) extends AnyVal

case class TypeParam(name: SymbolName) extends Positional


sealed trait Type extends Positional

object Types {

  case class Unit() extends Type

  case class String() extends Type

  case class Bool() extends Type

  case class Int() extends Type

  case class Double() extends Type

  case class Function(typeParams: List[TypeParam], paramTypes: List[Type], returnType: Type) extends Type

  case class TypeVariable(name: SymbolName) extends Type

  case class Array(elementType: Type) extends Type

}


sealed trait UnaryOperator

object UnaryOperators {

  case object Not extends UnaryOperator

}


sealed trait BinaryOperator

object BinaryOperators {

  case object And extends BinaryOperator

  case object Or extends BinaryOperator

  case object Add extends BinaryOperator

  case object Sub extends BinaryOperator

  case object Mul extends BinaryOperator

  case object Div extends BinaryOperator

  case object Mod extends BinaryOperator

  case object Eq extends BinaryOperator

  case object Neq extends BinaryOperator

  case object Less extends BinaryOperator

  case object LessEq extends BinaryOperator

  case object Greater extends BinaryOperator

  case object GreaterEq extends BinaryOperator

}


sealed trait Expression extends Positional

object Expressions {

  case class StringLiteral(value: String) extends Expression

  case class BoolLiteral(value: Boolean) extends Expression

  case class IntLiteral(value: Int) extends Expression

  case class DoubleLiteral(value: Double) extends Expression

  case class Variable(name: SymbolName) extends Expression

  case class ArrayAccess(name: SymbolName, index: Expression) extends Expression

  case class SystemVariable(name: SymbolName) extends Expression

  case class Predefined(name: SymbolName) extends Expression

  case class Apply(function: Expression, parameters: List[Expression]) extends Expression

  case class UnaryOp(operator: UnaryOperator, x: Expression) extends Expression

  case class BinaryOp(operator: BinaryOperator, x: Expression, y: Expression) extends Expression

  case class Lambda(typeParams: List[TypeParam], paramDefs: List[ParamDef], returnType: Type, body: Statement) extends Expression

}


case class ParamDef(name: SymbolName, typ: Type) extends Positional


case class FunctionProperties(inline: Boolean)


sealed trait SingleStatement extends Positional

object SingleStatements {

  case class VariableDeclaration(name: SymbolName, value: Expression) extends SingleStatement

  case class FunctionDefinition(name: SymbolName, properties: FunctionProperties, typeParams: List[TypeParam], paramDefs: List[ParamDef], returnType: Type, body: Statement) extends SingleStatement

  case class Call(function: Expression, parameters: List[Expression]) extends SingleStatement

  case class Run(command: Expression, parameters: List[Expression]) extends SingleStatement

  case class If(condition: Expression, trueBody: Statement, falseBody: Statement) extends SingleStatement

  case class While(condition: Expression, body: Statement) extends SingleStatement

  case class UpdateVariable(name: SymbolName, value: Expression) extends SingleStatement

  case class UpdateCell(name: SymbolName, index: Expression, value: Expression) extends SingleStatement

  case class ArrayDeclaration(name: SymbolName, elementType: Type) extends SingleStatement

  case class Return(value: Expression) extends SingleStatement

}


sealed trait Statement extends Iterable[SingleStatement]

object Statement {
  def normalize(body: Statement): Vector[SingleStatement] = {
    import Statements._

    @tailrec
    def impl(body: Statement, result: Vector[SingleStatement]): Vector[SingleStatement] =
      body match {
        case Single(statement) =>
          result :+ statement
        case Sequence(first, second) =>
          impl(second, result :+ first)
        case NoOp => result
      }

    impl(body, Vector())
  }

  def fromSingleStatements(singleStatements: List[SingleStatement]): Statement = {
    singleStatements match {
      case Nil => Statements.NoOp
      case head :: Nil => Statements.Single(head)
      case head :: tail => Statements.Sequence(head, fromSingleStatements(tail))
    }
  }
}

object Statements {

  case class Single(statement: SingleStatement) extends Statement {
    override def iterator: Iterator[SingleStatement] = new Iterator[SingleStatement] {
      private var iterated = false
      override def hasNext: Boolean = !iterated
      override def next(): SingleStatement = {
        require(!iterated)
        iterated = true
        statement
      }
    }
  }

  case class Sequence(first: SingleStatement, second: Statement) extends Statement {
    override def iterator: Iterator[SingleStatement] = new Iterator[SingleStatement] {
      private var iterated = false
      private val nextIterator = second.iterator
      override def hasNext: Boolean = !iterated || nextIterator.hasNext
      override def next(): SingleStatement = {
        if (!iterated) {
          iterated = true
          first
        } else {
          nextIterator.next()
        }
      }
    }
  }

  case object NoOp extends Statement {
    override def iterator: Iterator[SingleStatement] = new Iterator[SingleStatement] {
      override def hasNext: Boolean = false
      override def next(): SingleStatement =
        throw new IllegalStateException()
    }
  }

}


case class Script(body: Statement)
