package io.github.vigoo.bash.language


case class BashIdentifier(name: String)


sealed trait BashStatement {
  def flatten: List[BashStatement] = List(this)
  def normalize: BashStatement = this
}
object BashStatements {
  case object Nop extends BashStatement
  case class Assign(target: BashIdentifier, expression: BashExpression) extends BashStatement
  case class Command(name: BashExpression, params: List[BashExpression], hereString: Option[BashExpression] = None) extends BashStatement
  case class IfThenElse(conditional: BashExpression, onTrue: BashStatement, onFalse: BashStatement) extends BashStatement
  case class Declare(options: Set[BashDeclareOption], name: BashIdentifier, initialValue: Option[BashExpression]) extends BashStatement
  case class Let(expression: List[BashArithmeticExpression]) extends BashStatement
  case class Sequence(statements: List[BashStatement]) extends BashStatement {
    override def flatten: List[BashStatement] =
      statements.flatMap(_.flatten)

    override def normalize: BashStatement =
      flatten match {
        case Nil => Nop
        case List(statement) => statement
        case ss: List[BashStatement] => Sequence(ss)
      }
  }
}


sealed trait BashExpression
object BashExpressions {
  case class Literal(lit: String) extends BashExpression
  case class ReadVariable(variable: BashVariable) extends BashExpression
  case class ReadArray(variable: BashVariable, index: BashArrayIndex) extends BashExpression
  case class Eval(statement: BashStatement) extends BashExpression
  case class Conditional(condition: BashCondition) extends BashExpression
  case class Interpolated(parts: List[BashExpression]) extends BashExpression
  case class EvalArithmetic(expression: BashArithmeticExpression) extends BashExpression
}


sealed trait BashVariable
object BashVariables {
  case class Variable(name: BashIdentifier) extends BashVariable
}


sealed trait BashCondition
object BashConditions {
  case class Equals(a: BashExpression, b: BashExpression) extends BashCondition
}

sealed trait BashArrayIndex
object BashArrayIndices {
  case class Index(index: BashExpression) extends BashArrayIndex
  case object All extends BashArrayIndex
}

sealed trait BashDeclareOption
object BashDeclareOptions {
  case object Array extends BashDeclareOption
  case object ReadOnly extends BashDeclareOption
}


sealed trait BashArithmeticExpression
object BashArithmeticExpressions {
  case class Number(value: Int) extends BashArithmeticExpression // n
  case class Variable(variable: BashVariable) extends BashArithmeticExpression // $X
  case class PostIncrement(x: BashArithmeticExpression) extends BashArithmeticExpression // x++
  case class PostDecrement(x: BashArithmeticExpression) extends BashArithmeticExpression // x--
  case class PreIncrement(x: BashArithmeticExpression) extends BashArithmeticExpression // ++x
  case class PreDecrement(x: BashArithmeticExpression) extends BashArithmeticExpression // --x
  case class Minus(x: BashArithmeticExpression) extends BashArithmeticExpression // -x
  case class Plus(x: BashArithmeticExpression) extends BashArithmeticExpression // +x
  case class LogicalNot(x: BashArithmeticExpression) extends BashArithmeticExpression // !x
  case class BitwiseNot(x: BashArithmeticExpression) extends BashArithmeticExpression // ~x
  case class Exponentiation(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x ** y
  case class Add(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x + y
  case class Sub(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x - y
  case class Mul(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x * y
  case class Div(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x / y
  case class Rem(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x % y
  case class BitwiseLeftShift(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x << y
  case class BitwiseRightShift(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x >> y
  case class LessEq(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x <= y
  case class Less(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x < y
  case class Greater(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x > y
  case class GreaterEq(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x >= y
  case class Equal(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x == y
  case class NotEqual(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x != y
  case class BitwiseAnd(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x & y
  case class BitwiseXor(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x ^ y
  case class BitwiseOr(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x | y
  case class LogicalAnd(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x && y
  case class LogicalOr(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x || y
  case class Conditional(condition: BashArithmeticExpression, trueCase: BashArithmeticExpression, falseCase: BashArithmeticExpression)
    extends BashArithmeticExpression // condition ? trueCase : falseCase
  case class Assign(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x = y
  case class AssignMul(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x *= y
  case class AssignDiv(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x /= y
  case class AssignRem(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x %= y
  case class AssignAdd(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x += y
  case class AssignSub(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x -= y
  case class AssignShiftLeft(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x <<= y
  case class AssignShiftRight(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x >>= y
  case class AssignAnd(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x &= y
  case class AssignOr(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x |= y
  case class AssignXor(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x ^= y
  case class Comma(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x, y
}