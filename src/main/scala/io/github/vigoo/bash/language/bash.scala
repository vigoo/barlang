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
  case class Local(options: Set[BashDeclareOption], name: BashIdentifier, initialValue: Option[BashExpression]) extends BashStatement
  case class Let(expression: List[BashArithmeticExpression]) extends BashStatement
  case class Function(name: BashIdentifier, body: BashStatement) extends BashStatement
  case class Eval(statement: BashStatement) extends BashStatement
  case class ArrayUpdate(target: BashIdentifier, index: BashExpression, value: BashExpression) extends BashStatement
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
  case object True extends BashExpression
  case object False extends BashExpression
  case class And(a: BashExpression, b: BashExpression) extends BashExpression
  case class Or(a: BashExpression, b: BashExpression) extends BashExpression
}


sealed trait BashVariable
object BashVariables {
  case class Variable(name: BashIdentifier) extends BashVariable
  case class Positional(index: Int) extends BashVariable
}

sealed trait BashOption
object BashOptions {
  case object AllExport extends BashOption
  case object BraceExpand extends BashOption
  case object Emacs extends BashOption
  case object ErrExit extends BashOption
  case object ErrTrace extends BashOption
  case object FuncTrace extends BashOption
  case object HashAll extends BashOption
  case object HistExpand extends BashOption
  case object History extends BashOption
  case object IgnoreEof extends BashOption
  case object Keyword extends BashOption
  case object Monitor extends BashOption
  case object NoClobber extends BashOption
  case object NoExec extends BashOption
  case object NoGlob extends BashOption
  case object NoLog extends BashOption
  case object Notify extends BashOption
  case object NoUnset extends BashOption
  case object OneCmd extends BashOption
  case object Physical extends BashOption
  case object PipeFail extends BashOption
  case object Posix extends BashOption
  case object Privileged extends BashOption
  case object Verbose extends BashOption
  case object Vi extends BashOption
  case object Xtrace extends BashOption
}

sealed trait BashCondition
object BashConditions {
  case class Literal(value: String) extends BashCondition
  case class Variable(variable: BashVariable) extends BashCondition

  case class StringEquals(a: BashCondition, b: BashCondition) extends BashCondition
  case class StringNotEquals(a: BashCondition, b: BashCondition) extends BashCondition
  case class LexicographicLess(a: BashCondition, b: BashCondition) extends BashCondition
  case class LexicographicGreater(a: BashCondition, b: BashCondition) extends BashCondition
  case class Equals(a: BashCondition, b: BashCondition) extends BashCondition
  case class NotEquals(a: BashCondition, b: BashCondition) extends BashCondition
  case class Greater(a: BashCondition, b: BashCondition) extends BashCondition
  case class GreaterEq(a: BashCondition, b: BashCondition) extends BashCondition
  case class Less(a: BashCondition, b: BashCondition) extends BashCondition
  case class LessEq(a: BashCondition, b: BashCondition) extends BashCondition

  case class Not(a: BashCondition) extends BashCondition
  case class And(a: BashCondition, b: BashCondition) extends BashCondition
  case class Or(a: BashCondition, b: BashCondition) extends BashCondition

  case class FileExists(a: BashCondition) extends BashCondition
  case class BlockFileExists(a: BashCondition) extends BashCondition
  case class CharacterFileExists(a: BashCondition) extends BashCondition
  case class DirectoryExists(a: BashCondition) extends BashCondition
  case class RegularFileExists(a: BashCondition) extends BashCondition
  case class FileExistsWithSetGroupId(a: BashCondition) extends BashCondition
  case class SymbolicLinkExists(a: BashCondition) extends BashCondition
  case class FileExistsWithStickyBit(a: BashCondition) extends BashCondition
  case class NamedPipeExists(a: BashCondition) extends BashCondition
  case class ReadableFileExists(a: BashCondition) extends BashCondition
  case class NonEmptyFileExists(a: BashCondition) extends BashCondition
  case class IsOpenTerminalFileDescriptor(a: BashCondition) extends BashCondition
  case class FileExistsWithSetUserId(a: BashCondition) extends BashCondition
  case class WriteableFileExists(a: BashCondition) extends BashCondition
  case class ExecutableFileExists(a: BashCondition) extends BashCondition
  case class FileExistsOwnedByEffectiveGroupId(a: BashCondition) extends BashCondition
  case class FileExistsModifiedSinceRead(a: BashCondition) extends BashCondition
  case class SocketExists(a: BashCondition) extends BashCondition

  case class SameDeviceAndInode(a: BashCondition, b: BashCondition) extends BashCondition
  case class NewerThan(a: BashCondition, b: BashCondition) extends BashCondition
  case class OlderThan(a: BashCondition, b: BashCondition) extends BashCondition
  case class OptionEnabled(option: BashOption) extends BashCondition
  case class VariableSet(variable: BashVariable) extends BashCondition
  case class NameReferenceSet(variable: BashVariable) extends BashCondition

  case class ZeroLengthString(a: BashCondition) extends BashCondition
  case class NonZeroLengthString(a: BashCondition) extends BashCondition
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