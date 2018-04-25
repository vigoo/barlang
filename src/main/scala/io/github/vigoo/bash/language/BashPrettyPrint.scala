package io.github.vigoo.bash.language

import cats.data.State
import cats.implicits._
import io.github.vigoo.bash.language.BashConditions.StringEquals
import io.github.vigoo.bash.language.BashExpressions._
import io.github.vigoo.bash.language.BashStatements._
import io.github.vigoo.bash.language.BashVariables.Variable
import io.github.vigoo.simpp.PrettyPrint.PrettyPrinterContext
import io.github.vigoo.simpp.{PrettyPrint, PrettyPrinter}
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

case class BashPrettyPrinterState(inString: Boolean)

object BashPrettyPrint extends PrettyPrint[Fx.fx1[State[BashPrettyPrinterState, ?]]] {
  override def runAdditionalFx(f: Eff[BashPrettyPrint.R, Unit]): Eff[PrettyPrinterContext[NoFx], Unit] =
    f.evalState(BashPrettyPrinterState(inString = false))

  type BashFx = Fx.fx1[State[BashPrettyPrinterState, ?]]
  type PPrinter[A] = PrettyPrinter[A, BashFx]

  def getBashState: Eff[R, BashPrettyPrinterState] =
    get[R, BashPrettyPrinterState]

  def setBashState(newState: BashPrettyPrinterState): Eff[R, Unit] =
    put[R, BashPrettyPrinterState](newState)

  def inString[A](inner: Eff[R, A]): Eff[R, A] =
    for {
      state <- getBashState
      _ <- setBashState(state.copy(inString = true))
      result <- inner
      _ <- setBashState(state.copy(inString = state.inString))
    } yield result

  implicit val stringPrettyPrinter: PPrinter[String] =
    (value: String) => code(value)

  implicit val bashDeclareOptionPrettyPrinter: PPrinter[BashDeclareOption] = {
    case BashDeclareOptions.Array => code("-a")
    case BashDeclareOptions.ReadOnly => code("-r")
  }

  implicit val bashStatementPrettyPrinter: PPrinter[BashStatement] = {
    case Nop =>
      empty
    case Assign(target, expression) =>
      pretty(target) >> code("=") >> pretty(expression)
    case Command(name, params, None) =>
      pretty(sequence(name :: params, separator = " "))
    case Command(name, params, Some(hereString)) =>
      pretty(sequence(name :: (params ::: List(BashExpressions.Literal("<<<"), hereString)), separator = " "))
    case IfThenElse(conditional, onTrue, onFalse) =>
      code("if") >> space >> pretty(conditional) >> newline >>
        code("then") >> newline >>
        indented(pretty(onTrue)) >> newline >>
        code("else") >> newline >>
        indented(pretty(onFalse)) >> newline >>
        code("fi")
    case Declare(options, identifier, optionalInitialValue) =>
      val prefix = code("declare") >> space >> pretty(sequence(options.toList, separator = " ")) >> space >> code(identifier.name)
      optionalInitialValue match {
        case Some(initialValue) =>
          prefix >> code("=") >> pretty(initialValue)
        case None =>
          prefix
      }
    case Let(expressions) =>
      code("let") >> space >> pretty(sequence(expressions.map(expr => doubleQuoted(expr)), separator = " "))

    case Sequence(statements) =>
      pretty(sequence(statements, "\n"))
  }

  implicit val bashIdentifierPrettyPrinter: PPrinter[BashIdentifier] =
    (identifier: BashIdentifier) => code(identifier.name)

  implicit val bashVariablePrettyPrinter: PPrinter[BashVariable] = {
    case Variable(name) => pretty(name)
  }

  implicit val bashArrayIndexPrettyPrinter: PPrinter[BashArrayIndex] = {
    case BashArrayIndices.All => code("*")
    case BashArrayIndices.Index(index) => pretty(index)
  }

  implicit val bashExpressionPrettyPrinter: PPrinter[BashExpression] = {
    case Literal(lit) =>
      getBashState.flatMap { state =>
        if ((lit.exists(_.isWhitespace) || lit.isEmpty) && !state.inString) {
          doubleQuoted(lit)
        } else {
          code(lit)
        }
      }

    case ReadVariable(variable) =>
      val prettyVariable = print(variable)
      if (prettyVariable.length > 1) {
        dollar >> curlyBracketed(prettyVariable)
      } else {
        dollar >> code(prettyVariable)
      }
    case ReadArray(variable, index) =>
      dollar >> curlyBracketed(pretty(variable) >> squareBracketed(index))

    case Eval(statement) => between("$(", ")", statement)
    case Conditional(condition) => between("[[ ", " ]]", condition)
    case Interpolated(parts) => doubleQuoted(inString(pretty(sequence(parts))))
    case EvalArithmetic(expression) => between("$(( ", " ))", expression)
    case True => code("true")
    case False => code("false")
    case And(a, b) => pretty(a) >> space >> code("&&") >> space >> pretty(b)
    case Or(a, b) => pretty(a) >> space >> code("||") >> space >> pretty(b)
  }

  implicit val bashOptionPrettyPrinter: PPrinter[BashOption] = {
    case BashOptions.AllExport => code("allexport")
    case BashOptions.BraceExpand => code("braceexpand")
    case BashOptions.Emacs => code("emacs")
    case BashOptions.ErrExit => code("errexit")
    case BashOptions.ErrTrace => code("errtrace")
    case BashOptions.FuncTrace => code("functrace")
    case BashOptions.HashAll => code("hashall")
    case BashOptions.HistExpand => code("histexpand")
    case BashOptions.History => code("history")
    case BashOptions.IgnoreEof => code("ignoreeof")
    case BashOptions.Keyword => code("keyword")
    case BashOptions.Monitor => code("monitor")
    case BashOptions.NoClobber => code("noclobber")
    case BashOptions.NoExec => code("noexec")
    case BashOptions.NoGlob => code("noglob")
    case BashOptions.NoLog => code("nolog")
    case BashOptions.Notify => code("notify")
    case BashOptions.NoUnset => code("nounset")
    case BashOptions.OneCmd => code("onecmd")
    case BashOptions.Physical => code("physical")
    case BashOptions.PipeFail => code("pipefail")
    case BashOptions.Posix => code("posix")
    case BashOptions.Privileged => code("privileged")
    case BashOptions.Verbose => code("verbose")
    case BashOptions.Vi => code("vi")
    case BashOptions.Xtrace => code("xtrace")
  }

  private def binaryConditionalOp(a: BashCondition, b: BashCondition, op: String): PP[Unit] =
    pretty(a) >> space >> code(op) >> space >> pretty(b)

  private def unaryConditionalOp(a: BashCondition, op: String): PP[Unit] =
    code(op) >> space >> pretty(a)

  implicit val bashConditionPrettyPrinter: PPrinter[BashCondition] = {
    case BashConditions.Literal(value) => pretty(Literal(value))
    case BashConditions.Variable(variable) => pretty(ReadVariable(variable))

    case BashConditions.StringEquals(a, b) => binaryConditionalOp(a, b, "==")
    case BashConditions.StringNotEquals(a, b) => binaryConditionalOp(a, b, "!=")
    case BashConditions.LexicographicLess(a, b) => binaryConditionalOp(a, b, "<")
    case BashConditions.LexicographicGreater(a, b) => binaryConditionalOp(a, b, ">")
    case BashConditions.Equals(a, b) => binaryConditionalOp(a, b, "-eq")
    case BashConditions.NotEquals(a, b) => binaryConditionalOp(a, b, "-ne")
    case BashConditions.Greater(a, b) => binaryConditionalOp(a, b, "-gt")
    case BashConditions.GreaterEq(a, b) => binaryConditionalOp(a, b, "-ge")
    case BashConditions.Less(a, b) => binaryConditionalOp(a, b, "-lt")
    case BashConditions.LessEq(a, b) => binaryConditionalOp(a, b, "-le")

    case BashConditions.Not(a) => unaryConditionalOp(a, "!")
    case BashConditions.And(a, b) => binaryConditionalOp(a, b, "&&")
    case BashConditions.Or(a, b) => binaryConditionalOp(a, b, "||")

    case BashConditions.FileExists(a) => unaryConditionalOp(a, "-a")
    case BashConditions.BlockFileExists(a) => unaryConditionalOp(a, "-b")
    case BashConditions.CharacterFileExists(a) => unaryConditionalOp(a, "-c")
    case BashConditions.DirectoryExists(a) => unaryConditionalOp(a, "-d")
    case BashConditions.RegularFileExists(a) => unaryConditionalOp(a, "-f")
    case BashConditions.FileExistsWithSetGroupId(a) => unaryConditionalOp(a, "-g")
    case BashConditions.SymbolicLinkExists(a) => unaryConditionalOp(a, "-h")
    case BashConditions.FileExistsWithStickyBit(a) => unaryConditionalOp(a, "-k")
    case BashConditions.NamedPipeExists(a) => unaryConditionalOp(a, "-p")
    case BashConditions.ReadableFileExists(a) => unaryConditionalOp(a, "-r")
    case BashConditions.NonEmptyFileExists(a) => unaryConditionalOp(a, "-s")
    case BashConditions.IsOpenTerminalFileDescriptor(a) => unaryConditionalOp(a, "-t")
    case BashConditions.FileExistsWithSetUserId(a) => unaryConditionalOp(a, "-u")
    case BashConditions.WriteableFileExists(a) => unaryConditionalOp(a, "-w")
    case BashConditions.ExecutableFileExists(a) => unaryConditionalOp(a, "-x")
    case BashConditions.FileExistsOwnedByEffectiveGroupId(a) => unaryConditionalOp(a, "-G")
    case BashConditions.FileExistsModifiedSinceRead(a) => unaryConditionalOp(a, "-N")
    case BashConditions.SocketExists(a) => unaryConditionalOp(a, "-S")

    case BashConditions.SameDeviceAndInode(a, b) => binaryConditionalOp(a, b, "-ef")
    case BashConditions.NewerThan(a, b) => binaryConditionalOp(a, b, "-nt")
    case BashConditions.OlderThan(a, b) => binaryConditionalOp(a, b, "-ot")
    case BashConditions.OptionEnabled(option) => code("-o") >> space >> pretty(option)
    case BashConditions.VariableSet(variable) => code("-v") >> space >> pretty(variable)
    case BashConditions.NameReferenceSet(variable) => code("-R") >> space >> pretty(variable)

    case BashConditions.ZeroLengthString(a) => unaryConditionalOp(a, "-z")
    case BashConditions.NonZeroLengthString(a) => unaryConditionalOp(a, "-n")
  }

  implicit val bashArithmeticExpressionPrettyPrinter: PPrinter[BashArithmeticExpression] = expr => {

    def needParentheses(e: BashArithmeticExpression): Boolean = e match {
      case BashArithmeticExpressions.Number(_) => false
      case BashArithmeticExpressions.Variable(_) => false
      case _ => true
    }

    def parenthesedExpression(x: BashArithmeticExpression): PP[Unit] = {
      if (needParentheses(x)) {
        parenthesed(pretty(x))
      } else {
        pretty(x)
      }
    }

    def binary(x: BashArithmeticExpression, y: BashArithmeticExpression, op: String): PP[Unit] = {
      parenthesedExpression(x) >> space >> code(op) >> space >> parenthesedExpression(y)
    }

    def assignment(x: BashVariable, y: BashArithmeticExpression, op: String): PP[Unit] = {
      pretty(x) >> space >> code(op) >> space >> parenthesedExpression(y)
    }

    expr match {
      case BashArithmeticExpressions.Number(value) => code(value.toString)
      case BashArithmeticExpressions.Variable(variable) => pretty(ReadVariable(variable))
      case BashArithmeticExpressions.PostIncrement(x) => parenthesedExpression(x) >> code("++")
      case BashArithmeticExpressions.PostDecrement(x) => parenthesedExpression(x) >> code("--")
      case BashArithmeticExpressions.PreIncrement(x) => code("++") >> parenthesedExpression(x)
      case BashArithmeticExpressions.PreDecrement(x) => code("--") >> parenthesedExpression(x)
      case BashArithmeticExpressions.Minus(x) => code("-") >> parenthesedExpression(x)
      case BashArithmeticExpressions.Plus(x) => code("+") >> parenthesedExpression(x)
      case BashArithmeticExpressions.LogicalNot(x) => code("!") >> parenthesedExpression(x)
      case BashArithmeticExpressions.BitwiseNot(x) => code("~") >> parenthesedExpression(x)
      case BashArithmeticExpressions.Exponentiation(x, y) => binary(x, y, "**")
      case BashArithmeticExpressions.Add(x, y) => binary(x, y, "+")
      case BashArithmeticExpressions.Sub(x, y) => binary(x, y, "-")
      case BashArithmeticExpressions.Mul(x, y) => binary(x, y, "*")
      case BashArithmeticExpressions.Div(x, y) => binary(x, y, "/")
      case BashArithmeticExpressions.Rem(x, y) => binary(x, y, "%")
      case BashArithmeticExpressions.BitwiseLeftShift(x, y) => binary(x, y, "<<")
      case BashArithmeticExpressions.BitwiseRightShift(x, y) => binary(x, y, ">>")
      case BashArithmeticExpressions.LessEq(x, y) => binary(x, y, "<=")
      case BashArithmeticExpressions.Less(x, y) => binary(x, y, "<")
      case BashArithmeticExpressions.Greater(x, y) => binary(x, y, ">")
      case BashArithmeticExpressions.GreaterEq(x, y) => binary(x, y, ">=")
      case BashArithmeticExpressions.Equal(x, y) => binary(x, y, "==")
      case BashArithmeticExpressions.NotEqual(x, y) => binary(x, y, "!=")
      case BashArithmeticExpressions.BitwiseAnd(x, y) => binary(x, y, "&")
      case BashArithmeticExpressions.BitwiseXor(x, y) => binary(x, y, "^")
      case BashArithmeticExpressions.BitwiseOr(x, y) => binary(x, y, "|")
      case BashArithmeticExpressions.LogicalAnd(x, y) => binary(x, y, "&&")
      case BashArithmeticExpressions.LogicalOr(x, y) => binary(x, y, "||")
      case BashArithmeticExpressions.Conditional(condition, trueCase, falseCase) =>
        parenthesedExpression(condition) >> space >> code("?") >> space >> parenthesedExpression(trueCase) >> space >> code(":") >> space >> parenthesedExpression(falseCase)
      case BashArithmeticExpressions.Assign(x, y) => assignment(x, y, "=")
      case BashArithmeticExpressions.AssignMul(x, y) => assignment(x, y, "*=")
      case BashArithmeticExpressions.AssignDiv(x, y) => assignment(x, y, "/=")
      case BashArithmeticExpressions.AssignRem(x, y) => assignment(x, y, "%=")
      case BashArithmeticExpressions.AssignAdd(x, y) => assignment(x, y, "+=")
      case BashArithmeticExpressions.AssignSub(x, y) => assignment(x, y, "-=")
      case BashArithmeticExpressions.AssignShiftLeft(x, y) => assignment(x, y, "<<=")
      case BashArithmeticExpressions.AssignShiftRight(x, y) => assignment(x, y, ">>=")
      case BashArithmeticExpressions.AssignAnd(x, y) => assignment(x, y, "&=")
      case BashArithmeticExpressions.AssignOr(x, y) => assignment(x, y, "|=")
      case BashArithmeticExpressions.AssignXor(x, y) => assignment(x, y, "^=")
      case BashArithmeticExpressions.Comma(x, y) => binary(x, y, ",")
    }
  }
}
