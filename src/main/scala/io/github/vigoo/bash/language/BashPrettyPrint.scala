package io.github.vigoo.bash.language

import cats.data.State
import io.github.vigoo.bash.language.BashConditions.Equals
import io.github.vigoo.bash.language.BashExpressions._
import io.github.vigoo.bash.language.BashStatements._
import io.github.vigoo.bash.language.BashVariables.Variable
import io.github.vigoo.simpp.{PrettyPrint, PrettyPrinter}
import cats.implicits._
import io.github.vigoo.simpp.PrettyPrint.PrettyPrinterContext
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
      case Declare(options, identifier, optionalInitialValue) => {
        val prefix = code("declare") >> space >> pretty(sequence(options.toList, separator = " ")) >> space >> code(identifier.name)
        optionalInitialValue match {
          case Some(initialValue) =>
            prefix >> code("=") >> pretty(initialValue)
          case None =>
            prefix
        }
      }

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
        if (lit.exists(_.isWhitespace) && !state.inString) {
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
  }

  implicit val bashConditionPrettyPrinter: PPrinter[BashCondition] = {
    case Equals(a, b) => pretty(a) >> space >> code("==") >> space >> pretty(b)
  }
}
