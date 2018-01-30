package io.github.vigoo.bash.language

import cats.data.State
import io.github.vigoo.bash.language.BashConditions.Equals
import io.github.vigoo.bash.language.BashExpressions._
import io.github.vigoo.bash.language.BashStatements.{Assign, Command, IfThenElse, Nop}
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
  type PP[A] = PrettyPrinter[A, BashFx]

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

  implicit val stringPrettyPrinter: PP[String] =
    (value: String) => code(value)

  implicit val bashStatementPrettyPrinter: PP[BashStatement] = {
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
    }

  implicit val bashIdentifierPrettyPrinter: PP[BashIdentifier] =
    (identifier: BashIdentifier) => code(identifier.name)

  implicit val bashVariablePrettyPrinter: PP[BashVariable] = {
      case Variable(name) => pretty(name)
    }

  implicit val bashExpressionPrettyPrinter: PP[BashExpression] = {
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
    case Eval(statement) => between("$(", ")", statement)
    case Conditional(condition) => between("[[ ", " ]]", condition)
    case Interpolated(parts) => doubleQuoted(inString(pretty(sequence(parts))))
  }

  implicit val bashConditionPrettyPrinter: PP[BashCondition] = {
    case Equals(a, b) => pretty(a) >> space >> code("==") >> space >> pretty(b)
  }
}
