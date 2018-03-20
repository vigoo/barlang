package io.github.vigoo.bc.language

import io.github.vigoo.bash.language.{BashExpressions, BashPrettyPrint, BashVariables}
import io.github.vigoo.bc.language.BcExpressions.BashVariable
import io.github.vigoo.simpp.{PrettyPrint, PrettyPrinter}
import io.github.vigoo.simpp.PrettyPrint.PrettyPrinterContext
import org.atnos.eff.{Eff, NoFx}

object BcPrettyPrint extends PrettyPrint[NoFx] {
  override def runAdditionalFx(f: BcPrettyPrint.PP[Unit]): Eff[PrettyPrinterContext[NoFx], Unit] = f

  type PPrinter[T] = PrettyPrinter[T, NoFx]

  import BashPrettyPrint.bashExpressionPrettyPrinter

  implicit val expressionPrettyPrint: PPrinter[BcExpression] = {
    case BcExpressions.Number(value) => code(value.toString)
    case BcExpressions.BashVariable(identifier) => code(BashPrettyPrint.print(BashExpressions.ReadVariable(BashVariables.Variable(identifier))))
    case BcExpressions.Add(x, y) => parenthesed(x) >> space >> code("+") >> space >> parenthesed(y)
    case BcExpressions.Sub(x, y) => parenthesed(x) >> space >> code("-") >> space >> parenthesed(y)
    case BcExpressions.Mul(x, y) => parenthesed(x) >> space >> code("*") >> space >> parenthesed(y)
    case BcExpressions.Div(x, y) => parenthesed(x) >> space >> code("/") >> space >> parenthesed(y)
  }
}
