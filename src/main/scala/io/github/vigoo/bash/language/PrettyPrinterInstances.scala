package io.github.vigoo.bash.language

import io.github.vigoo.bash.language.BashExpressions.{Eval, Literal, ReadVariable}
import io.github.vigoo.bash.language.BashStatements.{Assign, Command, Nop}
import io.github.vigoo.bash.language.BashVariables.Variable
import io.github.vigoo.prettyprinter.PrettyPrinter
import io.github.vigoo.prettyprinter.PrettyPrinter.PrettyPrinterContext

object PrettyPrinterInstances {

  implicit def seqPrettyPrinter[ItemType](implicit itemPrettyPrinter: PrettyPrinter[ItemType]): PrettyPrinter[Seq[ItemType]] = new PrettyPrinter[Seq[ItemType]] {
    override def prettyPrint(value: Seq[ItemType])(implicit context: PrettyPrinterContext): Unit = {
      for ((item, index) <- value.zipWithIndex) {
        if (index > 0) {
          code(", ")
        }
        pretty(item)
      }
    }
  }

  implicit val bashStatementPrettyPrinter: PrettyPrinter[BashStatement] = new PrettyPrinter[BashStatement] {
    override def prettyPrint(statement: BashStatement)(implicit context: PrettyPrinterContext): Unit = {
      statement match {
        case Nop =>
        case Assign(target, expression) =>
          pretty(target)
          code("=")
          pretty(expression)
        case Command(name, params) =>
          pretty(name :: params)
      }
    }
  }

  implicit val bashIdentifierPrettyPrinter: PrettyPrinter[BashIdentifier] = new PrettyPrinter[BashIdentifier] {
    override def prettyPrint(identifier: BashIdentifier)(implicit context: PrettyPrinterContext): Unit =
      code(identifier.name)
  }

  implicit val bashVariablePrettyPrinter: PrettyPrinter[BashVariable] = new PrettyPrinter[BashVariable] {
    override def prettyPrint(variable: BashVariable)(implicit context: PrettyPrinterContext): Unit = {
      variable match {
        case Variable(name) => pretty(name)
      }
    }
  }

  implicit val bashExpressionPrettyPrinter: PrettyPrinter[BashExpression] = new PrettyPrinter[BashExpression] {
    override def prettyPrint(expression: BashExpression)(implicit context: PrettyPrinterContext): Unit = {
      expression match {
        case Literal(lit) => code(lit)
        case ReadVariable(variable) => dollar; pretty(variable)
        case Eval(statement) => between("$(", ")", statement)
      }
    }
  }
}
