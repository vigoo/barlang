package io.github.vigoo.bash.language

import io.github.vigoo.bash.language.BashStatements.Nop
import io.github.vigoo.prettyprinter.PrettyPrinter
import io.github.vigoo.prettyprinter.PrettyPrinter.PrettyPrinterContext

object PrettyPrinterInstances {

  implicit val bashStatementPrettyPrinter: PrettyPrinter[BashStatement] = new PrettyPrinter[BashStatement] {
    override def prettyPrint(statement: BashStatement)(implicit context: PrettyPrinterContext): Unit = {
      statement match {
        case Nop =>
      }
    }
  }
}
