package io.github.vigoo.bc.language

import io.github.vigoo.PrettyPrinterTests
import io.github.vigoo.bash.language.BashIdentifier
import io.github.vigoo.bc.language.BcExpressions._
import io.github.vigoo.bc.language.BcPrettyPrint._
import org.atnos.eff.NoFx
import org.specs2.Specification

class BcPrettyPrinterSpecs extends Specification with PrettyPrinterTests[NoFx, BcPrettyPrint.type] { def is = s2"""
  The bc expression pretty printer should correctly print
    numbers            ${Number(-1.5) should bePrintedAs("-1.5")}
    bash variables     ${BashVariable(BashIdentifier("TMP")) should bePrintedAs("${TMP}")}
    addition           ${Add(Number(1), BashVariable(BashIdentifier("X"))) should bePrintedAs("(1.0) + ($X)")}
    substraction       ${Sub(Number(3), Number(0.1)) should bePrintedAs("(3.0) - (0.1)")}
    multiplication     ${Mul(Number(1), Number(1)) should bePrintedAs("(1.0) * (1.0)")}
    division           ${Div(Number(10), Number(2)) should bePrintedAs("(10.0) / (2.0)")}
    nested expressions ${Add(Number(1), Div(BashVariable(BashIdentifier("X")), Number(2))) should bePrintedAs("(1.0) + (($X) / (2.0))")}
    function calls     ${FunctionCall("sin", Number(1.0)) should bePrintedAs("sin(1.0)")}
  """

  override val pp = BcPrettyPrint
}
