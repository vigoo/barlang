package io.github.vigoo.prettyprinter

import org.atnos.eff.{Eff, NoFx}
import org.specs2._
import _root_.io.github.vigoo.prettyprinter.PrettyPrint.PrettyPrinterContext

object BasePrettyPrinter extends PrettyPrint[NoFx] {
  override def runAdditionalFx(f: Eff[BasePrettyPrinter.R, Unit]): Eff[PrettyPrinterContext[NoFx], Unit] = f
}

class PrettyPrinterSpecs extends Specification with PrettyPrinterTests[NoFx, BasePrettyPrinter.type] { def is = s2"""
  With the pretty printer
    empty prints nothing           $emptyTest
    space prints space             $spaceTest
    dollar prints dollar           $dollarTest
    newline prints \\n             $newlineTest
    code prints the arg            $codeTest
    between wraps an inner printer $betweenTest
    parenthesed wraps in ()        $parenthesedTest
    squareBracketed wraps in []    $squareBracketedTest
    curlyBracketed wraps in {}     $curlyBracketedTest
    doubleQuoted wraps in ""       $doubleQuotedTest
    flatMap concatenates           $flatMapTest
    indentation works              $indentedTest
    sequence without sep works     $seqWithoutSep
    sequence with sep works        $seqWithSep
  """

  override val pp = BasePrettyPrinter
  import pp._

  def emptyTest = pp.empty should bePrinting("")
  def spaceTest = space should bePrinting(" ")
  def dollarTest = dollar should bePrinting("$")
  def newlineTest = newline should bePrinting("\n")
  def codeTest = code("hello world") should bePrinting("hello world")
  def betweenTest = pp.between("left", "right", code("inner")) should bePrinting("leftinnerright")
  def parenthesedTest = parenthesed(dollar) should bePrinting("($)")
  def squareBracketedTest = squareBracketed(dollar) should bePrinting("[$]")
  def curlyBracketedTest = curlyBracketed(dollar) should bePrinting("{$}")
  def doubleQuotedTest = doubleQuoted(code("hello world")) should bePrinting("\"hello world\"")
  def flatMapTest = (code("hello") >> space >> code("world")) should bePrinting("hello world")
  def indentedTest =
    (code("first line") >> newline >> indented(code("second line") >> newline >> code("third line") >> newline) >> code("fourth line")).should(
      bePrinting(
        """first line
          |    second line
          |    third line
          |fourth line""".stripMargin
      ))

  def seqWithoutSep = sequence(List(code("1"), code("2"), code("3"))) should bePrintedAs("123")
  def seqWithSep = sequence(List(code("1"), code("2"), code("3")), ", ") should bePrintedAs("1, 2, 3")
}
