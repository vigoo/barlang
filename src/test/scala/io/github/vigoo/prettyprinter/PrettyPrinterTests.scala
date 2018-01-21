package io.github.vigoo.prettyprinter

import org.specs2.matcher.Matcher
import cats.instances.string._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import PrettyPrint._

trait PrettyPrinterTests[AdditionalFx, PP <: PrettyPrint[AdditionalFx]] {
  this: Specification =>

  def pp: PP

  protected def bePrintedAs[T](expected: String)(implicit prettyPrinter: PrettyPrinter[T, AdditionalFx]): Matcher[T] =
    { (value: T) => pp.print(value)(prettyPrinter) must_== expected }

  protected def bePrinting(expected: String): Matcher[Eff[PP#R, Unit]] =
    { (value: Eff[PP#R, Unit]) =>
      val initialState = PrettyPrinterState(
        indentationSize = 4,
        indentation = 0,
        atLineStart = true
      )
      val ((), result) = pp.runAdditionalFx(value).runWriterMonoid.evalState(initialState).run
      result must_== expected
    }
}
