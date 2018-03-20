package io.github.vigoo

import io.github.vigoo.simpp._
import io.github.vigoo.simpp.PrettyPrint._
import cats.instances.string._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.matcher.Matcher

trait PrettyPrinterTests[AdditionalFx, PP <: PrettyPrint[AdditionalFx]] {
  this: Specification =>

  def pp: PP

  protected def bePrintedAs[T](expected: String)(implicit prettyPrinter: PrettyPrinter[T, AdditionalFx]): Matcher[T] =
    beEqualTo(expected) ^^ { (value: T) => pp.print(value)(prettyPrinter) }

  protected def bePrinting(expected: String): Matcher[Eff[PP#R, Unit]] =
    beEqualTo(expected) ^^ { (value: Eff[PP#R, Unit]) =>
      val initialState = PrettyPrinterState(
        indentationSize = 4,
        indentation = 0,
        atLineStart = true
      )
      val ((), result) = pp.runAdditionalFx(value).runWriterMonoid.evalState(initialState).run
      result
    }
}
