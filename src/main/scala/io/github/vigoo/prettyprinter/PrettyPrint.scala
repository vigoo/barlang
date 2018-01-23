package io.github.vigoo.prettyprinter

import cats.Foldable
import cats.data.{State, Writer}
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.Members.{&&:, &:}

object PrettyPrint {
  sealed trait SeqElemBase[I]

  case class SeqElem[I](elem: I) extends SeqElemBase[I]

  case class SeqSeparator[I](separator: String) extends SeqElemBase[I]


  case class PrettyPrinterState(indentationSize: Int, indentation: Int, atLineStart: Boolean)

  type PrettyPrinterContext[AdditionalFx] =
    Fx.append[Fx.fx2[Writer[String, ?], State[PrettyPrinterState, ?]], AdditionalFx]

  type _state[R] = State[PrettyPrinterState, ?] |= R
  type _writer[R] = Writer[String, ?] |= R
  type _prettyPrinter[R] = _state[R] &&: _writer[R]

  def write[R: _writer](s: String): Eff[R, Unit] =
    tell[R, String](s)

  def getState[R: _state]: Eff[R, PrettyPrinterState] =
    get[R, PrettyPrinterState]

  def modifyState[R: _state](f: PrettyPrinterState => PrettyPrinterState): Eff[R, Unit] =
    modify[R, PrettyPrinterState](f)
}

trait PrettyPrinter[-T, AdditionalFx] {
  def prettyPrint(value: T): Eff[PrettyPrint.PrettyPrinterContext[AdditionalFx], Unit]
}

trait PrettyPrint[AdditionalFx] {
  import PrettyPrint._
  type R = PrettyPrinterContext[AdditionalFx]

  def runAdditionalFx(f: Eff[R, Unit]): Eff[PrettyPrinterContext[NoFx], Unit]

  def append(char: Char): Eff[R, Unit] =
    if (char == '\n') {
      startNewLine()
    } else {
      for {
        _ <- indentIfNeeded()
        _ <- write[R](char.toString)
      } yield ()
    }

  def append(str: String): Eff[R, Unit] = {
    val lines = Foldable[List].intercalate(
      str.lines.toList.map(line => List(line)),
      List("\n"))

    def printLines(lines: List[String]): Eff[R, Unit] =
      lines match {
        case Nil => empty
        case "\n" :: remaining => startNewLine() >> printLines(remaining)
        case other :: remaining => indentIfNeeded() >> write[R](other) >> printLines(remaining)
      }
    printLines(lines)
  }

  def startNewLine(): Eff[R, Unit] =
    for {
      _ <- write[R]("\n")
      _ <- modifyState[R](_.copy(atLineStart = true))
    } yield ()

  def indent(): Eff[R, Unit] =
    modifyState[R](state => state.copy(indentation = state.indentation + 1))

  def unindent(): Eff[R, Unit] = {
    // TODO: error handling
    modifyState[R](state => state.copy(indentation = state.indentation - 1))
  }

  private def indentIfNeeded(): Eff[R, Unit] = {
    getState[R].flatMap { state =>
      if (state.atLineStart) {
        for {
          _ <- write[R](indentationChars(state))
          _ <- modifyState[R](_.copy(atLineStart = false))
        } yield ()
      } else {
        empty
      }
    }
  }

  private def indentationChars(state: PrettyPrinterState): String =
    " " * (state.indentation * state.indentationSize)


  def print[T](value: T)(implicit prettyPrinter: PrettyPrinter[T, AdditionalFx]): String = {
    val initialState = PrettyPrinterState(
      indentationSize = 4,
      indentation = 0,
      atLineStart = true
    )
    val ((), result) = runAdditionalFx(prettyPrinter.prettyPrint(value)).runWriterMonoid.evalState(initialState).run
    result
  }

  def empty: Eff[R, Unit] =
    unit[R]

  def space: Eff[R, Unit] =
    append(' ')

  def dollar: Eff[R, Unit] =
    append('$')

  def newline: Eff[R, Unit] =
    startNewLine()

  def code(str: String): Eff[R, Unit] = {
    append(str)
  }

  def pretty[I](value: I)(implicit innerPrettyPrinter: PrettyPrinter[I, AdditionalFx]): Eff[R, Unit] =
    innerPrettyPrinter.prettyPrint(value)

  def between[I](left: String, right: String, inner: I)(implicit innerPrettyPrinter: PrettyPrinter[I, AdditionalFx]): Eff[R, Unit] =
    for {
      _ <- append(left)
      _ <- pretty(inner)
      _ <- append(right)
    } yield ()

  def parenthesed[I](inner: I)(implicit innerPrettyPrinter: PrettyPrinter[I, AdditionalFx]): Eff[R, Unit] =
    between("(", ")", inner)

  def squareBracketed[I](inner: I)(implicit innerPrettyPrinter: PrettyPrinter[I, AdditionalFx]): Eff[R, Unit] =
    between("[", "]", inner)

  def curlyBracketed[I](inner: I)(implicit innerPrettyPrinter: PrettyPrinter[I, AdditionalFx]): Eff[R, Unit] =
    between("{", "}", inner)

  def doubleQuoted[I](inner: I)(implicit innerPrettyPrinter: PrettyPrinter[I, AdditionalFx]): Eff[R, Unit] =
    between("\"", "\"", inner)

  def indented[A](block: Eff[R, A]): Eff[R, A] =
    for {
      _ <- indent()
      result <- block
      _ <- unindent()
    } yield result

  def sequence[I](inner: List[I], separator: String = ""): List[SeqElemBase[I]] =
    Foldable[List].intercalate(
      inner.map(elem => List(SeqElem(elem).asInstanceOf[SeqElemBase[I]])),
      List(SeqSeparator[I](separator)))

  def printSequence[I](items: List[I], separator: String = "")(implicit innerPrettyPrinter: PrettyPrinter[I, AdditionalFx]): Eff[R, Unit] =
    sequencePrettyPrinter[I].prettyPrint(sequence(items, separator))


  implicit def primitivePrettyPrinter: PrettyPrinter[Eff[R, Unit], AdditionalFx] =
    (printer: Eff[R, Unit]) => printer

  implicit def sequencePrettyPrinter[I](implicit innerPrettyPrinter: PrettyPrinter[I, AdditionalFx]): PrettyPrinter[List[SeqElemBase[I]], AdditionalFx] =
    (elems: List[SeqElemBase[I]]) => {

      def printItems(items: List[SeqElemBase[I]]): Eff[R, Unit] =
        items match {
          case Nil => empty
          case SeqElem(elem) :: remaining => pretty(elem) >> printItems(remaining)
          case SeqSeparator(separator) :: remaining => code(separator)  >> printItems(remaining)
        }

      printItems(elems)
    }
}