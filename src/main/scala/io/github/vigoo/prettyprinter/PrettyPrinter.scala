package io.github.vigoo.prettyprinter

import PrettyPrinter.PrettyPrinterContext

trait PrettyPrinter[-T] {
  def prettyPrint(value: T)(implicit context: PrettyPrinterContext): Unit

  protected final def space(implicit context: PrettyPrinterContext): Unit =
    context.append(' ')

  protected final def newline(implicit context: PrettyPrinterContext): Unit =
    context.startNewLine()

  protected final def code(str: String)(implicit context: PrettyPrinterContext): Unit = {
    context.append(str)
  }

  protected final def pretty[I](value: I)(implicit innerPrettyPrinter: PrettyPrinter[I], context: PrettyPrinterContext): Unit =
    innerPrettyPrinter.prettyPrint(value)

  protected final def between[I](left: String, right: String, inner: I)(implicit innerPrettyPrinter: PrettyPrinter[I], context: PrettyPrinterContext): Unit = {
    context.append(left)
    pretty(inner)
    context.append(right)
  }

  protected final def parenthesed[I](inner: I)(implicit innerPrettyPrinter: PrettyPrinter[I], context: PrettyPrinterContext): Unit =
    between("(", ")", inner)

  protected final def squareBracketed[I](inner: I)(implicit innerPrettyPrinter: PrettyPrinter[I], context: PrettyPrinterContext): Unit =
    between("[", "]", inner)

  protected final def indented(block: => Unit)(implicit context: PrettyPrinterContext): Unit = {
    context.indent()
    try {
      block
    } finally {
      context.unindent()
    }
  }
}

object PrettyPrinter {

  class PrettyPrinterContext(builder: StringBuilder, indentationSize: Int = 4) {
    var indentation: Int = 0
    var atLineStart: Boolean = true

    def append(char: Char): Unit = {
      if (char == '\n') {
        startNewLine()
      } else {
        indentIfNeeded()
        builder.append(char)
      }
    }

    def append(str: String): Unit = {
      val lines = str.lines.toList

      for ((line, idx) <- lines.zipWithIndex) {
        indentIfNeeded()
        builder.append(line)
        if (idx > 0 && idx <= (lines.length - 2)) {
          startNewLine()
        }
      }
    }

    def startNewLine(): Unit = {
      builder.append('\n')
      atLineStart = true
    }

    def indent(): Unit = {
      indentation += 1
    }

    def unindent(): Unit = {
      require(indentation > 0)
      indentation -= 1
    }

    private def indentIfNeeded(): Unit = {
      if (atLineStart) {
        builder.append(indentationChars())
        atLineStart = false
      }
    }

    private def indentationChars(): String =
      " " * (indentation * indentationSize)
  }

  def apply[T](value: T)(implicit prettyPrinter: PrettyPrinter[T]): String = {
    val builder = StringBuilder.newBuilder
    implicit val context = new PrettyPrinterContext(builder)

    prettyPrinter.prettyPrint(value)
    builder.toString
  }
}