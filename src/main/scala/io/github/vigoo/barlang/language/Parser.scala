package io.github.vigoo.barlang.language

import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

object Parser {

  sealed trait Token extends Positional
  object Tokens {
    case class Bool() extends Token
    case class String() extends Token
    case class Int() extends Token
    case class Double() extends Token
    case class Unit() extends Token
    case class Arrow() extends Token
    case class Return() extends Token
    case class Val() extends Token
    case class Def() extends Token
    case class Colon() extends Token
    case class End() extends Token
    case class Inline() extends Token
    case class If() extends Token
    case class Then() extends Token
    case class Else() extends Token
    case class Fn() extends Token
    case class And() extends Token
    case class Or() extends Token
    case class Not() extends Token
    case class While() extends Token
    case class BackArrow() extends Token
    case class Array() extends Token
    case class Equals() extends Token
    case class DoubleEquals() extends Token
    case class NotEquals() extends Token
    case class Less() extends Token
    case class LessEquals() extends Token
    case class Greater() extends Token
    case class GreaterEquals() extends Token
    case class Mul() extends Token
    case class Div() extends Token
    case class Minus() extends Token
    case class Plus() extends Token
    case class Mod() extends Token
    case class BoolLiteral(value: Boolean) extends Token
    case class IntLiteral(value: scala.Int) extends Token
    case class DoubleLiteral(value: scala.Double) extends Token
    case class StringLiteral(value: java.lang.String) extends Token
    case class Identifier(name: java.lang.String) extends Token
    case class SystemIdentifier(name: java.lang.String) extends Token
    case class ParenStart() extends Token
    case class BracketStart() extends Token
    case class Comma() extends Token
    case class BracketEnd() extends Token
    case class ParenEnd() extends Token
    case class NewLine() extends Token
  }

  object BarlangLexer extends RegexParsers {
    override def skipWhitespace: Boolean = true
    override val whiteSpace: Regex = """[ \t\r]+""".r

    private def bool = positioned { "bool" ^^ (_ => Tokens.Bool()) }
    private def string = positioned { "string" ^^ (_ => Tokens.String()) }
    private def int = positioned { "int" ^^ (_ => Tokens.Int()) }
    private def double = positioned { "double" ^^ (_ => Tokens.Double()) }
    private def unit = positioned { "unit" ^^ (_ => Tokens.Unit()) }
    private def arrow = positioned { "->" ^^ (_ => Tokens.Arrow()) }
    private def `return` = positioned { "return" ^^ (_ => Tokens.Return()) }
    private def `val` = positioned { "val" ^^ (_ => Tokens.Val()) }
    private def `def` = positioned { "def" ^^ (_ => Tokens.Def()) }
    private def colon = positioned { ":" ^^ (_ => Tokens.Colon()) }
    private def end = positioned { "end" ^^ (_ => Tokens.End()) }
    private def inline = positioned { "inline" ^^ (_ => Tokens.Inline()) }
    private def `if` = positioned { "if" ^^ (_ => Tokens.If()) }
    private def `then` = positioned { "then" ^^ (_ => Tokens.Then()) }
    private def `else` = positioned { "else" ^^ (_ => Tokens.Else()) }
    private def fn = positioned { "fn" ^^ (_ => Tokens.Fn()) }
    private def and = positioned { "and" ^^ (_ => Tokens.And()) }
    private def or = positioned { "or" ^^ (_ => Tokens.Or()) }
    private def not = positioned { "not" ^^ (_ => Tokens.Not()) }
    private def `while` = positioned { "while" ^^ (_ => Tokens.While()) }
    private def backArrow = positioned { "<-" ^^ (_ => Tokens.BackArrow()) }
    private def array = positioned { "array" ^^ (_ => Tokens.Array()) }
    private def equals = positioned { "=" ^^ (_ => Tokens.Equals()) }
    private def doubleEquals = positioned { "==" ^^ (_ => Tokens.DoubleEquals()) }
    private def notEquals = positioned { "!=" ^^ (_ => Tokens.NotEquals()) }
    private def less = positioned { "<" ^^ (_ => Tokens.Less()) }
    private def lessEquals = positioned { "<=" ^^ (_ => Tokens.LessEquals()) }
    private def greater = positioned { ">" ^^ (_ => Tokens.Greater()) }
    private def greaterEquals = positioned { ">=" ^^ (_ => Tokens.GreaterEquals()) }
    private def mul = positioned { "*" ^^ (_ => Tokens.Mul()) }
    private def div = positioned { "/" ^^ (_ => Tokens.Div()) }
    private def plus = positioned { "+" ^^ (_ => Tokens.Plus()) }
    private def minus = positioned { "-" ^^ (_ => Tokens.Minus()) }
    private def mod = positioned { "mod" ^^ (_ => Tokens.Mod()) }
    private def boolLiteral = positioned {
      """true|false""".r ^^ {
          case "true" => Tokens.BoolLiteral(true)
          case "false" => Tokens.BoolLiteral(false)
        }
    }
    private def intLiteral = positioned { """\-?\d+""".r ^^ (s => Tokens.IntLiteral(s.toInt)) }
    private def doubleLiteral = positioned { """\-?(\d+\.\d+)""".r ^^ (s => Tokens.DoubleLiteral(s.toDouble)) }
    private def stringLiteral = positioned { """"[^"]*"""".r ^^ (s => Tokens.StringLiteral(s.substring(1, s.length - 1))) }
    private def identifier = positioned { """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ (s => Tokens.Identifier(s)) }
    private def systemIdentifier = positioned { """\$[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ (s => Tokens.SystemIdentifier(s.tail)) }
    private def parenStart = positioned { "(" ^^ (_ => Tokens.ParenStart()) }
    private def bracketStart = positioned { "[" ^^ (_ => Tokens.BracketStart()) }
    private def comma = positioned { "," ^^ (_ => Tokens.Comma()) }
    private def bracketEnd = positioned { "]" ^^ (_ => Tokens.BracketEnd()) }
    private def parenEnd = positioned { ")" ^^ (_ => Tokens.ParenEnd()) }
    private def newLine = positioned { "\n|;".r ^^ (_ => Tokens.NewLine()) }

    def tokens: Parser[List[Token]] =
      phrase(
        rep1(parenStart | bracketStart | comma | bracketEnd | parenEnd |
             bool | string | int | double | unit | arrow | `return` | `val` | `def` | colon | end | inline |
            `if` | `then` | `else` | fn | and | or | not | `while` | backArrow | array |
            boolLiteral | doubleLiteral | intLiteral | stringLiteral | systemIdentifier | identifier |
            doubleEquals | equals | lessEquals | less | greaterEquals | greater | mul | div | plus | minus | mod |
            newLine))
  }

  private class BarlangTokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[Token] = new BarlangTokenReader(tokens.tail)
  }

  object BarlangParser extends Parsers {
    override type Elem = Token
    import Tokens._

    private def stringLiteral: Parser[Expression] =
      positioned {
        accept("string literal", { case lit@StringLiteral(s) =>
          Expressions.StringLiteral(s)
        })
      }

    private def boolLiteral: Parser[Expression] =
      positioned {
        accept("bool literal", { case lit@BoolLiteral(b) =>
          Expressions.BoolLiteral(b)
        })
      }

    private def intLiteral: Parser[Expression] =
      positioned {
        accept("int literal", { case lit@IntLiteral(i) =>
          Expressions.IntLiteral(i)
        })
      }

    private def doubleLiteral: Parser[Expression] =
      positioned {
        accept("double literal", { case lit@DoubleLiteral(d) =>
          Expressions.DoubleLiteral(d)
        })
      }

    private def identifier: Parser[SymbolName] =
        accept("variable identifier", { case lit@Identifier(name) =>
          SymbolName(name)
        })

    private def variable: Parser[Expression] =
      positioned {
        identifier ^^ Expressions.Variable
      }

    private def systemVariable: Parser[Expression] =
      positioned {
        accept("system variable identifier", { case lit@SystemIdentifier(name) =>
          Expressions.SystemVariable(SymbolName(name))
        })
      }

    private def arrayAccess: Parser[Expression] =
      positioned {
        identifier ~ BracketStart() ~ expression() ~ BracketEnd() ^^ { case name ~ _ ~ index ~ _ =>
          Expressions.ArrayAccess(name, index)
        }
      }

    private def paramList: Parser[List[Expression]] =
      rep1sep(expression(), Comma())

    private def functionApplication(parenthesized: Boolean): Parser[Expression] =
      positioned {
        if (parenthesized) {
          ParenStart() ~ expression(true) ~ ParenEnd() ~ ParenStart() ~ paramList ~ ParenEnd() ^^ { case _ ~ fnExpr ~ _ ~ _ ~ params ~ _ =>
            Expressions.Apply(fnExpr, params)
          }
        } else {
          expression(true) ~ ParenStart() ~ paramList ~ ParenEnd() ^^ { case fnExpr ~ _ ~ params ~ _ =>
            Expressions.Apply(fnExpr, params)
          }
        }
      }

    private def expression(parenthesizedFunApp: Boolean = false): Parser[Expression] =
      (ParenStart() ~ expression(parenthesizedFunApp) ~ ParenEnd() ^^ { case _ ~ e ~ _ => e }) |
      stringLiteral | boolLiteral | intLiteral | doubleLiteral |
      arrayAccess | functionApplication(parenthesizedFunApp) | variable | systemVariable

    private def ret =
      positioned {
        Return() ~ expression() ~ NewLine() ^^ { case _ ~ expr ~ _ =>
          SingleStatements.Return(expr)
        }
      }

    private def singleStatement: Parser[SingleStatement] =
      ret


    private def statement: Parser[Statement] =
      rep1(singleStatement) ^^ Statement.fromSingleStatements

    def script: Parser[Script] =
      phrase(statement) ^^ Script

    def apply(tokens: Seq[Token]): ParseResult[Script] = {
      val reader = new BarlangTokenReader(tokens)
      script(reader)
    }
  }
}
