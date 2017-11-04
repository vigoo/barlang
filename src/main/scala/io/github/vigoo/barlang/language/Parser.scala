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

    override val whiteSpace: Regex =
      """(?m)([ \t\r]+)|(\/\/.*$)""".r

    private def bool = positioned {
      "bool" ^^ (_ => Tokens.Bool())
    }

    private def string = positioned {
      "string" ^^ (_ => Tokens.String())
    }

    private def int = positioned {
      "int" ^^ (_ => Tokens.Int())
    }

    private def double = positioned {
      "double" ^^ (_ => Tokens.Double())
    }

    private def unit = positioned {
      "unit" ^^ (_ => Tokens.Unit())
    }

    private def arrow = positioned {
      "->" ^^ (_ => Tokens.Arrow())
    }

    private def `return` = positioned {
      "return" ^^ (_ => Tokens.Return())
    }

    private def `val` = positioned {
      "val" ^^ (_ => Tokens.Val())
    }

    private def `def` = positioned {
      "def" ^^ (_ => Tokens.Def())
    }

    private def colon = positioned {
      ":" ^^ (_ => Tokens.Colon())
    }

    private def end = positioned {
      "end" ^^ (_ => Tokens.End())
    }

    private def inline = positioned {
      "inline" ^^ (_ => Tokens.Inline())
    }

    private def `if` = positioned {
      "if" ^^ (_ => Tokens.If())
    }

    private def `then` = positioned {
      "then" ^^ (_ => Tokens.Then())
    }

    private def `else` = positioned {
      "else" ^^ (_ => Tokens.Else())
    }

    private def fn = positioned {
      "fn" ^^ (_ => Tokens.Fn())
    }

    private def and = positioned {
      "and" ^^ (_ => Tokens.And())
    }

    private def or = positioned {
      "or" ^^ (_ => Tokens.Or())
    }

    private def not = positioned {
      "not" ^^ (_ => Tokens.Not())
    }

    private def `while` = positioned {
      "while" ^^ (_ => Tokens.While())
    }

    private def backArrow = positioned {
      "<-" ^^ (_ => Tokens.BackArrow())
    }

    private def array = positioned {
      "array" ^^ (_ => Tokens.Array())
    }

    private def equals = positioned {
      "=" ^^ (_ => Tokens.Equals())
    }

    private def doubleEquals = positioned {
      "==" ^^ (_ => Tokens.DoubleEquals())
    }

    private def notEquals = positioned {
      "!=" ^^ (_ => Tokens.NotEquals())
    }

    private def less = positioned {
      "<" ^^ (_ => Tokens.Less())
    }

    private def lessEquals = positioned {
      "<=" ^^ (_ => Tokens.LessEquals())
    }

    private def greater = positioned {
      ">" ^^ (_ => Tokens.Greater())
    }

    private def greaterEquals = positioned {
      ">=" ^^ (_ => Tokens.GreaterEquals())
    }

    private def mul = positioned {
      "*" ^^ (_ => Tokens.Mul())
    }

    private def div = positioned {
      "/" ^^ (_ => Tokens.Div())
    }

    private def plus = positioned {
      "+" ^^ (_ => Tokens.Plus())
    }

    private def minus = positioned {
      "-" ^^ (_ => Tokens.Minus())
    }

    private def mod = positioned {
      "mod" ^^ (_ => Tokens.Mod())
    }

    private def boolLiteral = positioned {
      """true|false""".r ^^ {
        case "true" => Tokens.BoolLiteral(true)
        case "false" => Tokens.BoolLiteral(false)
      }
    }

    private def intLiteral = positioned { """\-?\d+""".r ^^ (s => Tokens.IntLiteral(s.toInt))}

    private def doubleLiteral = positioned { """\-?(\d+\.\d+)""".r ^^ (s => Tokens.DoubleLiteral(s.toDouble))}

    private def stringLiteral = positioned { """"[^"]*"""".r ^^ (s => Tokens.StringLiteral(s.substring(1, s.length - 1)))}

    private def identifier = positioned { """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ (s => Tokens.Identifier(s))}

    private def systemIdentifier = positioned { """\$[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ (s => Tokens.SystemIdentifier(s.tail))}

    private def parenStart = positioned {
      "(" ^^ (_ => Tokens.ParenStart())
    }

    private def bracketStart = positioned {
      "[" ^^ (_ => Tokens.BracketStart())
    }

    private def comma = positioned {
      "," ^^ (_ => Tokens.Comma())
    }

    private def bracketEnd = positioned {
      "]" ^^ (_ => Tokens.BracketEnd())
    }

    private def parenEnd = positioned {
      ")" ^^ (_ => Tokens.ParenEnd())
    }

    private def newLine = positioned {
      "\n|;".r ^^ (_ => Tokens.NewLine())
    }

    def tokens: Parser[List[Token]] =
      phrase(
        rep1(parenStart | bracketStart | comma | bracketEnd | parenEnd |
          bool | string | int | double | unit | arrow | `return` | `val` | `def` | colon | end | inline |
          `if` | `then` | `else` | fn | and | or | not | `while` | backArrow | array |
          boolLiteral | doubleLiteral | intLiteral | stringLiteral |
          doubleEquals | equals | lessEquals | less | greaterEquals | greater | mul | div | plus | minus | mod |
          systemIdentifier | identifier |
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

    private def atLeastOneNewLine: Parser[List[Elem]] =
      rep(NewLine())

    private def typeList: Parser[List[Type]] =
      ParenStart() ~> rep1sep(typeExpr, Comma()) <~ ParenEnd()

    private def functionType: Parser[Type] =
      typeParamList ~ typeList ~ Arrow() ~ typeExpr ^^ {
        case typeParams ~ params ~ _ ~ returnType =>
          Types.Function(typeParams, params, returnType)
      }

    private def arrayType: Parser[Type] =
      BracketStart() ~> typeExpr <~ BracketEnd() ^^ (t => Types.Array(t))

    private def typeExpr: Parser[Type] =
      positioned {
        functionType |
          arrayType |
          (Int() ^^ (_ => Types.Int())) |
          (Bool() ^^ (_ => Types.Bool())) |
          (Unit() ^^ (_ => Types.Unit())) |
          (String() ^^ (_ => Types.String())) |
          (Double() ^^ (_ => Types.Double())) |
          (identifier ^^ (n => Types.TypeVariable(n)))
      }


    private def paramDef: Parser[ParamDef] = {
      positioned {
        identifier ~ Colon() ~ typeExpr ^^ { case name ~ _ ~ typ => ParamDef(name, typ) }
      }
    }

    private def paramDefs: Parser[List[ParamDef]] =
      ParenStart() ~> repsep(paramDef, Comma()) <~ ParenEnd()

    private def typeParam: Parser[TypeParam] =
      positioned {
        identifier ^^ (name => TypeParam(name))
      }

    private def typeParamList: Parser[List[TypeParam]] =
      (BracketStart() ~> repsep(typeParam, Comma()) <~ BracketEnd()) | success(List.empty)

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
        identifier ~ BracketStart() ~ expression ~ BracketEnd() ^^ { case name ~ _ ~ index ~ _ =>
          Expressions.ArrayAccess(name, index)
        }
      }

    private def paramList: Parser[List[Expression]] =
      repsep(expression, Comma())

    private def functionApplication: Parser[Expression] =
      positioned {
        identifier ~ ParenStart() ~ paramList ~ ParenEnd() ^^ { case fnName ~ _ ~ params ~ _ =>
          Expressions.Apply(Expressions.Variable(fnName), params)
        }
      }

    private def lambda: Parser[Expression] =
      positioned {
        Fn() ~> typeParamList ~ paramDefs ~ Colon() ~ typeExpr ~ atLeastOneNewLine ~ statement ~ End() ^^ {
          case typeParams ~ params ~ _ ~ returnType ~ _ ~ body ~ _ =>
            Expressions.Lambda(typeParams, params, returnType, body)
        }
      }

    private def term: Parser[Expression] =
      positioned {
        (ParenStart() ~ expression ~ ParenEnd() ^^ { case _ ~ e ~ _ => e }) |
          lambda |
          stringLiteral | boolLiteral | intLiteral | doubleLiteral | arrayAccess | functionApplication | variable | systemVariable
      }

    private def infix(term: Parser[Expression], cases: List[(Token, (Expression, Expression) => Expression)]): Parser[Expression] = {
      for {
        a <- term
        rest <-
        rep {
          cases.foldLeft[Parser[(Expression, (Expression, Expression) => Expression)]](failure("no matching operators")) { case (prev, (op, factory)) =>
            prev | positioned { op ~> term } ^^ { b => (b, factory) }
          }
        }
      } yield rest.foldLeft(a) { case (x, (y, factory)) => factory(x, y) }
    }

    private def prefix(nextLevel: Parser[Expression], operator: Token, factory: Expression => Expression): Parser[Expression] =
      operator ~ nextLevel ^^ { case _ ~ a => factory(a) } | nextLevel

    private def exprL4: Parser[Expression] =
      prefix(term, Not(), Expressions.UnaryOp(UnaryOperators.Not, _))

    private def exprL3: Parser[Expression] =
      infix(exprL4, List(
        Mul() -> (Expressions.BinaryOp(BinaryOperators.Mul, _, _)),
        Div() -> (Expressions.BinaryOp(BinaryOperators.Div, _, _)),
        Mod() -> (Expressions.BinaryOp(BinaryOperators.Mod, _, _)),
        And() -> (Expressions.BinaryOp(BinaryOperators.And, _, _))
      ))

    private def exprL2: Parser[Expression] =
      infix(exprL3, List(
        Plus() -> (Expressions.BinaryOp(BinaryOperators.Add, _, _)),
        Minus() -> (Expressions.BinaryOp(BinaryOperators.Sub, _, _)),
        Or() -> (Expressions.BinaryOp(BinaryOperators.Or, _, _))
      ))

    private def exprL1: Parser[Expression] =
      infix(exprL2, List(
        DoubleEquals() -> (Expressions.BinaryOp(BinaryOperators.Eq, _, _)),
        NotEquals() -> (Expressions.BinaryOp(BinaryOperators.Neq, _, _)),
        Less() -> (Expressions.BinaryOp(BinaryOperators.Less, _, _)),
        LessEquals() -> (Expressions.BinaryOp(BinaryOperators.LessEq, _, _)),
        Greater() -> (Expressions.BinaryOp(BinaryOperators.Greater, _, _)),
        GreaterEquals() -> (Expressions.BinaryOp(BinaryOperators.GreaterEq, _, _))
      ))

    private def expression: Parser[Expression] =
      exprL1

    private def variableDecl: Parser[SingleStatement] =
      positioned {
        Val() ~> identifier ~ Equals() ~ expression <~ atLeastOneNewLine ^^ { case name ~ _ ~ expr =>
          SingleStatements.VariableDeclaration(name, expr)
        }
      }

    private def arrayDecl: Parser[SingleStatement] =
      positioned {
        Array() ~> BracketStart() ~> typeExpr ~ BracketEnd() ~ identifier <~ atLeastOneNewLine ^^ {
          case typ ~ _ ~ name =>
            SingleStatements.ArrayDeclaration(name, typ)
        }
      }

    private def inlineKeyword: Parser[Boolean] =
      opt(Inline() ^^ (_ => true)).map(_.getOrElse(false))

    private def functionDefinition: Parser[SingleStatement] =
      positioned {
        inlineKeyword ~ Def() ~ identifier ~ typeParamList ~ paramDefs ~ Colon() ~ typeExpr ~ atLeastOneNewLine ~ statement <~ End() <~ atLeastOneNewLine ^^ {
          case isInline ~ _ ~ name ~ typeParameters ~ parameters ~ _ ~ returnType ~ _ ~ body =>
            SingleStatements.FunctionDefinition(
              name = name,
              properties = FunctionProperties(isInline),
              typeParams = typeParameters,
              paramDefs = parameters,
              returnType = returnType,
              body = body
            )
        }
      }

    private def ret: Parser[SingleStatement] =
      positioned {
        Return() ~> expression <~ atLeastOneNewLine ^^ { expr =>
          SingleStatements.Return(expr)
        }
      }

    private def run: Parser[SingleStatement] =
      positioned {
        Greater() ~> expression ~ rep(expression) <~ atLeastOneNewLine ^^ {
          case command ~ parameters =>
            SingleStatements.Run(command, parameters)
        }
      }

    private def ifThenElse: Parser[SingleStatement] =
      positioned {
        (If() ~> expression ~ Then() ~ atLeastOneNewLine ~ statement).flatMap {
          case condition ~ _ ~ _ ~ trueBody =>
            Else() ~> atLeastOneNewLine ~> statement <~ End() <~ atLeastOneNewLine ^^ {
              falseBody =>
                SingleStatements.If(condition, trueBody, falseBody)
            } | End() <~ atLeastOneNewLine ^^ { _ =>
              SingleStatements.If(condition, trueBody, Statements.NoOp)
            }
        }
      }

    private def whileLoop: Parser[SingleStatement] =
      positioned {
        While() ~> expression ~ atLeastOneNewLine ~ statement <~ End() <~ atLeastOneNewLine ^^ {
          case condition ~ _ ~ body =>
            SingleStatements.While(condition, body)
        }
      }

    private def updateCell: Parser[SingleStatement] =
      positioned {
        identifier ~ BracketStart() ~ expression ~ BracketEnd() ~ BackArrow() ~ expression <~ atLeastOneNewLine ^^ {
          case name ~ _ ~ index ~ _ ~ _ ~ value =>
            SingleStatements.UpdateCell(name, index, value)
        }
      }

    private def updateVariable: Parser[SingleStatement] =
      positioned {
        identifier ~ BackArrow() ~ expression <~ atLeastOneNewLine ^^ {
          case name ~ _ ~ value =>
            SingleStatements.UpdateVariable(name, value)
        }
      }

    private def functionCall: Parser[SingleStatement] =
      positioned {
        functionApplication <~ atLeastOneNewLine ^^ {
          case Expressions.Apply(function, parameters) =>
            SingleStatements.Call(function, parameters)
          case e =>
            throw new IllegalStateException(s"Function application parser returned with unexpected expression: $e")
        }
      }

    private def singleStatement: Parser[SingleStatement] =
      variableDecl |
        arrayDecl |
        functionDefinition |
        ret |
        run |
        ifThenElse |
        whileLoop |
        updateCell |
        updateVariable |
        functionCall

    private def statement: Parser[Statement] =
      rep(singleStatement) ^^ Statement.fromSingleStatements

    def script: Parser[Script] =
      phrase(rep(NewLine()) ~> statement) ^^ Script

    def apply(tokens: Seq[Token]): ParseResult[Script] = {
      val reader = new BarlangTokenReader(tokens)
      script(reader)
    }
  }

}
