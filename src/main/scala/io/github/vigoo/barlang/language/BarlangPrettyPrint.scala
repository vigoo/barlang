package io.github.vigoo.barlang.language

import cats.Traverse
import io.github.vigoo.barlang.language.BinaryOperators._
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import io.github.vigoo.barlang.language.Statements.{NoOp, Sequence, Single}
import io.github.vigoo.simpp.{PrettyPrint, PrettyPrinter}
import cats.implicits._
import io.github.vigoo.simpp.PrettyPrint.PrettyPrinterContext
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._


object BarlangPrettyPrint extends PrettyPrint[NoFx] {
  override def runAdditionalFx(f: Eff[BarlangPrettyPrint.R, Unit]): Eff[PrettyPrinterContext[NoFx], Unit] = f

  type PPrinter[T] = PrettyPrinter[T, NoFx]

  implicit val stringPrettyPrinter: PPrinter[String] =
    (value: String) => code(value)

  implicit val symbolNamePrettyPrinter: PPrinter[SymbolName] = (value: SymbolName) => {
    code(value.name)
  }

  implicit val typeParamPrettyPrinter: PPrinter[TypeParam] = (value: TypeParam) => {
    pretty(value.name)
  }

  implicit val typePrettyPrinter: PPrinter[Type] = {
    case Types.Unit() =>
      code("unit")
    case Types.String() =>
      code("string")
    case Types.Bool() =>
      code("bool")
    case Types.Int() =>
      code("int")
    case Types.Double() =>
      code("double")
    case Types.Function(typeParams, paramTypes, returnType) =>
      (if (typeParams.nonEmpty) {
        squareBracketed(sequence(typeParams, ", ")) >> space
      } else {
        empty
      }) >> parenthesed(sequence(paramTypes, ", ")) >> code(" -> ") >> pretty(returnType)

    case Types.TypeVariable(name) =>
      pretty(name)
    case Types.Array(elementType) =>
      squareBracketed(elementType)
  }

  implicit val paramDefPrettyPrinter: PPrinter[ParamDef] = (value: ParamDef) => {
    pretty(value.name) >> code(": ") >> pretty(value.typ)
  }

  implicit val expressionPrettyPrinter: PPrinter[Expression] = {
    case StringLiteral(text) =>
      between("\"", "\"", escaped(text))

    case BoolLiteral(true) =>
      code("true")
    case BoolLiteral(false) =>
      code("false")

    case IntLiteral(number) =>
      code(number.toString)
    case DoubleLiteral(number) =>
      code(number.toString)

    case Variable(name) =>
      pretty(name)

    case ArrayAccess(name, index) =>
      pretty(name) >> squareBracketed(index)

    case SystemVariable(name) =>
      code("$") >> pretty(name)

    case Predefined(name) =>
      pretty(name)

    case Apply(function, parameters) =>
      pretty(function) >> parenthesed(sequence(parameters, ", "))

    case UnaryOp(UnaryOperators.Not, x) =>
      code("not") >> space >> parenthesed(x)

    case BinaryOp(operator, x, y) =>
      val opStr = operator match {
        case And => "and"
        case Or => "or"
        case Add => "+"
        case Sub => "-"
        case Mul => "*"
        case Div => "/"
        case Mod => "mod"
        case Eq => "=="
        case Neq => "!="
        case Less => "<"
        case LessEq => "<="
        case Greater => ">"
        case GreaterEq => ">="
      }
      code("(") >> pretty(x) >> space >> code(opStr) >> space >> pretty(y) >> code(")")

    case Lambda(typeParams, paramDefs, returnType, body) =>
      code("fn") >>
        (if (typeParams.nonEmpty) {
          squareBracketed(sequence(typeParams, ", "))
        } else {
          empty
        }) >>
        space >> parenthesed(sequence(paramDefs, ", ")) >> code(": ") >> pretty(returnType) >> newline >>
        indented(pretty(body)) >> newline >>
        code("end")
  }

  implicit val singleStatementPrettyPrinter: PPrinter[SingleStatement] = {
    case VariableDeclaration(name, VariableProperties(false), value) =>
      code("val ") >> pretty(name) >> code(" = ") >> pretty(value)
    case VariableDeclaration(name, VariableProperties(true), value) =>
      code("var ") >> pretty(name) >> code(" = ") >> pretty(value)

    case ArrayDeclaration(name, elementType) =>
      code("array") >> squareBracketed(elementType) >> space >> pretty(name)

    case FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) =>
      (if (properties.inline) {
        code("inline ")
      } else {
        empty
      }) >>
        code("def ") >> pretty(name) >>
        (if (typeParams.nonEmpty) {
          squareBracketed(sequence(typeParams, ", "))
        } else {
          empty
        }) >>
        parenthesed(sequence(paramDefs, ", ")) >> code(": ") >> pretty(returnType) >> newline >>
        indented(pretty(body)) >> newline >>
        code("end")

    case Call(function, parameters) =>
      pretty(function) >> parenthesed(sequence(parameters, ", "))

    case Run(command, parameters) =>
      code("> ") >> pretty(command) >> space >>
        Traverse[List].traverse(parameters.zipWithIndex) { case (item, index) =>
          (if (index > 0) {
            space
          } else {
            empty
          }) >> pretty(item)
        }.map(_ => ())

    case If(condition, trueBody, falseBody) =>
      code("if") >> space >> pretty(condition) >> space >> code("then") >> newline >>
        indented {
          pretty(trueBody) >> newline
        } >>
        (if (falseBody != Statements.NoOp) {
          code("else") >> newline >>
            indented {
              pretty(falseBody)
            } >> newline
        } else {
          empty
        }) >>
        code("end")

    case While(condition, body) =>
      code("while") >> space >> pretty(condition) >> newline >>
        indented(pretty(body)) >> newline >>
        code("end")

    case UpdateVariable(name, value) =>
      pretty(name) >> space >> code("<-") >> space >> pretty(value)

    case UpdateCell(name, index, value) =>
      pretty(name) >> squareBracketed(index) >> code(" <- ") >> pretty(value)

    case Return(value) =>
      code("return") >> space >> pretty(value)
  }

  implicit val statementPrettyPrinter: PPrinter[Statement] = (value: Statement) => value match {
    case Single(statement) =>
      pretty(statement)

    case Sequence(first, second) =>
      pretty(first) >> newline >>
      pretty(second)

    case NoOp =>
      empty
  }

  implicit val scriptPrettyPrinter: PPrinter[Script] = (value: Script) => pretty(value.body)

  private def escaped(text: String): String =
    text.map {
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c: Char => c
    }.mkString
}
