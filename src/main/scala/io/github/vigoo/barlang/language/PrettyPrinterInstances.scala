package io.github.vigoo.barlang.language

import io.github.vigoo.barlang.language.BinaryOperators._
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import io.github.vigoo.barlang.language.Statements.{NoOp, Sequence, Single}
import io.github.vigoo.barlang.prettyprinter.PrettyPrinter
import io.github.vigoo.barlang.prettyprinter.PrettyPrinter.PrettyPrinterContext


object PrettyPrinterInstances {

  implicit val stringPrettyPrinter: PrettyPrinter[String] = new PrettyPrinter[String] {
    override def prettyPrint(value: String)(implicit context: PrettyPrinterContext): Unit =
      code(value)
  }

  implicit def seqPrettyPrinter[ItemType](implicit itemPrettyPrinter: PrettyPrinter[ItemType]): PrettyPrinter[Seq[ItemType]] = new PrettyPrinter[Seq[ItemType]] {
    override def prettyPrint(value: Seq[ItemType])(implicit context: PrettyPrinterContext): Unit = {
      for ((item, index) <- value.zipWithIndex) {
        if (index > 0) {
          code(", ")
        }
        pretty(item)
      }
    }
  }

  implicit val symbolNamePrettyPrinter: PrettyPrinter[SymbolName] = new PrettyPrinter[SymbolName] {
    override def prettyPrint(value: SymbolName)(implicit context: PrettyPrinterContext): Unit = {
      code(value.name)
    }
  }

  implicit val typeParamPrettyPrinter: PrettyPrinter[TypeParam] = new PrettyPrinter[TypeParam] {
    override def prettyPrint(value: TypeParam)(implicit context: PrettyPrinterContext): Unit = {
      pretty(value.name)
    }
  }

  implicit val typePrettyPrinter: PrettyPrinter[Type] = new PrettyPrinter[Type] {
    override def prettyPrint(value: Type)(implicit context: PrettyPrinterContext): Unit =
      value match {
        case Types.Unit =>
          code("unit")
        case Types.String =>
          code("string")
        case Types.Bool =>
          code("bool")
        case Types.Int =>
          code("int")
        case Types.Double =>
          code("double")
        case Types.Function(typeParams, paramTypes, returnType) =>
          if (typeParams.nonEmpty) {
            squareBracketed(typeParams)
            space
          }
          parenthesed(paramTypes)
          code(" -> ")
          pretty(returnType)

        case Types.TypeVariable(name) =>
          pretty(name)
        case Types.Array(elementType) =>
          squareBracketed(elementType)
      }
  }

  implicit val paramDefPrettyPrinter: PrettyPrinter[ParamDef] = new PrettyPrinter[ParamDef] {
    override def prettyPrint(value: ParamDef)(implicit context: PrettyPrinterContext): Unit = {
      pretty(value.name)
      code(": ")
      pretty(value.typ)
    }
  }

  implicit val expressionPrettyPrinter: PrettyPrinter[Expression] = new PrettyPrinter[Expression] {
    override def prettyPrint(value: Expression)(implicit context: PrettyPrinterContext): Unit =
      value match {
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
          pretty(name)
          squareBracketed(index)

        case SystemVariable(name) =>
          code("$")
          pretty(name)

        case Predefined(name) =>
          pretty(name)

        case Apply(function, parameters) =>
          pretty(function)
          parenthesed(parameters)

        case UnaryOp(UnaryOperators.Not, x) =>
          code("not")
          space
          parenthesed(x)

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
          code("(")
          pretty(x); space; code(opStr); space; pretty(y)
          code(")")

        case Lambda(typeParams, paramDefs, returnType, body) =>
          code("fn")
          if (typeParams.nonEmpty) {
            squareBracketed(typeParams)
          }
          space; parenthesed(paramDefs)
          code(": ")
          pretty(returnType)
          newline; indented {
            pretty(body)
          }
          newline; code("end")

      }
  }

  implicit val singleStatementPrettyPrinter: PrettyPrinter[SingleStatement] = new PrettyPrinter[SingleStatement] {
    override def prettyPrint(statement: SingleStatement)(implicit context: PrettyPrinterContext): Unit =
      statement match {
        case VariableDeclaration(name, value) =>
          code("val "); pretty(name); code(" = "); pretty(value)

        case ArrayDeclaration(name, elementType) =>
          code("array"); squareBracketed(elementType); space; pretty(name);

        case FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) =>
          if (properties.inline) {
            code("inline ")
          }
          code("def "); space; pretty(name)
          if (typeParams.nonEmpty) {
            squareBracketed(typeParams)
          }
          parenthesed(paramDefs)
          code(": ")
          pretty(returnType)
          newline; indented {
            pretty(body)
          }
          newline; code("end")

        case Call(function, parameters) =>
          pretty(function)
          parenthesed(parameters)

        case Run(command, parameters) =>
          code("> ")
          pretty(command)
          space
          for ((item, index) <- parameters.zipWithIndex) {
            if (index > 0) {
              space
            }
            pretty(item)
          }

        case If(condition, trueBody, falseBody) =>
          code("if "); pretty(condition); space; code("then"); newline
          indented {
            pretty(trueBody)
            newline
          }
          if (falseBody != Statements.NoOp) {
            code("else"); newline
            indented {
              pretty(falseBody)
            }
            newline
          }
          code("end")

        case While(condition, body) =>
          code("while "); pretty(condition); newline
          indented {
            pretty(body)
          }
          newline; code("end")

        case UpdateVariable(name, value) =>
          pretty(name); code(" <- "); pretty(value);

        case UpdateCell(name, index, value) =>
          pretty(name); squareBracketed(index); code(" <- "); pretty(value);

        case Return(value) =>
          code("return "); pretty(value)
      }
  }

  implicit val statementPrettyPrinter: PrettyPrinter[Statement] = new PrettyPrinter[Statement] {
    override def prettyPrint(value: Statement)(implicit context: PrettyPrinterContext): Unit =
      value match {
        case Single(statement) =>
          pretty(statement)

        case Sequence(first, second) =>
          pretty(first)
          newline
          pretty(second)

        case NoOp =>
      }
  }

  implicit val scriptPrettyPrinter: PrettyPrinter[Script] = new PrettyPrinter[Script] {
    override def prettyPrint(value: Script)(implicit context: PrettyPrinterContext): Unit =
      pretty(value.body)
  }

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
