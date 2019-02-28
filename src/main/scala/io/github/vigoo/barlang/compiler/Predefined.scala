package io.github.vigoo.barlang.compiler

import io.github.vigoo.barlang.compiler.Compiler.predefined
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import io.github.vigoo.barlang.language.Statements.{NoOp, Sequence, Single}
import io.github.vigoo.barlang.language._
import io.github.vigoo.bash.language._
import io.github.vigoo.bc.language.{BcExpressions, BcPrettyPrint}
import io.github.vigoo.bc.language.BcPrettyPrint._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

trait Predefined {
  this: CompilerTypes =>


  type ExpressionCompilerFunction = Expression => (BashExpression => Eff[StatementCompiler, BashStatement]) => Eff[StatementCompiler, BashStatement]


  sealed trait PredefinedExpression

  case class SimplePredefinedExpression(expression: Expression) extends PredefinedExpression

  case class CustomExpression(fn: (ExpressionCompilerFunction, List[TypedExpression]) => Eff[ExpressionCompiler, BashExpression]) extends PredefinedExpression


  case class PredefinedValue(name: SymbolName, typ: Type, value: PredefinedExpression)


  private def predef(name: SymbolName, typ: Type, expression: Expression): (SymbolName, PredefinedValue) =
    name -> PredefinedValue(name, typ, SimplePredefinedExpression(expression))

  private def custom(name: SymbolName, typ: Type, fn: (ExpressionCompilerFunction, List[TypedExpression]) => Eff[ExpressionCompiler, BashExpression]): (SymbolName, PredefinedValue) =
    name -> PredefinedValue(name, typ, CustomExpression(fn))

  private def toInt(expressionCompiler: ExpressionCompilerFunction, params: List[TypedExpression]): Eff[ExpressionCompiler, BashExpression] = {
    params match {
      case List(TypedExpression(SimpleType(Types.Double()), doubleExpression)) =>
        for {
          temporarySymbol1 <- generateTempSymbol[ExpressionCompiler]
          temporarySymbol2 <- generateTempSymbol[ExpressionCompiler]
          _ <- prerequisite(expressionCompiler(doubleExpression) { compiledExpression =>
            pure[StatementCompiler, BashStatement](
              BashStatements.Assign(BashIdentifier(temporarySymbol1.identifier), compiledExpression))
          })
          _ <- prerequisite(pure[StatementCompiler, BashStatement](
            BashStatements.Assign(
              BashIdentifier(temporarySymbol2.identifier),
              BashExpressions.Eval(BashStatements.Command(
                BashExpressions.Literal("printf"),
                List(
                  BashExpressions.Literal("%.0f"),
                  BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(temporarySymbol1.identifier)))
                ))))))
        } yield BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(temporarySymbol2.identifier)))
      case _ =>
        left[ExpressionCompiler, CompilerError, BashExpression](InvalidParameterTypeForPredefined(SymbolName("toInt"), params.map(_.typ)))
    }
  }

  private def bcFunction(name: String, functionName: String)(expressionCompiler: ExpressionCompilerFunction, params: List[TypedExpression]): Eff[ExpressionCompiler, BashExpression] = {
    params match {
      case List(TypedExpression(SimpleType(Types.Double()), doubleExpression)) =>
        for {
          temporarySymbol1 <- generateTempSymbol[ExpressionCompiler]
          _ <- prerequisite(expressionCompiler(doubleExpression) { compiledExpression =>
            pure[StatementCompiler, BashStatement](
              BashStatements.Assign(BashIdentifier(temporarySymbol1.identifier), compiledExpression))
          })
          temporarySymbol2 <- generateTempSymbol[ExpressionCompiler]
          _ <- prerequisite(pure[StatementCompiler, BashStatement](
            BashStatements.Assign(
              BashIdentifier(temporarySymbol2.identifier),
              BashExpressions.Eval(BashStatements.Command(
                name = BashExpressions.Literal("bc"),
                params = List(
                  BashExpressions.Literal("-l")
                ),
                hereString = Some(
                  BashExpressions.Literal(BcPrettyPrint.print(
                    BcExpressions.FunctionCall(functionName,
                      BcExpressions.BashVariable(BashIdentifier(temporarySymbol1.identifier))))))
              ))
            )
          ))
        } yield BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(temporarySymbol2.identifier)))
      case _ =>
        left[ExpressionCompiler, CompilerError, BashExpression](InvalidParameterTypeForPredefined(SymbolName(name), params.map(_.typ)))
    }
  }

  private def str(expressionCompiler: ExpressionCompilerFunction, params: List[TypedExpression]): Eff[ExpressionCompiler, BashExpression] = {
    def literalString(value: String) =
      pure[ExpressionCompiler, BashExpression](BashExpressions.Literal(value))

    def strVariable(typ: ExtendedType, assignedSymbol: AssignedSymbol): Eff[ExpressionCompiler, BashExpression] =
      typ match {
        case SimpleType(Types.String()) =>
          pure[ExpressionCompiler, BashExpression](BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(assignedSymbol.identifier))))
        case SimpleType(Types.Int()) =>
          pure[ExpressionCompiler, BashExpression](BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(assignedSymbol.identifier))))
        case SimpleType(Types.Double()) =>
          pure[ExpressionCompiler, BashExpression](BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(assignedSymbol.identifier))))
        case SimpleType(Types.Bool()) =>
          for {
            temporarySymbol <- generateTempSymbol[ExpressionCompiler]
            conditionExpression = BashExpressions.Conditional(BashConditions.StringEquals(
              BashConditions.Variable(BashVariables.Variable(BashIdentifier(assignedSymbol.identifier))),
              BashConditions.Literal("0")))
            onTrue = BashStatements.Assign(
              BashIdentifier(temporarySymbol.identifier),
              BashExpressions.Literal("true")
            )
            onFalse = BashStatements.Assign(
              BashIdentifier(temporarySymbol.identifier),
              BashExpressions.Literal("false")
            )
            _ <- prerequisite(pure[StatementCompiler, BashStatement](
              BashStatements.IfThenElse(conditionExpression, onTrue, onFalse)
            ))
          } yield BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(temporarySymbol.identifier)))
        case _ =>
          left[ExpressionCompiler, CompilerError, BashExpression](InvalidParameterTypeForPredefined(SymbolName("str"), List(typ)))
      }

    params match {
      case List(TypedExpression(SimpleType(Types.String()), StringLiteral(value))) =>
        literalString(value)
      case List(TypedExpression(SimpleType(Types.Bool()), BoolLiteral(true))) =>
        literalString("true")
      case List(TypedExpression(SimpleType(Types.Bool()), BoolLiteral(false))) =>
        literalString("false")
      case List(TypedExpression(SimpleType(Types.Int()), IntLiteral(value))) =>
        literalString(value.toString)
      case List(TypedExpression(SimpleType(Types.Double()), DoubleLiteral(value))) =>
        literalString(value.toString)
      case List(TypedExpression(typ, Variable(symbol))) =>
        for {
          maybeVariable <- findSymbol[ExpressionCompiler](symbol)
          result <- maybeVariable match {
            case Some(assignedSymbol) =>
              strVariable(typ, assignedSymbol)
            case None =>
              left[ExpressionCompiler, CompilerError, BashExpression](UndefinedSymbol(symbol))
          }
        } yield result
      case List(TypedExpression(typ, expression)) =>
        for {
          temporarySymbol <- generateTempSymbol[ExpressionCompiler]
          _ <- prerequisite(expressionCompiler(expression) { compiledExpression =>
            pure[StatementCompiler, BashStatement](
              BashStatements.Assign(BashIdentifier(temporarySymbol.identifier), compiledExpression))
          })
          result <- strVariable(typ, temporarySymbol)
        } yield result
    }
  }

  val predefined: Map[SymbolName, PredefinedValue] = Map(
    predef(SymbolName("pi"), Types.Double(), DoubleLiteral(math.Pi)),
    custom(SymbolName("toInt"), Types.Function(List.empty, List(Types.Double()), Types.Int()), toInt),
    custom(SymbolName("str"), Types.Function(List(TypeParam(SymbolName("T"))), List(Types.TypeVariable(SymbolName("T"))), Types.String()), str),
    custom(SymbolName("sin"), Types.Function(List.empty, List(Types.Double()), Types.Double()), bcFunction("sin", "s")),
    custom(SymbolName("cos"), Types.Function(List.empty, List(Types.Double()), Types.Double()), bcFunction("cos", "c")),
  )

  def isCustomPredefinedExpression(symbol: SymbolName): Boolean =
    predefined.get(symbol) match {
      case Some(PredefinedValue(_, _, CustomExpression(_))) => true
      case _ => false
    }

  def replacePredefs(statement: SingleStatement): SingleStatement = statement match {
    case VariableDeclaration(name, props, value) =>
      VariableDeclaration(name, props, replacePredefs(value))
    case FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) =>
      FunctionDefinition(name, properties, typeParams, paramDefs, returnType, replacePredefs(body))
    case Call(function, parameters) =>
      Call(function, parameters.map(replacePredefs))
    case Run(command, parameters) =>
      Run(replacePredefs(command), parameters.map(replacePredefs))
    case If(condition, trueBody, falseBody) =>
      If(replacePredefs(condition), replacePredefs(trueBody), replacePredefs(falseBody))
    case While(condition, body) =>
      While(replacePredefs(condition), replacePredefs(body))
    case UpdateVariable(name, value) =>
      UpdateVariable(name, replacePredefs(value))
    case UpdateCell(name, index, value) =>
      UpdateCell(name, replacePredefs(index), replacePredefs(value))
    case ArrayDeclaration(name, elementType) =>
      ArrayDeclaration(name, elementType)
    case Return(value) =>
      Return(replacePredefs(value))
  }

  def replacePredefs(statement: Statement): Statement = statement match {
    case Single(singleStatement) =>
      Single(replacePredefs(singleStatement))
    case Sequence(first, second) =>
      Sequence(replacePredefs(first), replacePredefs(second))
    case NoOp =>
      NoOp
  }

  def replacePredefs(expression: Expression): Expression = {
    expression match {
      case Variable(name) if predefined.contains(name) => Predefined(name)
      case ArrayAccess(name, index) => ArrayAccess(name, replacePredefs(index))
      case Apply(function, parameters) => Apply(replacePredefs(function), parameters.map(replacePredefs))
      case UnaryOp(operator, x) => UnaryOp(operator, replacePredefs(x))
      case BinaryOp(operator, x, y) => BinaryOp(operator, replacePredefs(x), replacePredefs(y))
      case Lambda(typeParams, paramDefs, returnType, body) => Lambda(typeParams, paramDefs, returnType, replacePredefs(body))
      case _ => expression
    }
  }
}
