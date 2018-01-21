package io.github.vigoo.barlang.compiler

import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language._
import io.github.vigoo.bash.language._
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
    /*
    bcfun _ fun compileExpression [TypedExpression (SimpleType TDouble) expr] = do
      tmpSym <- generateTmpSym
      prereq $ compileExpression expr $
        \cExpr -> return $ SH.Assign $ SH.Var (asId tmpSym) cExpr

      tmpSym' <- generateTmpSym
      prereq $ return $ SH.Sequence (SH.Annotated (SH.Lines [(asIdString tmpSym') <> "=$(bc -l <<< \"" <> B.fromString fun <> "($" <> (asIdString tmpSym) <> ")\")"] []) SH.Empty) (noAnnotation SH.Empty)
      return (SH.ReadVar (SH.VarIdent $ asId tmpSym'))

    bcfun n _ _ ps = throwError $ InvalidParameterTypeForPredefined n (map texpType ps)
     */

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
                hereString = Some(BashExpressions.Interpolated(
                  List(
                    BashExpressions.Literal(functionName),
                    BashExpressions.Literal("("),
                    BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(temporarySymbol1.identifier))),
                    BashExpressions.Literal(")")
                  )
                ))
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
        case SimpleType(Types.Bool()) =>
          for {
            temporarySymbol <- generateTempSymbol[ExpressionCompiler]
            conditionExpression = BashExpressions.Conditional(BashConditions.Equals(
              BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(assignedSymbol.identifier))),
              BashExpressions.Literal("0")))
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
}
