package io.github.vigoo.barlang.compiler

import io.github.vigoo.barlang.language.Expressions.DoubleLiteral
import io.github.vigoo.barlang.language.{Expression, SymbolName, Type, Types}
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

  val predefined: Map[SymbolName, PredefinedValue] = Map(
    predef(SymbolName("pi"), Types.Double(), DoubleLiteral(math.Pi)),
    custom(SymbolName("toInt"), Types.Function(List.empty, List(Types.Double()), Types.Int()), toInt)
  )
}
