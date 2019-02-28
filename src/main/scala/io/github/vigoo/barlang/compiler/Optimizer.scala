package io.github.vigoo.barlang.compiler

import cats.syntax.monoid._
import cats.data.State
import io.github.vigoo.barlang.language.Expressions.Variable
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import io.github.vigoo.barlang.language._

trait Optimizer {

  def optimize(script: Script): Script = {
    optimizeScript(script).evalState(initialState).run
  }

  case class DefinedFunction(name: SymbolName,
                             typeParams: List[TypeParam],
                             paramDefs: List[ParamDef],
                             returnType: Type,
                             body: Statement,
                             inlined: Boolean)

  case class OptimizerState(functionMap: Map[SymbolName, DefinedFunction])

  private val initialState: OptimizerState = OptimizerState(Map.empty)

  private type ScriptOptimizer = Fx.fx1[State[OptimizerState, ?]]

  private def optimizeScript(script: Script): Eff[ScriptOptimizer, Script] = {
    for {
      optimizedBody <- optimizeStatement(script.body)
    } yield Script(body = optimizedBody)
  }

  private def optimizeStatement(statement: Statement): Eff[ScriptOptimizer, Statement] = {
    statement match {
      case Statements.Single(singleStatement) =>
        for {
          optimizedStatement <- optimizeSingleStatement(singleStatement)
        } yield optimizedStatement
      case Statements.Sequence(first, second) =>
        for {
          optimizedFirst <- optimizeSingleStatement(first)
          optimizedSecond <- optimizeStatement(second)
        } yield optimizedFirst |+| optimizedSecond
      case Statements.NoOp =>
        pure(Statements.NoOp)
    }
  }

  private def isAutoInlineable(statement: Statement): Boolean = {
    statement.toList match {
      case List(SingleStatements.Return(_)) => false
      case List(_) => true
      case _ => false
    }
  }

  private def defineFunction(name: SymbolName,
                             properties: FunctionProperties,
                             typeParams: List[TypeParam],
                             paramDefs: List[ParamDef],
                             returnType: Type,
                             body: Statement): Eff[ScriptOptimizer, DefinedFunction] = {

    val definedFunction = DefinedFunction(
      name,
      typeParams,
      paramDefs,
      returnType,
      body,
      inlined = properties.inline || isAutoInlineable(body),
    )

    for {
      _ <- modify[ScriptOptimizer, OptimizerState] { state =>
        state.copy(functionMap = state.functionMap + (name -> definedFunction))
      }
    } yield definedFunction
  }

  private def optimizeSingleStatement(statement: SingleStatement): Eff[ScriptOptimizer, Statement] = {
    statement match {
      case SingleStatements.FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) =>
        for {
          optimizedBody <- optimizeStatement(body)
          definedFunction <- defineFunction(name, properties, typeParams, paramDefs, returnType, optimizedBody)
          result = if (definedFunction.inlined) {
            Statements.NoOp
          } else {
            Statements.Single(SingleStatements.FunctionDefinition(name, properties, typeParams, paramDefs, returnType, optimizedBody))
          }
        } yield result

      case call@SingleStatements.Call(Variable(name), parameters) =>
        for {
          state <- get[ScriptOptimizer, OptimizerState]
          result <- state.functionMap.get(name) match {
            case Some(definedFunction) if definedFunction.inlined =>
              pure[ScriptOptimizer, Statement](inlinedFunctionApplication(definedFunction, parameters))
            case _ =>
              pure[ScriptOptimizer, Statement](Statements.Single(call))
          }
        } yield result
      case SingleStatements.If(condition, trueBody, falseBody) =>
        for {
          optimizedTrue <- optimizeStatement(trueBody)
          optimizedFalse <- optimizeStatement(falseBody)
        } yield Statements.Single(SingleStatements.If(condition, optimizedTrue, optimizedFalse))
      case SingleStatements.While(condition, body) =>
        for {
          optimizedBody <- optimizeStatement(body)
        } yield Statements.Single(SingleStatements.While(condition, optimizedBody))
      case _ =>
        pure[ScriptOptimizer, Statement](Statements.Single(statement))
    }
  }

  private def replaceSubExpression(in: Expression, from: Expression, to: Expression): Expression = {
    if (in == from) {
      to
    } else {
      in match {
        case Expressions.ArrayAccess(name, index) =>
          Expressions.ArrayAccess(name, replaceSubExpression(index, from, to))
        case Expressions.Apply(function, parameters) =>
          Expressions.Apply(
            replaceSubExpression(function, from, to),
            parameters.map(e => replaceSubExpression(e, from, to))
          )
        case Expressions.UnaryOp(operator, x) =>
          Expressions.UnaryOp(operator, replaceSubExpression(x, from, to))
        case Expressions.BinaryOp(operator, x, y) =>
          Expressions.BinaryOp(
            operator,
            replaceSubExpression(x, from, to),
            replaceSubExpression(y, from, to))
        case Expressions.Lambda(typeParams, paramDefs, returnType, body) =>
          Expressions.Lambda(typeParams, paramDefs, returnType, replaceExpression(body, from, to))
        case _ =>
          in
      }
    }
  }

  private def replaceExpressionInStatement(in: SingleStatement, from: Expression, to: Expression): SingleStatement = {
    in match {
      case SingleStatements.VariableDeclaration(name, props, value) =>
        SingleStatements.VariableDeclaration(name, props, replaceSubExpression(value, from, to))
      case SingleStatements.Call(function, parameters) =>
        SingleStatements.Call(
          replaceSubExpression(function, from, to),
          parameters.map(e => replaceSubExpression(e, from, to))
        )
      case SingleStatements.Run(command, parameters) =>
        SingleStatements.Run(
          replaceSubExpression(command, from, to),
          parameters.map(e => replaceSubExpression(e, from, to))
        )
      case SingleStatements.If(condition, trueBody, falseBody) =>
        SingleStatements.If(
          replaceSubExpression(condition, from, to),
          replaceExpression(trueBody, from, to),
          replaceExpression(falseBody, from, to)
        )
      case SingleStatements.While(condition, body) =>
        SingleStatements.While(
          replaceSubExpression(condition, from, to),
          replaceExpression(body, from, to)
        )
      case SingleStatements.UpdateVariable(name, value) =>
        SingleStatements.UpdateVariable(name, replaceSubExpression(value, from, to))
      case SingleStatements.UpdateCell(name, index, value) =>
        SingleStatements.UpdateCell(
          name,
          replaceSubExpression(index, from, to),
          replaceSubExpression(value, from, to))
      case SingleStatements.Return(value) =>
        SingleStatements.Return(replaceSubExpression(value, from, to))
      case _ =>
        in
    }
  }

  private def replaceExpression(in: Statement, from: Expression, to: Expression): Statement = {
    in match {
      case Statements.Single(statement) =>
        Statements.Single(replaceExpressionInStatement(statement, from, to))
      case Statements.Sequence(first, second) =>
        Statements.Sequence(
          replaceExpressionInStatement(first, from, to),
          replaceExpression(second, from, to))
      case Statements.NoOp =>
        Statements.NoOp
    }
  }

  private def inlinedFunctionApplication(function: DefinedFunction, expressions: List[Expression]): Statement = {
    (function.paramDefs zip expressions).foldLeft(function.body) { case (statement, (paramDef, paramExpr)) =>
      replaceExpression(statement, Variable(paramDef.name), paramExpr)
    }
  }
}

object Optimizer extends Optimizer