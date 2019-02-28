package io.github.vigoo.barlang.compiler

import cats.{Monoid, Traverse}
import cats.instances.list._
import cats.syntax.traverse._
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import io.github.vigoo.barlang.language.Statements.{NoOp, Sequence, Single}
import io.github.vigoo.barlang.language._
import io.github.vigoo.bash.language.BashPrettyPrint._
import io.github.vigoo.bash.language._
import io.github.vigoo.bc.language.{BcExpression, BcExpressions}
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scala.language.higherKinds

object Compiler extends TypeChecker with CompilerTypes with Predefined {

  def initialContext: Context =
    Context(
      scope = GlobalScope,
      symbols = Map.empty,
      symbolTypes = predefined.map { case (name, predefinedValue) => name -> SimpleType(predefinedValue.typ) },
      mutability = Map.empty,
      lastTmp = 0
    )

  def compileToString(script: Script): CompilerResult[String] = {
    compile(script).runEither.evalState(initialContext).run.map(bashStatement => print(bashStatement))
  }

  def compile(script: Script): Eff[StatementCompiler, BashStatement] = {
    val replacedBody = replacePredefs(script.body)
    for {
      _ <- typeCheckStatement(replacedBody)
      // TODO: header, shebang, set flags
      compiled <- compileStatement(replacedBody)
    } yield compiled
  }

  def compileDoubleBinaryOperator(x: Expression, y: Expression, constructor: (BcExpression, BcExpression) => BcExpression): Eff[ExpressionCompiler, BcExpression] = {
    for {
      compiledX <- compileDoubleExpression(x)
      compiledY <- compileDoubleExpression(y)
    } yield constructor(compiledX, compiledY)
  }

  def compileExtractedDoubleExpression(expression: Expression): Eff[ExpressionCompiler, BcExpression] = {
    for {
      compiledExpr <- compile(expression)
      tempSymbol <- generateTempSymbol[ExpressionCompiler]
      tempId = BashIdentifier(tempSymbol.identifier)
      _ <- prerequisite(pure[StatementCompiler, BashStatement](
        BashStatements.Assign( // TODO: use local
          tempId,
          compiledExpr
        )))
    } yield BcExpressions.BashVariable(tempId)
  }

  def compileDoubleExpression(expression: Expression): Eff[ExpressionCompiler, BcExpression] = {
    def r = pure[ExpressionCompiler, BcExpression] _

    expression match {
      case IntLiteral(value) => r(BcExpressions.Number(value.toDouble))
      case DoubleLiteral(value) => r(BcExpressions.Number(value))
      case BinaryOp(BinaryOperators.Add, x, y) => compileDoubleBinaryOperator(x, y, BcExpressions.Add)
      case BinaryOp(BinaryOperators.Sub, x, y) => compileDoubleBinaryOperator(x, y, BcExpressions.Sub)
      case BinaryOp(BinaryOperators.Mul, x, y) => compileDoubleBinaryOperator(x, y, BcExpressions.Mul)
      case BinaryOp(BinaryOperators.Div, x, y) => compileDoubleBinaryOperator(x, y, BcExpressions.Div)
      case Apply(_, _) => compileExtractedDoubleExpression(expression)
      case ArrayAccess(_, _) => compileExtractedDoubleExpression(expression)
      case Predefined(name) =>
        predefined.get(name) match {
          case Some(PredefinedValue(_, _, SimplePredefinedExpression(predefExpr))) => compileExtractedDoubleExpression(predefExpr)
          case Some(_) => left[ExpressionCompiler, CompilerError, BcExpression](InvalidUseOfPredefinedFunction(name))
          case None => left[ExpressionCompiler, CompilerError, BcExpression](UndefinedSymbol(name))
        }
      case Variable(name) =>
        for {
          existingSymbol <- findSymbol[ExpressionCompiler](name)
          result <- existingSymbol match {
            case Some(assignedSym) =>
              r(BcExpressions.BashVariable(BashIdentifier(assignedSym.identifier)))
            case None =>
              left[ExpressionCompiler, CompilerError, BcExpression](UndefinedSymbol(name))
          }
        } yield result
      case _ => left[ExpressionCompiler, CompilerError, BcExpression](UnsupportedExpression(expression))
    }
  }

  def doubleExpressionToTempVar(expression: Expression): Eff[ExpressionCompiler, AssignedSymbol] = {
    import io.github.vigoo.bc.language.BcPrettyPrint._

    for {
      bcExpression <- compileDoubleExpression(expression)
      compiledExpr = BashExpressions.Eval(
        BashStatements.Command(
          BashExpressions.Literal("bc"),
          List(BashExpressions.Literal("-l")),
          Some(BashExpressions.Literal(print(bcExpression)))))
      tempSymbol <- generateTempSymbol[ExpressionCompiler]
      _ <- prerequisite(pure[StatementCompiler, BashStatement](
        BashStatements.Assign( // TODO: use local
          BashIdentifier(tempSymbol.identifier),
          compiledExpr
        )))
    } yield tempSymbol
  }

  def compileIntegerExpression(expression: Expression): Eff[ExpressionCompiler, BashArithmeticExpression] = {
    def r = pure[ExpressionCompiler, BashArithmeticExpression] _

    def binaryIntegerOp(x: Expression, y: Expression, createOp: (BashArithmeticExpression, BashArithmeticExpression) => BashArithmeticExpression): Eff[ExpressionCompiler, BashArithmeticExpression] = {
      for {
        compiledX <- compileIntegerExpression(x)
        compiledY <- compileIntegerExpression(y)
      } yield createOp(compiledX, compiledY)
    }

    expression match {
      case IntLiteral(value) => r(BashArithmeticExpressions.Number(value))
      case BinaryOp(BinaryOperators.Add, x, y) => binaryIntegerOp(x, y, BashArithmeticExpressions.Add.apply)
      case BinaryOp(BinaryOperators.Sub, x, y) => binaryIntegerOp(x, y, BashArithmeticExpressions.Sub.apply)
      case BinaryOp(BinaryOperators.Mul, x, y) => binaryIntegerOp(x, y, BashArithmeticExpressions.Mul.apply)
      case BinaryOp(BinaryOperators.Div, x, y) => binaryIntegerOp(x, y, BashArithmeticExpressions.Div.apply)
      case BinaryOp(BinaryOperators.Mod, x, y) => binaryIntegerOp(x, y, BashArithmeticExpressions.Rem.apply)
      case Apply(_, _) =>
        for {
          compiledExpr <- compile(expression)
          tempSymbol <- generateTempSymbol[ExpressionCompiler]
          _ <- prerequisite(pure[StatementCompiler, BashStatement](
            BashStatements.Assign( // TODO: use local
              BashIdentifier(tempSymbol.identifier),
              compiledExpr
            )))
        } yield BashArithmeticExpressions.Variable(BashVariables.Variable(BashIdentifier(tempSymbol.identifier)))
      case Variable(name) =>
        for {
          existingSymbol <- findSymbol[ExpressionCompiler](name)
          result <- existingSymbol match {
            case Some(assignedSym) =>
              r(BashArithmeticExpressions.Variable(BashVariables.Variable(BashIdentifier(assignedSym.identifier))))
            case None =>
              left[ExpressionCompiler, CompilerError, BashArithmeticExpression](UndefinedSymbol(name))
          }
        } yield result
      case _ => left[ExpressionCompiler, CompilerError, BashArithmeticExpression](UnsupportedExpression(expression))
    }
  }

  def integerExpressionToTempVar(expression: Expression): Eff[ExpressionCompiler, AssignedSymbol] = {
    for {
      arithmeticExpression <- compileIntegerExpression(expression)
      compiledExpr = BashExpressions.EvalArithmetic(arithmeticExpression)
      tempSymbol <- generateTempSymbol[ExpressionCompiler]
      _ <- prerequisite(pure[StatementCompiler, BashStatement](
        BashStatements.Assign( // TODO: use local
          BashIdentifier(tempSymbol.identifier),
          compiledExpr
        )))
    } yield tempSymbol

  }

  def readTempVar[R: _compilerState : _compilerResult](symbol: AssignedSymbol): Eff[R, BashExpression] =
    pure[R, BashExpression](BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(symbol.identifier))))

  def numericBinaryExpr(op: BinaryOp): Eff[ExpressionCompiler, Option[BashExpression]] = {
    def r = pure[ExpressionCompiler, Option[BashExpression]] _

    for {
      typeX <- typeCheckExpression[ExpressionCompiler](op.x)
      typeY <- typeCheckExpression[ExpressionCompiler](op.y)
      result <- (typeX, typeY) match {
        case (SimpleType(Types.String()), SimpleType(Types.String())) => r(None)
        case (SimpleType(Types.Double()), _) => doubleExpressionToTempVar(op).flatMap(readTempVar[ExpressionCompiler]).map(Some.apply)
        case (_, SimpleType(Types.Double())) => doubleExpressionToTempVar(op).flatMap(readTempVar[ExpressionCompiler]).map(Some.apply)
        case _ => integerExpressionToTempVar(op).flatMap(readTempVar[ExpressionCompiler]).map(Some.apply)
      }
    } yield result
  }

  def compileNumericSubExpression(expression: Expression): Eff[ExpressionCompiler, Option[BashExpression]] = {
    def r = pure[ExpressionCompiler, Option[BashExpression]] _

    expression match {
      case op@BinaryOp(BinaryOperators.Add, _, _) => numericBinaryExpr(op)
      case op@BinaryOp(BinaryOperators.Sub, _, _) => numericBinaryExpr(op)
      case op@BinaryOp(BinaryOperators.Mul, _, _) => numericBinaryExpr(op)
      case op@BinaryOp(BinaryOperators.Div, _, _) => numericBinaryExpr(op)
      case op@BinaryOp(BinaryOperators.Mod, _, _) => numericBinaryExpr(op)
      case IntLiteral(value) =>
        r(Some(
          BashExpressions.Literal(value.toString)
        ))
      case DoubleLiteral(value) =>
        r(Some(
          BashExpressions.Literal(value.toString)
        ))
      case _ =>
        r(None)
    }
  }

  def binaryCondition(x: Expression, y: Expression, factory: (BashCondition, BashCondition) => BashCondition): Eff[ExpressionCompiler, BashCondition] =
    for {
      compiledX <- compileCondition(x)
      compiledY <- compileCondition(y)
    } yield factory(compiledX, compiledY)

  def compileCondition(expression: Expression): Eff[ExpressionCompiler, BashCondition] = {
    def r = pure[ExpressionCompiler, BashCondition] _

    expression match {
      case BoolLiteral(true) => r(BashConditions.Literal("TRUE"))
      case BoolLiteral(false) => r(BashConditions.Literal(""))
      case StringLiteral(value) => r(BashConditions.Literal(value))
      case IntLiteral(value) => r(BashConditions.Literal(value.toString))
      case DoubleLiteral(value) => r(BashConditions.Literal(value.toString))
      case Variable(name) =>
        for {
          existingSymbol <- findSymbol[ExpressionCompiler](name)
          existingType <- findType[ExpressionCompiler](name)
          result <- (existingSymbol, existingType) match {
            case (Some(symbol), Some(SimpleType(Types.Bool()))) =>
              r(BashConditions.StringEquals(
                BashConditions.Variable(BashVariables.Variable(BashIdentifier(symbol.identifier))),
                BashConditions.Literal("0")))
            case (Some(symbol), Some(SimpleType(Types.Int()))) =>
              r(BashConditions.Variable(BashVariables.Variable(BashIdentifier(symbol.identifier))))
            case (Some(symbol), Some(SimpleType(Types.Double()))) =>
              r(BashConditions.Variable(BashVariables.Variable(BashIdentifier(symbol.identifier))))
            case (Some(symbol), Some(SimpleType(Types.String()))) =>
              r(BashConditions.Variable(BashVariables.Variable(BashIdentifier(symbol.identifier))))
            case (Some(_), Some(typ)) =>
              left[ExpressionCompiler, CompilerError, BashCondition](UnsupportedTypeInBooleanExpression(name, typ))
            case _ =>
              left[ExpressionCompiler, CompilerError, BashCondition](UndefinedSymbol(name))
          }
        } yield result
      case UnaryOp(UnaryOperators.Not, x) =>
        for {
          compiledX <- compileCondition(x)
        } yield BashConditions.Not(compiledX)
      case BinaryOp(BinaryOperators.Eq, x, y) =>
        for {
          typeX <- typeCheckExpression[ExpressionCompiler](x)
          typeY <- typeCheckExpression[ExpressionCompiler](y)
          op = (typeX, typeY) match {
            case (SimpleType(Types.String()), SimpleType(Types.String())) => BashConditions.StringEquals
            case _ => BashConditions.Equals
          }
          result <- binaryCondition(x, y, op)
        } yield result
      case BinaryOp(BinaryOperators.Neq, x, y) =>
        for {
          typeX <- typeCheckExpression[ExpressionCompiler](x)
          typeY <- typeCheckExpression[ExpressionCompiler](y)
          op = (typeX, typeY) match {
            case (SimpleType(Types.String()), SimpleType(Types.String())) => BashConditions.StringNotEquals
            case _ => BashConditions.NotEquals
          }
          result <- binaryCondition(x, y, op)
        } yield result
      case BinaryOp(BinaryOperators.Less, x, y) =>
        binaryCondition(x, y, BashConditions.Less)
      case BinaryOp(BinaryOperators.LessEq, x, y) =>
        binaryCondition(x, y, BashConditions.LessEq)
      case BinaryOp(BinaryOperators.Greater, x, y) =>
        binaryCondition(x, y, BashConditions.Greater)
      case BinaryOp(BinaryOperators.GreaterEq, x, y) =>
        binaryCondition(x, y, BashConditions.GreaterEq)
      case BinaryOp(BinaryOperators.And, x, y) =>
        binaryCondition(x, y, BashConditions.And)
      case BinaryOp(BinaryOperators.Or, x, y) =>
        binaryCondition(x, y, BashConditions.Or)
      case _ =>
        for {
          compiledExpr <- compile(expression)
          tempSymbol <- generateTempSymbol[ExpressionCompiler]
          _ <- prerequisite(pure[StatementCompiler, BashStatement](
            BashStatements.Assign( // TODO: use local
              BashIdentifier(tempSymbol.identifier),
              compiledExpr
            )))
        } yield BashConditions.Variable(BashVariables.Variable(BashIdentifier(tempSymbol.identifier)))
    }
  }

  def compileBooleanExpression(expression: Expression): Eff[ExpressionCompiler, BashExpression] = {
    def r = pure[ExpressionCompiler, BashExpression] _

    expression match {
      case BoolLiteral(true) =>
        r(BashExpressions.True)
      case BoolLiteral(false) =>
        r(BashExpressions.False)
      case Variable(name) =>
        for {
          existingSymbol <- findSymbol[ExpressionCompiler](name)
          result <- existingSymbol match {
            case Some(symbol) =>
              r(BashExpressions.Eval(BashStatements.Command(
                BashExpressions.Literal("exit"),
                List(
                  BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(symbol.identifier)))
                )
              )))
            case None =>
              left[ExpressionCompiler, CompilerError, BashExpression](UndefinedSymbol(name))
          }
        } yield result
      case BinaryOp(BinaryOperators.And, x, y) =>
        for {
          compiledX <- compileBooleanExpression(x)
          compiledY <- compileBooleanExpression(y)
        } yield BashExpressions.And(compiledX, compiledY)
      case BinaryOp(BinaryOperators.Or, x, y) =>
        for {
          compiledX <- compileBooleanExpression(x)
          compiledY <- compileBooleanExpression(y)
        } yield BashExpressions.Or(compiledX, compiledY)

      case _ =>
        for {
          condition <- compileCondition(expression)
        } yield BashExpressions.Conditional(condition)
    }
  }

  def booleanExpressionToTempVar(expression: Expression): Eff[ExpressionCompiler, AssignedSymbol] = {
    for {
      compiledExpr <- compileBooleanExpression(expression)
      tempSymbol <- generateTempSymbol[ExpressionCompiler]
      _ <- prerequisite(pure[StatementCompiler, BashStatement](
        BashStatements.Assign( // TODO: use local
          BashIdentifier(tempSymbol.identifier),
          compiledExpr
        )))
    } yield tempSymbol

  }

  def compileBooleanSubExpression(expression: Expression): Eff[ExpressionCompiler, Option[BashExpression]] = {
    def r = pure[ExpressionCompiler, Option[BashExpression]] _

    def compileAndReturn(): Eff[ExpressionCompiler, Option[BashExpression]] =
        compileBooleanExpression(expression).map(Some.apply)

    expression match {
      case op@UnaryOp(UnaryOperators.Not, _) => compileAndReturn()
      case op@BinaryOp(BinaryOperators.And, _, _) => compileAndReturn()
      case op@BinaryOp(BinaryOperators.Or, _, _) => compileAndReturn()
      case op@BinaryOp(BinaryOperators.Eq, _, _) => compileAndReturn()
      case op@BinaryOp(BinaryOperators.Neq, _, _) => compileAndReturn()
      case op@BinaryOp(BinaryOperators.Less, _, _) => compileAndReturn()
      case op@BinaryOp(BinaryOperators.LessEq, _, _) => compileAndReturn()
      case op@BinaryOp(BinaryOperators.Greater, _, _) => compileAndReturn()
      case op@BinaryOp(BinaryOperators.GreaterEq, _, _) => compileAndReturn()
      case _ => r(None)
    }
  }

  def compileStringSubExpression(expression: Expression): Eff[ExpressionCompiler, Option[BashExpression]] = {
    val noResult = pure[ExpressionCompiler, Option[BashExpression]](None)

    expression match {
      case BinaryOp(BinaryOperators.Add, x, y) =>
        for {
          typeX <- typeCheckExpression[ExpressionCompiler](x)
          typeY <- typeCheckExpression[ExpressionCompiler](y)

          result <- (typeX, typeY) match {
            case (SimpleType(Types.String()), SimpleType(Types.String())) =>
              for {
                compiledX <- compile(x)
                compiledY <- compile(y)
              } yield Some(BashExpressions.Interpolated(List(compiledX, compiledY)))
            case _ =>
              noResult
          }
        } yield result

      case _ =>
        noResult
    }
  }

  def compile(expression: Expression): Eff[ExpressionCompiler, BashExpression] = {
    def r(bashExpr: BashExpression): Eff[ExpressionCompiler, BashExpression] =
      pure[ExpressionCompiler, BashExpression](bashExpr)

    expression match {
      case StringLiteral(value) =>
        r(BashExpressions.Literal(value))

      case BoolLiteral(true) =>
        r(BashExpressions.Literal("0"))
      case BoolLiteral(false) =>
        r(BashExpressions.Literal("1"))

      case Variable(name) =>
        for {
          existingSymbol <- findSymbol[ExpressionCompiler](name)
          symbolType <- findType[ExpressionCompiler](name)
          result <- existingSymbol match {
            case Some(symbol) =>
              symbolType match {
                case Some(SimpleType(Types.Function(_, _, _))) =>
                  // Function reference by name
                  r(BashExpressions.Literal(symbol.identifier))
                case _ =>
                  r(BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(symbol.identifier))))
              }
            case None =>
              left[ExpressionCompiler, CompilerError, BashExpression](UndefinedSymbol(name))
          }
        } yield result

      case ArrayAccess(name, index) =>
        for {
          existingSymbol <- findSymbol[ExpressionCompiler](name)
          result <- existingSymbol match {
            case Some(arraySymbol) =>
              for {
                compiledIndex <- compile(index)
                tempSymbol <- generateTempSymbol[ExpressionCompiler]
                _ <- prerequisite(pure[StatementCompiler, BashStatement](
                  BashStatements.Declare(
                    Set(BashDeclareOptions.Array/*, BashDeclareOptions.ReadOnly*/), // TODO: is ReadOnly invalid here?
                    BashIdentifier(tempSymbol.identifier),
                    Some(BashExpressions.ReadArray(
                      BashVariables.Variable(BashIdentifier(arraySymbol.identifier)),
                      BashArrayIndices.All)))))
              } yield BashExpressions.ReadArray(
                BashVariables.Variable(BashIdentifier(tempSymbol.identifier)),
                BashArrayIndices.Index(compiledIndex))
            case None =>
              left[ExpressionCompiler, CompilerError, BashExpression](UndefinedSymbol(name))
          }
        } yield result

      case SystemVariable(SymbolName(name)) =>
        r(BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(name))))

      case Predefined(name) =>
        predefined.get(name) match {
          case Some(PredefinedValue(_, _, SimplePredefinedExpression(expr))) =>
            compile(expr)
          case Some(PredefinedValue(_, _, CustomExpression(_))) =>
            left[ExpressionCompiler, CompilerError, BashExpression](InvalidUseOfPredefinedFunction(name))
          case None =>
            left[ExpressionCompiler, CompilerError, BashExpression](UndefinedSymbol(name))
        }

      case Apply(Predefined(symbol), parameters) if isCustomPredefinedExpression(symbol) =>
        predefined.get(symbol) match {
          case Some(PredefinedValue(_, _, CustomExpression(fn))) =>
            for {
              parameterTypes <- parameters.traverse[Eff[ExpressionCompiler, ?], ExtendedType](
                param => typeCheckExpression[ExpressionCompiler](param))
              result <- fn(
                expression => factory => compileExpression(expression)(factory),
                parameters.zip(parameterTypes).map { case (param, paramType) => TypedExpression(paramType, param) }
              )
            } yield result
          case _ =>
            left[ExpressionCompiler, CompilerError, BashExpression](IllegalState(s"isCustomPredefinedExpression failure"))
        }

      case Apply(functionRefExpression, parameters) =>
        for {
          functionReference <- compile(functionRefExpression)
          compiledParams <- parameters.traverse[Eff[ExpressionCompiler, ?], BashExpression](compile)
          tempSymbol <- generateTempSymbol[ExpressionCompiler]
          _ <- prerequisite(pure[StatementCompiler, BashStatement](
            BashStatements.Sequence(List(
              BashStatements.Assign(BashIdentifier(tempSymbol.identifier), BashExpressions.Literal("")), // TODO: use local
              BashStatements.Command(functionReference, BashExpressions.Literal(tempSymbol.identifier) :: compiledParams)
            ))
          ))
        } yield BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier(tempSymbol.identifier)))

      case Lambda(typeParams, paramDefs, returnType, body) =>
        // TODO: Implement Lambda support
        ???

      case other: Expression =>
        for {
          stringResult <- compileStringSubExpression(other)
          numericResult <- compileNumericSubExpression(other)
          boolResult <- compileBooleanSubExpression(other)

          result <- stringResult.orElse(boolResult).orElse(numericResult) match {
            case Some(value) => r(value)
            case None => left[ExpressionCompiler, CompilerError, BashExpression](UnsupportedExpression(other))
          }
        } yield result
    }
  }

  def compileExpression(expression: Expression)(factory: BashExpression => Eff[StatementCompiler, BashStatement]): Eff[StatementCompiler, BashStatement] =
    for {
      result <- compile(expression).runWriter[Eff[StatementCompiler, BashStatement]]
      (compiledExpression, compilePrereqs) = result
      prereqs <- Traverse[List].sequence(compilePrereqs)
      resultStatement <- factory(compiledExpression)
    } yield BashStatements.Sequence(prereqs ::: List(resultStatement)).normalize

  def compileExpressions(expressions: List[Expression])(factory: List[BashExpression] => Eff[StatementCompiler, BashStatement]): Eff[StatementCompiler, BashStatement] =
    for {
      pairs <- Traverse[List].sequence(expressions.map(compile(_).runWriter[Eff[StatementCompiler, BashStatement]]))
      compiledExpressions = pairs.map { case (compiledExpression, _) => compiledExpression }
      mergedPrereqs = Monoid[List[Eff[StatementCompiler, BashStatement]]].combineAll(pairs.map { case (_, statements) => statements })
      prereqs <- Traverse[List].sequence(mergedPrereqs)
      resultStatement <- factory(compiledExpressions)
    } yield BashStatements.Sequence(prereqs ::: List(resultStatement)).normalize

  def withFunctionHeader(context: Context, compiledBody: BashStatement, paramDefs: List[ParamDef], returnType: Type): BashStatement = {
    val startIndex =
      if (returnType == Types.Unit())
        1
      else
        2
    val paramCount = paramDefs.length
    val returnVariableDeclaration: BashStatement =
      if (returnType == Types.Unit()) {
        BashStatements.Nop
      } else {
        val retVar = AssignedSymbol.inScope(context.scope, SymbolName("_retvar"))
        BashStatements.Local(
          Set.empty,
          BashIdentifier(retVar.identifier),
          Some(BashExpressions.ReadVariable(
            BashVariables.Positional(1)
          ))
        )
      }
    val parameterDeclarations =
      paramDefs
        .zipWithIndex
        .map { case (paramDef, idx) => (paramDef.name, startIndex + idx) }
        .map { case (name, position) =>
          BashStatements.Local(
            Set(BashDeclareOptions.ReadOnly),
            BashIdentifier(context.symbols(name).identifier),
            Some(BashExpressions.ReadVariable(
              BashVariables.Positional(position)
            ))
          )
        }

    BashStatements.Sequence(returnVariableDeclaration :: parameterDeclarations ::: List(compiledBody))
  }

  def compileSingleStatement(singleStatement: SingleStatement): Eff[StatementCompiler, BashStatement] = {
    singleStatement match {
      case VariableDeclaration(name, props, expression) =>
        for {
          existingSymbol <- findSymbol[StatementCompiler](name)
          result <- existingSymbol match {
            case Some(_) =>
              left[StatementCompiler, CompilerError, BashStatement](SymbolAlreadyDefined(name))
            case None =>
              for {
                assignedSymbol <- createIdentifier[StatementCompiler](name, props.mutable)
                result <- compileExpression(expression) { compiledExpression =>
                  pure[StatementCompiler, BashStatement](BashStatements.Assign(BashIdentifier(assignedSymbol.identifier), compiledExpression))
                }
              } yield result
          }
        } yield result
      case FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) =>
        for {
          existingSymbol <- findSymbol[StatementCompiler](name)
          result <- existingSymbol match {
            case Some(_) =>
              left[StatementCompiler, CompilerError, BashStatement](SymbolAlreadyDefined(name))
            case None =>
              for {
                assignedSymbol <- createIdentifier[StatementCompiler](name, isMutable = false)
                // TODO: add type parameters to context
                functionContext <- createFunctionContext[StatementCompiler](name, paramDefs)
                compiledBody <- runChildContext(functionContext, typeCheckStatement(body) >> compileStatement(body))
              } yield BashStatements.Function(BashIdentifier(assignedSymbol.identifier), withFunctionHeader(functionContext, compiledBody, paramDefs, returnType).normalize)
          }
        } yield result
      case Call(function, parameters) =>
        compileExpression(function) { compiledFunction =>
          compileExpressions(parameters) { compiledParameters =>
            pure[StatementCompiler, BashStatement](BashStatements.Command(compiledFunction, compiledParameters))
          }
        }
      case Run(command, parameters) =>
        compileExpression(command) { compiledFunction =>
          compileExpressions(parameters) { compiledParameters =>
            pure[StatementCompiler, BashStatement](BashStatements.Command(compiledFunction, compiledParameters))
          }
        }
      case If(condition, trueBody, falseBody) =>
        for {
          conditionResult <- compile(condition).runWriter[Eff[StatementCompiler, BashStatement]]
          (compiledCondition, conditionPrereqs) = conditionResult
          prereqs <- Traverse[List].sequence(conditionPrereqs)
          trueContext <- cloneContext[StatementCompiler]()
          falseContext <- cloneContext[StatementCompiler]()
          compiledTrueBody <- runChildContext(trueContext, typeCheckStatement(trueBody) >> compileStatement(trueBody))
          compiledFalseBody <- runChildContext(falseContext, typeCheckStatement(falseBody) >> compileStatement(falseBody))
        } yield BashStatements.Sequence(
          prereqs ::: List(
            BashStatements.IfThenElse(compiledCondition, compiledTrueBody, compiledFalseBody)
          )).normalize
      case While(condition, body) =>
        for {
          conditionResult <- compile(condition).runWriter[Eff[StatementCompiler, BashStatement]]
          (compiledCondition, conditionPrereqs) = conditionResult
          prereqs <- Traverse[List].sequence(conditionPrereqs)
          childContext <- cloneContext[StatementCompiler]()
          compiledBody <- runChildContext(childContext, typeCheckStatement(body) >> compileStatement(body))
        } yield BashStatements.Sequence(
          prereqs ::: List(
            BashStatements.While(compiledCondition, compiledBody)
          )
        ).normalize
      case UpdateVariable(name, value) =>
        for {
          existingSymbol <- findSymbol[StatementCompiler](name)
          existingMutability <- findMutability[StatementCompiler](name)
          result <- (existingSymbol, existingMutability) match {
            case (Some(symbol), Some(true)) =>
              compileExpression(value) { compiledValue =>
                pure[StatementCompiler, BashStatement](
                  BashStatements.Assign(BashIdentifier(symbol.identifier), compiledValue)
                )
              }
            case (Some(_), _) =>
              left[StatementCompiler, CompilerError, BashStatement](UpdatingImmutableVariable(name))
            case (None, _) =>
              left[StatementCompiler, CompilerError, BashStatement](UndefinedSymbol(name))
          }
        } yield result
      case UpdateCell(name, index, value) =>
        for {
          existingSymbol <- findSymbol[StatementCompiler](name)
          result <- existingSymbol match {
            case Some(arraySymbol) =>
              compileExpression(index) { compiledIndex =>
                compileExpression(value) { compiledValue =>
                  pure[StatementCompiler, BashStatement](
                    BashStatements.ArrayUpdate(BashIdentifier(arraySymbol.identifier), compiledIndex, compiledValue)
                  )
                }
              }
            case None =>
              left[StatementCompiler, CompilerError, BashStatement](UndefinedSymbol(name))
          }
        } yield result
      case ArrayDeclaration(name, elementType) =>
        for {
          existingSymbol <- findSymbol[StatementCompiler](name)
          result <- existingSymbol match {
            case Some(_) =>
              left[StatementCompiler, CompilerError, BashStatement](SymbolAlreadyDefined(name))
            case None =>
              for {
                assignedSymbol <- createIdentifier[StatementCompiler](name, isMutable = true)
              } yield BashStatements.Declare(Set(BashDeclareOptions.Array), BashIdentifier(assignedSymbol.identifier), None) // TODO: use local (?)
          }
        } yield result
      case Return(expression) =>
        for {
          expressionType <- typeCheckExpression[StatementCompiler](expression)
          result0 <- expressionType match {
            case SimpleType(Types.Array(_)) =>
              expression match {
                case Variable(returnedVariable) =>
                  for {
                    existingSymbol <- findSymbol[StatementCompiler](returnedVariable)
                    ctx <- get[StatementCompiler, Context]
                    retVar = AssignedSymbol.inScope(ctx.scope, SymbolName("_retvar"))
                    result1 <- existingSymbol match {
                      case Some(symbol) =>
                        pure[StatementCompiler, BashStatement](
                          BashStatements.Eval(
                            BashStatements.Assign(
                              BashIdentifier(retVar.identifier),
                              BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier(symbol.identifier)), BashArrayIndices.All)))
                        )
                      case None =>
                        left[StatementCompiler, CompilerError, BashStatement](UndefinedSymbol(returnedVariable))
                    }
                  } yield result1
                case _ =>
                  left[StatementCompiler, CompilerError, BashStatement](UnsupportedReturnExpression(expression))
              }
            case _ =>
              for {
                result1 <- compileExpression(expression) { compiledExpression =>
                  for {
                    tempSymbol <- generateTempSymbol[StatementCompiler]
                    tempId = BashIdentifier(tempSymbol.identifier)
                    ctx <- get[StatementCompiler, Context]
                    retVar = AssignedSymbol.inScope(ctx.scope, SymbolName("_retvar"))
                  } yield BashStatements.Sequence(List(
                    BashStatements.Assign(tempId, compiledExpression), // TODO: use local
                    BashStatements.Eval(BashStatements.Assign(BashIdentifier(retVar.identifier), BashExpressions.ReadVariable(BashVariables.Variable(tempId))))
                  ))
                }
              } yield result1
          }
        } yield result0
    }
  }

  def compileStatement(statement: Statement): Eff[StatementCompiler, BashStatement] = {
    statement match {
      case Single(singleStatement) =>
        compileSingleStatement(singleStatement)
      case Sequence(first, rest) =>
        for {
          compiledFirst <- compileSingleStatement(first)
          compiledRest <- compileStatement(rest)
        } yield BashStatements.Sequence(compiledFirst :: compiledRest.flatten)
      case NoOp =>
        pure(BashStatements.Nop)
    }
  }
}
