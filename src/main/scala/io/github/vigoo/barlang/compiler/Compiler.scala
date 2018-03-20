package io.github.vigoo.barlang.compiler

import cats.data.Kleisli
import cats.syntax.traverse._
import cats.instances.list._
import io.github.vigoo.barlang.compiler
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import io.github.vigoo.barlang.language.Statements.{NoOp, Sequence, Single}
import io.github.vigoo.barlang.language._
import io.github.vigoo.bash.language._
import io.github.vigoo.bash.language.BashPrettyPrint._
import io.github.vigoo.bc.language.{BcExpression, BcExpressions}
import io.github.vigoo.simpp.PrettyPrint

import scala.language.higherKinds

object Compiler extends CompilerTypes with Predefined {

  def initialContext: Context =
    Context(
      scope = GlobalScope,
      symbols = Map.empty,
      symbolTypes = predefined.map { case (name, predefinedValue) => name -> SimpleType(predefinedValue.typ) },
      lastTmp = 0
    )

  def compileToString(script: Script): CompilerResult[String] = {
    compile(script).runEither.evalState(initialContext).run.map(bashStatement => print(bashStatement))
  }

  def compile(script: Script): Eff[StatementCompiler, BashStatement] = {
    val replacedBody = replacePredefs(script.body)
    for {
      _ <- typeCheckStatement(replacedBody)
      compiled <- compileStatement(replacedBody)
    } yield compiled
  }

  def typeCheck(expression: Expression, context: Context): Either[CompilerError, ExtendedType] =
    typeCheckExpression[StatementCompiler](replacePredefs(expression)).runEither.evalState(context).run

  def typeCheck(statement: SingleStatement, context: Context): Either[CompilerError, ExtendedType] =
    typeCheckSingleStatement(replacePredefs(statement)).runEither.evalState(context).run

  def replacePredefs(statement: SingleStatement): SingleStatement = statement match {
    case VariableDeclaration(name, value) =>
      VariableDeclaration(name, replacePredefs(value))
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
      case Apply(function, parameters) => Apply(function, parameters.map(replacePredefs))
      case UnaryOp(operator, x) => UnaryOp(operator, replacePredefs(x))
      case BinaryOp(operator, x, y) => BinaryOp(operator, replacePredefs(x), replacePredefs(y))
      case Lambda(typeParams, paramDefs, returnType, body) => Lambda(typeParams, paramDefs, returnType, replacePredefs(body))
      case _ => expression
    }
  }

  def createFunctionContext(name: SymbolName, paramDefs: List[ParamDef]): Eff[StatementCompiler, Context] = {
    for {
      context <- get[StatementCompiler, Context]
      newScope = context.scope.functionScope(name)
      extendedSymbolTypes = paramDefs.foldLeft(context.symbolTypes) { (types, paramDef) =>
        types + (paramDef.name -> SimpleType(paramDef.typ))
      }
      extendedSymbols = paramDefs.foldLeft(context.symbols) { (symbols, paramDef) =>
        symbols + (paramDef.name -> AssignedSymbol.inScope(context.scope, paramDef.name))
      }
    } yield context.copy(
      scope = newScope,
      symbols = extendedSymbols,
      symbolTypes = extendedSymbolTypes
    )
  }

  def unifyTypeVariables[R : _compilerState : _compilerResult](typeParameters: List[TypeParam], actualTypes: List[ExtendedType], typeDefinitions: List[Type]): Eff[R, Map[SymbolName, Type]] = {
    val typeVariableNames = typeParameters.map(_.name).toSet
    val pairs = actualTypes.zip(typeDefinitions)

    val mappings = pairs.map {
      case (SimpleType(actualType), (Types.TypeVariable(name))) if typeVariableNames.contains(name) =>
        Some(name -> actualType)
      case _ =>
        None
    }

    // TODO: error on ambiguous type mappings
    pure[R, Map[SymbolName, Type]](mappings.flatten.toMap)
  }

  def appliedType(mapping: Map[SymbolName, Type])(typ: Type): Type = {
    typ match {
      case Types.TypeVariable(name) if mapping.contains(name) =>
        mapping(name)
      case Types.Function(typeParams, paramTypes, returnType) =>
        Types.Function(typeParams, paramTypes.map(appliedType(mapping)), appliedType(mapping)(returnType))
      case _ =>
        typ
    }
  }

  def appliedExtendedType(mapping: Map[SymbolName, Type])(typ: Type): ExtendedType = {
    SimpleType(appliedType(mapping)(typ))
  }

  def typeCheckBinaryBooleanExpression[R : _compilerState : _compilerResult](a: Expression, b: Expression): Eff[R, ExtendedType] = {
    for {
      typA <- typeCheckExpression[R](a)
      typB <- typeCheckExpression[R](b)
      result <- (typA, typB) match {
        case (SimpleType(Types.Bool()), SimpleType(Types.Bool())) =>
          pure[R, ExtendedType](SimpleType(Types.Bool()))
        case _ =>
          left[R, CompilerError, ExtendedType](InvalidBooleanExpression(None))

      }
    } yield result
  }

  def typeCheckBinaryNumericExpression[R : _compilerState : _compilerResult](a: Expression, b: Expression, resultType: Option[Type]): Eff[R, ExtendedType] = {
    for {
      typA <- typeCheckExpression[R](a)
      typB <- typeCheckExpression[R](b)
      result <- (typA, typB) match {
        case (SimpleType(Types.Int()), SimpleType(Types.Int())) =>
          pure[R, ExtendedType](SimpleType(resultType.getOrElse(Types.Int())))
        case (SimpleType(Types.Double()), SimpleType(Types.Double())) =>
          pure[R, ExtendedType](SimpleType(resultType.getOrElse(Types.Double())))
        case (SimpleType(Types.Double()), SimpleType(Types.Int())) =>
          pure[R, ExtendedType](SimpleType(resultType.getOrElse(Types.Double())))
        case (SimpleType(Types.Int()), SimpleType(Types.Double())) =>
          pure[R, ExtendedType](SimpleType(resultType.getOrElse(Types.Double())))
        case _ =>
          left[R, CompilerError, ExtendedType](InvalidTypesInNumericExpression(List(typA, typB)))

      }
    } yield result
  }

  def typeCheckEqualityExpression[R : _compilerState : _compilerResult](a: Expression, b: Expression): Eff[R, ExtendedType] = {
    for {
      typA <- typeCheckExpression[R](a)
      typB <- typeCheckExpression[R](b)
      result <- if (typA =:= typB) {
        pure[R, ExtendedType](SimpleType(Types.Bool()))
      } else {
        left[R, CompilerError, ExtendedType](EqualityUsedOnNonEqualTypes(typA, typB))
      }
    } yield result
  }

  def typeCheckExpression[R : _compilerState : _compilerResult](expression: Expression): Eff[R, ExtendedType] = {
    expression match {
      case StringLiteral(value) =>
        pure(SimpleType(Types.String()))
      case BoolLiteral(value) =>
        pure(SimpleType(Types.Bool()))
      case IntLiteral(value) =>
        pure(SimpleType(Types.Int()))
      case DoubleLiteral(value) =>
        pure(SimpleType(Types.Double()))
      case Variable(name) =>
        for {
          existingType <- findType[R](name)
          result <- existingType match {
            case Some(t) =>
              pure[R, ExtendedType](t)
            case None =>
              left[R, CompilerError, ExtendedType](CannotInferType(name))
          }
        } yield result
      case ArrayAccess(name, index) =>
        for {
          existingType <- findType[R](name)
          indexType <- typeCheckExpression(index)
          result <- existingType match {
            case Some(SimpleType(Types.Array(elementType))) =>
              indexType match {
                case SimpleType(Types.Int()) =>
                  pure[R, ExtendedType](SimpleType(elementType))
                case _ =>
                  left[R, CompilerError, ExtendedType](InvalidArrayIndexType(indexType))
              }
            case Some(t) =>
              left[R, CompilerError, ExtendedType](NonArrayTypeIndexed(t))
            case none =>
              left[R, CompilerError, ExtendedType](UndefinedSymbol(name))
          }
        } yield result

      case SystemVariable(name) =>
        pure(SimpleType(Types.String()))
      case Predefined(name) =>
        predefined.get(name) match {
          case Some(predefinedValue) =>
            pure[R, ExtendedType](SimpleType(predefinedValue.typ))
          case None =>
            left[R, CompilerError, ExtendedType](CannotInferType(name))
        }
      case Apply(functionReference, parameters) =>
        for {
          parameterTypes <- parameters.traverse[Eff[R, ?], ExtendedType](expression => typeCheckExpression[R](expression))
          functionReferenceType <- typeCheckExpression[R](functionReference)
          result <- functionReferenceType match {
            case SimpleType(Types.Function(typeParams, expectedParameterTypes, returnType)) =>
              for {
                typeVarMapping <- unifyTypeVariables[R](typeParams, parameterTypes, expectedParameterTypes)
                result <- if (parameterTypes =:= expectedParameterTypes.map(appliedExtendedType(typeVarMapping))) {
                  pure[R, ExtendedType](appliedExtendedType(typeVarMapping)(returnType))
                } else {
                  left[R, CompilerError, ExtendedType](InvalidParameterTypes(expectedParameterTypes, parameterTypes))
                }
              } yield result
            case FunctionReference(typeParams, expectedParameterTypes, returnType) =>
              for {
                typeVarMapping <- unifyTypeVariables(typeParams, parameterTypes, expectedParameterTypes)
                result <- if (parameterTypes =:= expectedParameterTypes.map(appliedExtendedType(typeVarMapping))) {
                  pure[R, ExtendedType](appliedExtendedType(typeVarMapping)(returnType))
                } else {
                  left[R, CompilerError, ExtendedType](InvalidParameterTypes(expectedParameterTypes, parameterTypes))
                }
              } yield result
            case _ =>
              left[R, CompilerError, ExtendedType](SymbolNotBoundToFunction(expression))
          }
        } yield result
      case UnaryOp(UnaryOperators.Not, x) =>
        for {
          typ <- typeCheckExpression[R](x)
          result <- typ match {
            case SimpleType(Types.Bool()) =>
              pure[R, ExtendedType](SimpleType(Types.Bool()))
            case _ =>
              left[R, CompilerError, ExtendedType](InvalidBooleanExpression(Some(x)))
          }
        } yield result

      case BinaryOp(BinaryOperators.And, a, b) =>
        typeCheckBinaryBooleanExpression[R](a, b)
      case BinaryOp(BinaryOperators.Or, a, b) =>
        typeCheckBinaryBooleanExpression[R](a, b)

      case BinaryOp(BinaryOperators.Add, a, b) =>
        for {
          typA <- typeCheckExpression[R](a)
          typB <- typeCheckExpression[R](b)
          result <- (typA, typB) match {
            case (SimpleType(Types.String()), SimpleType(Types.String())) =>
              pure[R, ExtendedType](SimpleType(Types.String()))
            case _ =>
              typeCheckBinaryNumericExpression[R](a, b, None)
          }
        } yield result

      case BinaryOp(BinaryOperators.Sub, a, b) =>
        typeCheckBinaryNumericExpression[R](a, b, None)
      case BinaryOp(BinaryOperators.Mul, a, b) =>
        typeCheckBinaryNumericExpression[R](a, b, None)
      case BinaryOp(BinaryOperators.Div, a, b) =>
        typeCheckBinaryNumericExpression[R](a, b, None)

      case BinaryOp(BinaryOperators.Mod, a, b) =>
        for {
          typ <- typeCheckBinaryNumericExpression[R](a, b, None)
          result <- typ match {
            case SimpleType(Types.Int()) =>
              pure[R, ExtendedType](SimpleType(Types.Int()))
            case _ =>
              left[R, CompilerError, ExtendedType](InvalidTypesInNumericExpression(List(typ)))
          }
        } yield result

      case BinaryOp(BinaryOperators.Less, a, b) =>
        typeCheckBinaryNumericExpression[R](a, b, Some(Types.Bool()))
      case BinaryOp(BinaryOperators.LessEq, a, b) =>
        typeCheckBinaryNumericExpression[R](a, b, Some(Types.Bool()))
      case BinaryOp(BinaryOperators.Greater, a, b) =>
        typeCheckBinaryNumericExpression[R](a, b, Some(Types.Bool()))
      case BinaryOp(BinaryOperators.GreaterEq, a, b) =>
        typeCheckBinaryNumericExpression[R](a, b, Some(Types.Bool()))

      case BinaryOp(BinaryOperators.Eq, a, b) =>
        typeCheckEqualityExpression[R](a, b)
      case BinaryOp(BinaryOperators.Neq, a, b) =>
        typeCheckEqualityExpression[R](a, b)

      case Lambda(typeParams, paramDefs, returnType, body) =>
        // TODO: type check body
        pure(SimpleType(Types.Function(typeParams, paramDefs.map(_.typ), returnType)))
    }
  }

  def typeCheckSingleStatement(statement: SingleStatement): Eff[StatementCompiler, ExtendedType] = {
    statement match {
      case VariableDeclaration(name, value) =>
        for {
          valueType <- typeCheckExpression[StatementCompiler](value)
          _ <- storeType[StatementCompiler](name, valueType)
        } yield SimpleType(Types.Unit())
      case FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) =>
        val paramTypes = paramDefs.map(_.typ)
        (for {
          _ <- storeType[StatementCompiler](name, SimpleType(Types.Function(typeParams, paramTypes, returnType)))
          functionContext <- createFunctionContext(name, paramDefs)
          bodyType <- runChildContext(functionContext, typeCheckStatement(body))
        } yield bodyType).flatMap { bodyType =>

          if (bodyType =:= SimpleType(returnType)) {
            pure[StatementCompiler, ExtendedType](SimpleType(Types.Unit()))
          } else {
            left[StatementCompiler, CompilerError, ExtendedType](InvalidReturnType(name, returnType, bodyType))
          }
        }
      case Call(function, parameters) =>
        typeCheckExpression(Expressions.Apply(function, parameters))
      case Run(command, parameters) =>
        for {
          commandType <- typeCheckExpression[StatementCompiler](command)
          paramTypes <- parameters.traverse[Eff[StatementCompiler, ?], ExtendedType](expr => typeCheckExpression[StatementCompiler](expr))
          result <- if (commandType =:= SimpleType(Types.String()) && paramTypes.forall(t => t =:= SimpleType(Types.String()))) {
            pure[StatementCompiler, ExtendedType](SimpleType(Types.Unit()))
          } else {
            left[StatementCompiler, CompilerError, ExtendedType](InvalidParametersForRunStatement(commandType, paramTypes))
          }
        } yield result
      case If(condition, trueBody, falseBody) =>
        typeCheckExpression[StatementCompiler](condition).flatMap { conditionType =>
          if (conditionType =:= SimpleType(Types.Bool())) {
            for {
              childContext <- cloneContext[StatementCompiler]()
              _ <- runChildContext(childContext, typeCheckStatement(trueBody))
              _ <- runChildContext(childContext, typeCheckStatement(falseBody))
            } yield SimpleType(Types.Unit())
          } else {
            left[StatementCompiler, CompilerError, ExtendedType](InvalidConditionTypeForIf(conditionType))
          }
        }
      case While(condition, body) =>
        typeCheckExpression[StatementCompiler](condition).flatMap { conditionType =>
          if (conditionType =:= SimpleType(Types.Bool())) {
            for {
              childContext <- cloneContext[StatementCompiler]()
              _ <- runChildContext(childContext, typeCheckStatement(body))
            } yield SimpleType(Types.Unit())
          } else {
            left[StatementCompiler, CompilerError, ExtendedType](InvalidConditionTypeForWhile(conditionType))
          }
        }
      case UpdateVariable(name, value) =>
        for {
          valueType <- typeCheckExpression[StatementCompiler](value)
          existingType <- findType[StatementCompiler](name)
          result <- existingType match {
            case Some(t) if t =:= valueType =>
              pure[StatementCompiler, ExtendedType](SimpleType(Types.Unit()))
            case Some(t) =>
              left[StatementCompiler, CompilerError, ExtendedType](VariableUpdateTypeMismatch(valueType, t))
            case None =>
              left[StatementCompiler, CompilerError, ExtendedType](UndefinedSymbol(name))
          }
        } yield result
      case UpdateCell(name, index, value) =>
        for {
          indexType <- typeCheckExpression[StatementCompiler](index)
          valueType <- typeCheckExpression[StatementCompiler](value)
          existingType <- findType[StatementCompiler](name)
          result <- existingType match {
            case Some(SimpleType(Types.Array(elementType))) if valueType =:= SimpleType(elementType) =>
              indexType match {
                case SimpleType(Types.Int()) =>
                  pure[StatementCompiler, ExtendedType](SimpleType(Types.Unit()))
                case _ =>
                  left[StatementCompiler, CompilerError, ExtendedType](InvalidArrayIndexType(indexType))
              }
            case Some(t) =>
              left[StatementCompiler, CompilerError, ExtendedType](VariableUpdateTypeMismatch(valueType, t))
            case none =>
              left[StatementCompiler, CompilerError, ExtendedType](UndefinedSymbol(name))
          }
        } yield result
      case ArrayDeclaration(name, elementType) =>
        for {
          _ <- storeType[StatementCompiler](name, SimpleType(Types.Array(elementType)))
        } yield SimpleType(Types.Unit())
      case Return(value) =>
        typeCheckExpression[StatementCompiler](value)
    }
  }

  def typeCheckStatement(statement: Statement): Eff[StatementCompiler, ExtendedType] = {
    statement match {
      case Single(singleStatement) =>
        typeCheckSingleStatement(singleStatement)
      case Sequence(first, second) =>
        for {
          _ <- typeCheckSingleStatement(first)
          t <- typeCheckStatement(second)
        } yield t
      case NoOp =>
        pure(SimpleType(Types.Unit()))
    }
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
        BashStatements.Assign(
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
          findResult <- findSymbol[ExpressionCompiler](name)
          result <- findResult match {
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
        BashStatements.Assign(
          BashIdentifier(tempSymbol.identifier),
          compiledExpr
        )))
    } yield tempSymbol
  }

  def integerExpressionToTempVar(expression: Expression): Eff[ExpressionCompiler, AssignedSymbol] = ???

  def readTempVar[R : _compilerState : _compilerResult](symbol: AssignedSymbol): Eff[R, BashExpression] = ???

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
                    Set(BashDeclareOptions.Array, BashDeclareOptions.ReadOnly),
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

      case SystemVariable(name) => ???
      case Predefined(name) => ???
      case Apply(function, parameters) => ???
      case UnaryOp(operator, x) => ???
      case BinaryOp(operator, x, y) => ???
      case Lambda(typeParams, paramDefs, returnType, body) => ???

      case other: Expression =>
        for {
          numericResult <- compileNumericSubExpression(other)
          result <- numericResult match {
            case Some(value) => r(value)
            case None => left[ExpressionCompiler, CompilerError, BashExpression](UnsupportedExpression(other))
          }
        } yield result
    }
  }

  def compileExpression(expression: Expression)(factory: BashExpression => BashStatement): Eff[StatementCompiler, BashStatement] =
    for {
      result <- compile(expression).runWriter[Eff[StatementCompiler, BashStatement]]
      (compiledExpression, compilePrereqs) = result
      prereqs <- compilePrereqs.sequenceA
      resultStatement = factory(compiledExpression)
    } yield BashStatements.Sequence(prereqs ::: List(resultStatement)).normalize

  def compileSingleStatement(singleStatement: SingleStatement): Eff[StatementCompiler, BashStatement] = {
    singleStatement match {
      case VariableDeclaration(name, expression) =>
        for {
          existingSymbol <- findSymbol[StatementCompiler](name)
          result <- existingSymbol match {
            case Some(_) =>
              left[StatementCompiler, CompilerError, BashStatement](SymbolAlreadyDefined(name))
            case None =>
              for {
                assignedSymbol <- createIdentifier[StatementCompiler](name)
                result <- compileExpression(expression) { compiledExpression =>
                  BashStatements.Assign(BashIdentifier(assignedSymbol.identifier), compiledExpression)
                }
              } yield result
          }
        } yield result
      case FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) => ???
      case Call(function, parameters) => ???
      case Run(command, parameters) => ???
      case If(condition, trueBody, falseBody) => ???
      case While(condition, body) => ???
      case UpdateVariable(name, value) => ???
      case UpdateCell(name, index, value) => ???
      case ArrayDeclaration(name, elementType) => ???
      case Return(value) => ???
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

object CompilerPlayground extends App {

  val src =
    """val x = 5
      |
    """.stripMargin
  val tokens = Parser.BarlangLexer.parse(Parser.BarlangLexer.tokens, src)
  val result = Parser.BarlangParser(tokens.get)
  result match {
    case Parser.BarlangParser.Success(ast, next) =>
      val result = Compiler.compileToString(ast)
      println(result)
    case Parser.BarlangParser.NoSuccess(msg, next) =>
      println(s"${next.pos.line}:${next.pos.column} $msg")
      println(src.lines.toVector(next.pos.line - 1))
  }
}