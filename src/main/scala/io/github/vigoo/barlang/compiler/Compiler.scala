package io.github.vigoo.barlang.compiler

import cats.instances.list._
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import io.github.vigoo.barlang.language.Statements.{NoOp, Sequence, Single}
import io.github.vigoo.barlang.language._
import io.github.vigoo.bash.language.{BashStatement, BashStatements}
import io.github.vigoo.bash.language.BashPrettyPrint._
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
    typeCheckExpression(replacePredefs(expression)).runEither.evalState(context).run

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

  def unifyTypeVariables(typeParameters: List[TypeParam], actualTypes: List[ExtendedType], typeDefinitions: List[Type]): Eff[StatementCompiler, Map[SymbolName, Type]] = {
    val typeVariableNames = typeParameters.map(_.name).toSet
    val pairs = actualTypes.zip(typeDefinitions)

    val mappings = pairs.map {
      case (SimpleType(actualType), (Types.TypeVariable(name))) if typeVariableNames.contains(name) =>
        Some(name -> actualType)
      case _ =>
        None
    }

    // TODO: error on ambiguous type mappings
    pure[StatementCompiler, Map[SymbolName, Type]](mappings.flatten.toMap)
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

  def typeCheckBinaryBooleanExpression(a: Expression, b: Expression): Eff[StatementCompiler, ExtendedType] = {
    for {
      typA <- typeCheckExpression(a)
      typB <- typeCheckExpression(b)
      result <- (typA, typB) match {
        case (SimpleType(Types.Bool()), SimpleType(Types.Bool())) =>
          pure[StatementCompiler, ExtendedType](SimpleType(Types.Bool()))
        case _ =>
          left[StatementCompiler, CompilerError, ExtendedType](InvalidBooleanExpression(None))

      }
    } yield result
  }

  def typeCheckBinaryNumericExpression(a: Expression, b: Expression, resultType: Option[Type]): Eff[StatementCompiler, ExtendedType] = {
    for {
      typA <- typeCheckExpression(a)
      typB <- typeCheckExpression(b)
      result <- (typA, typB) match {
        case (SimpleType(Types.Int()), SimpleType(Types.Int())) =>
          pure[StatementCompiler, ExtendedType](SimpleType(resultType.getOrElse(Types.Int())))
        case (SimpleType(Types.Double()), SimpleType(Types.Double())) =>
          pure[StatementCompiler, ExtendedType](SimpleType(resultType.getOrElse(Types.Double())))
        case (SimpleType(Types.Double()), SimpleType(Types.Int())) =>
          pure[StatementCompiler, ExtendedType](SimpleType(resultType.getOrElse(Types.Double())))
        case (SimpleType(Types.Int()), SimpleType(Types.Double())) =>
          pure[StatementCompiler, ExtendedType](SimpleType(resultType.getOrElse(Types.Double())))
        case _ =>
          left[StatementCompiler, CompilerError, ExtendedType](InvalidTypesInNumericExpression(List(typA, typB)))

      }
    } yield result
  }

  def typeCheckEqualityExpression(a: Expression, b: Expression): Eff[StatementCompiler, ExtendedType] = {
    for {
      typA <- typeCheckExpression(a)
      typB <- typeCheckExpression(b)
      result <- if (typA =:= typB) {
        pure[StatementCompiler, ExtendedType](SimpleType(Types.Bool()))
      } else {
        left[StatementCompiler, CompilerError, ExtendedType](EqualityUsedOnNonEqualTypes(typA, typB))
      }
    } yield result
  }

  def typeCheckExpression(expression: Expression): Eff[StatementCompiler, ExtendedType] = {
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
          existingType <- findType[StatementCompiler](name)
          result <- existingType match {
            case Some(t) =>
              pure[StatementCompiler, ExtendedType](t)
            case None =>
              left[StatementCompiler, CompilerError, ExtendedType](CannotInferType(name))
          }
        } yield result
      case ArrayAccess(name, index) =>
        for {
          existingType <- findType[StatementCompiler](name)
          indexType <- typeCheckExpression(index)
          result <- existingType match {
            case Some(SimpleType(Types.Array(elementType))) =>
              indexType match {
                case SimpleType(Types.Int()) =>
                  pure[StatementCompiler, ExtendedType](SimpleType(elementType))
                case _ =>
                  left[StatementCompiler, CompilerError, ExtendedType](InvalidArrayIndexType(indexType))
              }
            case Some(t) =>
              left[StatementCompiler, CompilerError, ExtendedType](NonArrayTypeIndexed(t))
            case none =>
              left[StatementCompiler, CompilerError, ExtendedType](UndefinedSymbol(name))
          }
        } yield result

      case SystemVariable(name) =>
        pure(SimpleType(Types.String()))
      case Predefined(name) =>
        predefined.get(name) match {
          case Some(predefinedValue) =>
            pure[StatementCompiler, ExtendedType](SimpleType(predefinedValue.typ))
          case None =>
            left[StatementCompiler, CompilerError, ExtendedType](CannotInferType(name))
        }
      case Apply(functionReference, parameters) =>
        for {
          parameterTypes <- parameters.traverseA(expression => typeCheckExpression(expression))
          functionReferenceType <- typeCheckExpression(functionReference)
          result <- functionReferenceType match {
            case SimpleType(Types.Function(typeParams, expectedParameterTypes, returnType)) =>
              for {
                typeVarMapping <- unifyTypeVariables(typeParams, parameterTypes, expectedParameterTypes)
                result <- if (parameterTypes =:= expectedParameterTypes.map(appliedExtendedType(typeVarMapping))) {
                  pure[StatementCompiler, ExtendedType](appliedExtendedType(typeVarMapping)(returnType))
                } else {
                  left[StatementCompiler, CompilerError, ExtendedType](InvalidParameterTypes(expectedParameterTypes, parameterTypes))
                }
              } yield result
            case FunctionReference(typeParams, expectedParameterTypes, returnType) =>
              for {
                typeVarMapping <- unifyTypeVariables(typeParams, parameterTypes, expectedParameterTypes)
                result <- if (parameterTypes =:= expectedParameterTypes.map(appliedExtendedType(typeVarMapping))) {
                  pure[StatementCompiler, ExtendedType](appliedExtendedType(typeVarMapping)(returnType))
                } else {
                  left[StatementCompiler, CompilerError, ExtendedType](InvalidParameterTypes(expectedParameterTypes, parameterTypes))
                }
              } yield result
            case _ =>
              left[StatementCompiler, CompilerError, ExtendedType](SymbolNotBoundToFunction(expression))
          }
        } yield result
      case UnaryOp(UnaryOperators.Not, x) =>
        for {
          typ <- typeCheckExpression(x)
          result <- typ match {
            case SimpleType(Types.Bool()) =>
              pure[StatementCompiler, ExtendedType](SimpleType(Types.Bool()))
            case _ =>
              left[StatementCompiler, CompilerError, ExtendedType](InvalidBooleanExpression(Some(x)))
          }
        } yield result

      case BinaryOp(BinaryOperators.And, a, b) =>
        typeCheckBinaryBooleanExpression(a, b)
      case BinaryOp(BinaryOperators.Or, a, b) =>
        typeCheckBinaryBooleanExpression(a, b)

      case BinaryOp(BinaryOperators.Add, a, b) =>
        for {
          typA <- typeCheckExpression(a)
          typB <- typeCheckExpression(b)
          result <- (typA, typB) match {
            case (SimpleType(Types.String()), SimpleType(Types.String())) =>
              pure[StatementCompiler, ExtendedType](SimpleType(Types.String()))
            case _ =>
              typeCheckBinaryNumericExpression(a, b, None)
          }
        } yield result

      case BinaryOp(BinaryOperators.Sub, a, b) =>
        typeCheckBinaryNumericExpression(a, b, None)
      case BinaryOp(BinaryOperators.Mul, a, b) =>
        typeCheckBinaryNumericExpression(a, b, None)
      case BinaryOp(BinaryOperators.Div, a, b) =>
        typeCheckBinaryNumericExpression(a, b, None)

      case BinaryOp(BinaryOperators.Mod, a, b) =>
        for {
          typ <- typeCheckBinaryNumericExpression(a, b, None)
          result <- typ match {
            case SimpleType(Types.Int()) =>
              pure[StatementCompiler, ExtendedType](SimpleType(Types.Int()))
            case _ =>
              left[StatementCompiler, CompilerError, ExtendedType](InvalidTypesInNumericExpression(List(typ)))
          }
        } yield result

      case BinaryOp(BinaryOperators.Less, a, b) =>
        typeCheckBinaryNumericExpression(a, b, Some(Types.Bool()))
      case BinaryOp(BinaryOperators.LessEq, a, b) =>
        typeCheckBinaryNumericExpression(a, b, Some(Types.Bool()))
      case BinaryOp(BinaryOperators.Greater, a, b) =>
        typeCheckBinaryNumericExpression(a, b, Some(Types.Bool()))
      case BinaryOp(BinaryOperators.GreaterEq, a, b) =>
        typeCheckBinaryNumericExpression(a, b, Some(Types.Bool()))

      case BinaryOp(BinaryOperators.Eq, a, b) =>
        typeCheckEqualityExpression(a, b)
      case BinaryOp(BinaryOperators.Neq, a, b) =>
        typeCheckEqualityExpression(a, b)

      case Lambda(typeParams, paramDefs, returnType, body) =>
        // TODO: type check body
        pure(SimpleType(Types.Function(typeParams, paramDefs.map(_.typ), returnType)))
    }
  }

  def typeCheckSingleStatement(statement: SingleStatement): Eff[StatementCompiler, ExtendedType] = {
    statement match {
      case VariableDeclaration(name, value) =>
        for {
          valueType <- typeCheckExpression(value)
          _ <- storeType(name, valueType)
        } yield SimpleType(Types.Unit())
      case FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) =>
        val paramTypes = paramDefs.map(_.typ)
        (for {
          _ <- storeType(name, SimpleType(Types.Function(typeParams, paramTypes, returnType)))
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
          commandType <- typeCheckExpression(command)
          paramTypes <- parameters.traverseA(expr => typeCheckExpression(expr))
          result <- if (commandType =:= SimpleType(Types.String()) && paramTypes.forall(t => t =:= SimpleType(Types.String()))) {
            pure[StatementCompiler, ExtendedType](SimpleType(Types.Unit()))
          } else {
            left[StatementCompiler, CompilerError, ExtendedType](InvalidParametersForRunStatement(commandType, paramTypes))
          }
        } yield result
      case If(condition, trueBody, falseBody) =>
        typeCheckExpression(condition).flatMap { conditionType =>
          if (conditionType =:= SimpleType(Types.Bool())) {
            for {
              childContext <- cloneContext()
              _ <- runChildContext(childContext, typeCheckStatement(trueBody))
              _ <- runChildContext(childContext, typeCheckStatement(falseBody))
            } yield SimpleType(Types.Unit())
          } else {
            left[StatementCompiler, CompilerError, ExtendedType](InvalidConditionTypeForIf(conditionType))
          }
        }
      case While(condition, body) =>
        typeCheckExpression(condition).flatMap { conditionType =>
          if (conditionType =:= SimpleType(Types.Bool())) {
            for {
              childContext <- cloneContext()
              _ <- runChildContext(childContext, typeCheckStatement(body))
            } yield SimpleType(Types.Unit())
          } else {
            left[StatementCompiler, CompilerError, ExtendedType](InvalidConditionTypeForWhile(conditionType))
          }
        }
      case UpdateVariable(name, value) =>
        for {
          valueType <- typeCheckExpression(value)
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
          indexType <- typeCheckExpression(index)
          valueType <- typeCheckExpression(value)
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
          _ <- storeType(name, SimpleType(Types.Array(elementType)))
        } yield SimpleType(Types.Unit())
      case Return(value) =>
        typeCheckExpression(value)
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

  def compileStatement(statement: Statement): Eff[StatementCompiler, BashStatement] = {
    pure(BashStatements.Nop)
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