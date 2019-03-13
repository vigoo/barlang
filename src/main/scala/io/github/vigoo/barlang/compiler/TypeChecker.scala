package io.github.vigoo.barlang.compiler

import cats.instances.list._
import cats.syntax.traverse._
import io.github.vigoo.barlang.language.Expressions._
import io.github.vigoo.barlang.language.SingleStatements._
import io.github.vigoo.barlang.language.Statements.{NoOp, Sequence, Single}
import io.github.vigoo.barlang.language._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

trait TypeChecker {
  this: CompilerTypes with Predefined =>

  abstract class EffectOps[R] {
    protected implicit def compilerResult: _compilerResult[R]

    def succeedWith(extendedType: ExtendedType): Eff[R, ExtendedType] =
      pure[R, ExtendedType](extendedType)
    def succeedWith(typ: Type): Eff[R, ExtendedType] =
      succeedWith(SimpleType(typ))
    def failWith(compilerError: CompilerError): Eff[R, ExtendedType] =
      left[R, CompilerError, ExtendedType](compilerError)
  }

  object EffectOps {
    def apply[R](implicit cr: _compilerResult[R]): EffectOps[R] = new EffectOps[R] {
      override protected val compilerResult: _compilerResult[R] = cr
    }
  }

  def typeCheck(expression: Expression, context: Context): Either[CompilerError, ExtendedType] =
    typeCheckExpression[StatementCompiler](replacePredefs(expression)).runEither.evalState(context).run

  def typeCheck(statement: SingleStatement, context: Context): Either[CompilerError, ExtendedType] =
    typeCheckSingleStatement(replacePredefs(statement)).runEither.evalState(context).run

  def typeCheckBinaryBooleanExpression[R: _compilerState : _compilerResult](a: Expression, b: Expression): Eff[R, ExtendedType] = {
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

  def typeCheckBinaryNumericExpression[R: _compilerState : _compilerResult](a: Expression, b: Expression, resultType: Option[Type]): Eff[R, ExtendedType] = {
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

  def typeCheckEqualityExpression[R: _compilerState : _compilerResult](a: Expression, b: Expression): Eff[R, ExtendedType] = {
    val effectOps = EffectOps[R]
    import effectOps._

    for {
      typA <- typeCheckExpression[R](a)
      typB <- typeCheckExpression[R](b)
      result <- if (typA =:= typB) {
        succeedWith(Types.Bool())
      } else {
        failWith(EqualityUsedOnNonEqualTypes(typA, typB))
      }
    } yield result
  }

  def typeCheckExpression[R: _compilerState : _compilerResult](expression: Expression): Eff[R, ExtendedType] = {
    val effectOps = EffectOps[R]
    import effectOps._

    expression match {
      case StringLiteral(_) =>
        succeedWith(Types.String())
      case BoolLiteral(_) =>
        succeedWith(Types.Bool())
      case IntLiteral(_) =>
        succeedWith(Types.Int())
      case DoubleLiteral(_) =>
        succeedWith(Types.Double())
      case Variable(name) =>
        for {
          existingType <- findType[R](name)
          result <- existingType match {
            case Some(t) =>
              succeedWith(t)
            case None =>
              failWith(CannotInferType(name))
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
                  succeedWith(elementType)
                case _ =>
                  failWith(InvalidArrayIndexType(indexType))
              }
            case Some(t) =>
              failWith(NonArrayTypeIndexed(t))
            case None =>
              failWith(UndefinedSymbol(name))
          }
        } yield result

      case SystemVariable(_) =>
        succeedWith(Types.String())
      case Predefined(name) =>
        predefined.get(name) match {
          case Some(predefinedValue) =>
            succeedWith(predefinedValue.typ)
          case None =>
            failWith(CannotInferType(name))
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
                  succeedWith(appliedExtendedType(typeVarMapping)(returnType))
                } else {
                  failWith(InvalidParameterTypes(expectedParameterTypes, parameterTypes))
                }
              } yield result
            case FunctionReference(typeParams, expectedParameterTypes, returnType) =>
              for {
                typeVarMapping <- unifyTypeVariables(typeParams, parameterTypes, expectedParameterTypes)
                result <- if (parameterTypes =:= expectedParameterTypes.map(appliedExtendedType(typeVarMapping))) {
                  succeedWith(appliedExtendedType(typeVarMapping)(returnType))
                } else {
                  failWith(InvalidParameterTypes(expectedParameterTypes, parameterTypes))
                }
              } yield result
            case _ =>
              failWith(SymbolNotBoundToFunction(expression))
          }
        } yield result
      case UnaryOp(UnaryOperators.Not, x) =>
        for {
          typ <- typeCheckExpression[R](x)
          result <- typ match {
            case SimpleType(Types.Bool()) =>
              succeedWith(Types.Bool())
            case _ =>
              failWith(InvalidBooleanExpression(Some(x)))
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
              succeedWith(Types.String())
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
              succeedWith(Types.Int())
            case _ =>
              failWith(InvalidTypesInNumericExpression(List(typ)))
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
        succeedWith(Types.Function(typeParams, paramDefs.map(_.typ), returnType))
    }
  }

  def typeCheckSingleStatement(statement: SingleStatement): Eff[StatementCompiler, ExtendedType] = {
    val effectOps = EffectOps[StatementCompiler]
    import effectOps._

    statement match {
      case VariableDeclaration(name, properties, value) =>
        for {
          valueType <- typeCheckExpression[StatementCompiler](value)
          _ <- storeType[StatementCompiler](name, valueType)
        } yield SimpleType(Types.Unit())
      case FunctionDefinition(name, properties, typeParams, paramDefs, returnType, body) =>
        val paramTypes = paramDefs.map(_.typ)
        (for {
          _ <- storeType[StatementCompiler](name, SimpleType(Types.Function(typeParams, paramTypes, returnType)))
          functionContext <- createFunctionContext[StatementCompiler](name, paramDefs)
          bodyType <- runChildContext(functionContext, typeCheckStatement(body))
        } yield bodyType).flatMap { bodyType =>

          if (bodyType =:= SimpleType(returnType)) {
            succeedWith(Types.Unit())
          } else {
            failWith(InvalidReturnType(name, returnType, bodyType))
          }
        }
      case Call(function, parameters) =>
        typeCheckExpression(Expressions.Apply(function, parameters))
      case Run(command, parameters) =>
        for {
          commandType <- typeCheckExpression[StatementCompiler](command)
          paramTypes <- parameters.traverse[Eff[StatementCompiler, ?], ExtendedType](expr => typeCheckExpression[StatementCompiler](expr))
          result <- if (commandType =:= SimpleType(Types.String()) && paramTypes.forall(t => t =:= SimpleType(Types.String()))) {
            succeedWith(Types.Unit())
          } else {
            failWith(InvalidParametersForRunStatement(commandType, paramTypes))
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
            failWith(InvalidConditionTypeForIf(conditionType))
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
            failWith(InvalidConditionTypeForWhile(conditionType))
          }
        }
      case UpdateVariable(name, value) =>
        for {
          valueType <- typeCheckExpression[StatementCompiler](value)
          existingType <- findType[StatementCompiler](name)
          result <- existingType match {
            case Some(t) if t =:= valueType =>
              succeedWith(Types.Unit())
            case Some(t) =>
              failWith(VariableUpdateTypeMismatch(valueType, t))
            case None =>
              failWith(UndefinedSymbol(name))
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
                  succeedWith(Types.Unit())
                case _ =>
                  failWith(InvalidArrayIndexType(indexType))
              }
            case Some(t) =>
              failWith(VariableUpdateTypeMismatch(valueType, t))
            case None =>
              failWith(UndefinedSymbol(name))
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

  private def unifyTypeVariables[R: _compilerState : _compilerResult](typeParameters: List[TypeParam], actualTypes: List[ExtendedType], typeDefinitions: List[Type]): Eff[R, Map[SymbolName, Type]] = {
    val typeVariableNames = typeParameters.map(_.name).toSet
    val pairs = actualTypes.zip(typeDefinitions)

    val mappings = pairs.map {
      case (SimpleType(actualType), Types.TypeVariable(name)) if typeVariableNames.contains(name) =>
        Some(name -> actualType)
      case _ =>
        None
    }

    // TODO: error on ambiguous type mappings
    pure[R, Map[SymbolName, Type]](mappings.flatten.toMap)
  }

  private def appliedType(mapping: Map[SymbolName, Type])(typ: Type): Type = {
    typ match {
      case Types.TypeVariable(name) if mapping.contains(name) =>
        mapping(name)
      case Types.Function(typeParams, paramTypes, returnType) =>
        Types.Function(typeParams, paramTypes.map(appliedType(mapping)), appliedType(mapping)(returnType))
      case _ =>
        typ
    }
  }

  private def appliedExtendedType(mapping: Map[SymbolName, Type])(typ: Type): ExtendedType = {
    SimpleType(appliedType(mapping)(typ))
  }
}
