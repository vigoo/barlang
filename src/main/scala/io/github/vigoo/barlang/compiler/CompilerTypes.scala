package io.github.vigoo.barlang.compiler

import cats.data._
import io.github.vigoo.barlang.language._
import io.github.vigoo.bash.language.BashStatement
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

trait CompilerTypes {


  trait Scope {
    def functionScope(name: SymbolName): Scope = FunctionScope(name, this)
    def identifierPrefix: String
  }

  object GlobalScope extends Scope {
    override def identifierPrefix: String = ""
  }
  case class FunctionScope(name: SymbolName, parent: Scope) extends Scope {
    override def identifierPrefix: String = s"${parent.identifierPrefix}${name}_"
  }

  case class AssignedSymbol(name: SymbolName, identifier: String)

  object AssignedSymbol {
    def inScope(scope: Scope, name: SymbolName): AssignedSymbol = {
      AssignedSymbol(name, s"${scope.identifierPrefix}${name.name}")
    }
  }


  sealed trait ExtendedType

  case class SimpleType(typ: Type) extends ExtendedType

  case class FunctionReference(typeParams: List[TypeParam], paramTypes: List[Type], returnType: Type) extends ExtendedType


  sealed trait CompilerError

  case class InvalidReturnType(name: SymbolName, returnType: Type, bodyType: ExtendedType) extends CompilerError

  case class InvalidParameterTypes(expectedTypes: List[Type], actualTypes: List[ExtendedType]) extends CompilerError

  case class InvalidParametersForRunStatement(commandType: ExtendedType, parameterTypes: List[ExtendedType]) extends CompilerError

  case class InvalidConditionTypeForIf(conditionType: ExtendedType) extends CompilerError

  case class InvalidConditionTypeForWhile(conditionType: ExtendedType) extends CompilerError

  case class VariableUpdateTypeMismatch(type1: ExtendedType, type2: ExtendedType) extends CompilerError

  case class InvalidArrayIndexType(indexType: ExtendedType) extends CompilerError

  case class CannotInferType(name: SymbolName) extends CompilerError

  case class NonArrayTypeIndexed(typ: ExtendedType) extends CompilerError

  case class InvalidParameterTypeForPredefined(name: SymbolName, paramTypes: List[ExtendedType]) extends CompilerError

  case class UndefinedSymbol(name: SymbolName) extends CompilerError

  case class SymbolNotBoundToFunction(expression: Expression) extends CompilerError

  case class InvalidBooleanExpression(expression: Option[Expression]) extends CompilerError

  case class InvalidTypesInNumericExpression(types: List[ExtendedType]) extends CompilerError

  case class EqualityUsedOnNonEqualTypes(type1: ExtendedType, type2: ExtendedType) extends CompilerError

  case class UnknownError(reason: Throwable) extends CompilerError


  trait TypeEq[T] {
    def typeEq(a: T, b: T): Boolean
  }

  def typeEq[T: TypeEq](a: T, b: T): Boolean = implicitly[TypeEq[T]].typeEq(a, b)

  implicit def listTypeEq[T: TypeEq]: TypeEq[List[T]] = (a: List[T], b: List[T]) => {
    (a, b) match {
      case (Nil, Nil) => true
      case (Nil, _ :: _) => false
      case (_ :: _, Nil) => false
      case (x :: xs, y :: ys) => typeEq(x, y) && typeEq(xs, ys)
    }
  }
  implicit val typeTypeEq: TypeEq[Type] = (a: Type, b: Type) => a == b
  implicit val typeParamTypeEq: TypeEq[TypeParam] = (a: TypeParam, b: TypeParam) => a == b
  implicit val exendedTypeTypeEq: TypeEq[ExtendedType] = (a: ExtendedType, b: ExtendedType) => {
    (a, b) match {
      case (SimpleType(t1), SimpleType(t2)) =>
        typeEq(t1, t2)
      case (SimpleType(Types.Function(typeParams1, paramTypes1, returnType1)), FunctionReference(typeParams2, paramTypes2, returnType2)) =>
        typeEq(typeParams1, typeParams2) && typeEq(paramTypes1, paramTypes2) && typeEq(returnType1, returnType2)
      case (FunctionReference(typeParams1, paramTypes1, returnType1), SimpleType(Types.Function(typeParams2, paramTypes2, returnType2))) =>
        typeEq(typeParams1, typeParams2) && typeEq(paramTypes1, paramTypes2) && typeEq(returnType1, returnType2)
      case (FunctionReference(typeParams1, paramTypes1, returnType1), FunctionReference(typeParams2, paramTypes2, returnType2)) =>
        typeEq(typeParams1, typeParams2) && typeEq(paramTypes1, paramTypes2) && typeEq(returnType1, returnType2)
      case _ =>
        false
    }
  }

  implicit class InfixTypeEqOp[T : TypeEq](value: T) {
    def =:=(other: T): Boolean =
      typeEq(value, other)
  }

  case class Context(scope: Scope, symbols: Map[SymbolName, AssignedSymbol], symbolTypes: Map[SymbolName, ExtendedType], lastTmp: Int)


  type CompilerResult[A] = Either[CompilerError, A]
  type StatementCompiler = Fx.fx2[CompilerResult, State[Context, ?]]
  type ExpressionCompiler = Fx.fx3[CompilerResult, State[Context, ?], Writer[Eff[StatementCompiler, BashStatement], ?]]


  case class TypedExpression(typ: ExtendedType, expression: Expression)


  def storeType(name: SymbolName, typ: ExtendedType): Eff[StatementCompiler, Unit] = {
    modify[StatementCompiler, Context] { context => context.copy(
      symbolTypes = context.symbolTypes + (name -> typ))
    }
  }

  def cloneContext(): Eff[StatementCompiler, Context] =
    get[StatementCompiler, Context]


  def runChildContext[A](context: Context, f: Eff[StatementCompiler, A]): Eff[StatementCompiler, A] = {
    f.runEither.evalState(context).run match {
      case Left(error) =>
        left[StatementCompiler, CompilerError, A](error)
      case Right(result) =>
        right[StatementCompiler, CompilerError, A](result)
    }
  }

  def findSymbol[R](name: SymbolName)(implicit member: State[Context, ?] |= R): Eff[R, Option[AssignedSymbol]] =
    for {
      context <- get[R, Context]
    } yield context.symbols.get(name)

  def findType[R](name: SymbolName)(implicit member: State[Context, ?] |= R): Eff[R, Option[ExtendedType]] =
    for {
      context <- get[R, Context]
    } yield context.symbolTypes.get(name)


  def generateTempSymbol[R](implicit member: State[Context, ?] |= R): Eff[R, AssignedSymbol] = {
    for {
      context <- get[R, Context]
      next = context.lastTmp + 1
      idString = s"_${context.scope.identifierPrefix}_tmp$next"
      _ <- put[R, Context](context.copy(lastTmp = next))
    } yield AssignedSymbol(SymbolName(s"tmp$next"), idString)
  }

  def prerequisite(statement: Eff[StatementCompiler, BashStatement]): Eff[ExpressionCompiler, Unit] =
    tell[ExpressionCompiler, Eff[StatementCompiler, BashStatement]](statement)
}
