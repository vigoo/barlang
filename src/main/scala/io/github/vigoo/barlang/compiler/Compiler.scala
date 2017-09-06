package io.github.vigoo.barlang.compiler

import cats.data._
import io.github.vigoo.barlang.language.SingleStatements._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import io.github.vigoo.barlang.language.Statements.{NoOp, Sequence, Single}
import io.github.vigoo.barlang.language._
import io.github.vigoo.bash.language.{BashStatement, BashStatements}
import io.github.vigoo.bash.language.PrettyPrinterInstances._
import io.github.vigoo.prettyprinter.PrettyPrinter

import scala.language.higherKinds

object Compiler {

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

  case class UnknownError(reason: Throwable) extends CompilerError


  case class Context(scope: Scope, symbols: Map[SymbolName, AssignedSymbol], symbolTypes: Map[SymbolName, ExtendedType], lastTmp: Int)


  type CompilerResult[A] = Either[CompilerError, A]
  type StatementCompiler = Fx.fx2[CompilerResult, State[Context, ?]]
  type ExpressionCompiler = Fx.fx3[CompilerResult, State[Context, ?], Writer[BashStatement, ?]]

  def compileToString(script: Script): CompilerResult[String] = {
    val initialContext = Context(
      scope = GlobalScope,
      symbols = Map.empty,
      symbolTypes = Map.empty, // TODO: type of predefined symbols
      lastTmp = 0
    )
    compile(script).runEither.evalState(initialContext).run.map(bashStatement => PrettyPrinter(bashStatement))
  }

  def compile(script: Script): Eff[StatementCompiler, BashStatement] = {
    val replacedBody = replacePredefs(script.body)
    for {
      _ <- typeCheckStatement(replacedBody)
      compiled <- compileStatement(replacedBody)
    } yield compiled
  }

  def replacePredefs(statement: Statement): Statement = {
    statement // TODO
  }

  def storeType(name: SymbolName, typ: ExtendedType): Eff[StatementCompiler, Unit] = {
    modify[StatementCompiler, Context] { context => context.copy(
      symbolTypes = context.symbolTypes + (name -> typ))
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

  def runChildContext[A](context: Context, f: Eff[StatementCompiler, A]): Eff[StatementCompiler, A] = {
    f.runEither.evalState(context).run match {
      case Left(error) =>
        left[StatementCompiler, CompilerError, A](error)
      case Right(result) =>
        right[StatementCompiler, CompilerError, A](result)
    }
  }

  def typeCheckExpression(value: Expression): Eff[StatementCompiler, ExtendedType] = {
    ???
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

          if (bodyType == SimpleType(returnType)) { // TODO: use typeEq
            pure(SimpleType(Types.Unit()))
          } else {
            left[StatementCompiler, CompilerError, ExtendedType](InvalidReturnType(name, returnType, bodyType))
          }
        }
      case Call(function, parameters) =>
        ???
      case Run(command, parameters) =>
        ???
      case If(condition, trueBody, falseBody) =>
        ???
      case While(condition, body) =>
        ???
      case UpdateVariable(name, value) =>
        ???
      case UpdateCell(name, index, value) =>
        ???
      case ArrayDeclaration(name, elementType) =>
        for {
          _ <- storeType(name, SimpleType(Types.Array(elementType)))
        } yield SimpleType(Types.Unit())
      case Return(value) =>
        ???
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
      val result = Compiler.compile(ast)
      println(result)
    case Parser.BarlangParser.NoSuccess(msg, next) =>
      println(s"${next.pos.line}:${next.pos.column} $msg")
      println(src.lines.toVector(next.pos.line - 1))
  }
}