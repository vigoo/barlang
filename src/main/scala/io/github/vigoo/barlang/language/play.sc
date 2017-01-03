
import io.github.vigoo.barlang.language._
import io.github.vigoo.barlang.prettyprinter.PrettyPrinter
import io.github.vigoo.barlang.language.PrettyPrinterInstances._

PrettyPrinter("something")

PrettyPrinter(List("a", "b", "c"))

PrettyPrinter(TypeParam(SymbolName("T")))

PrettyPrinter(Types.Unit)

PrettyPrinter(Types.Bool)

PrettyPrinter(Types.Array(Types.Int))

PrettyPrinter(Types.Function(List(), List(Types.String, Types.Int), Types.Unit))

PrettyPrinter(Types.Function(List(TypeParam(SymbolName("T"))), List(Types.String, Types.Int), Types.TypeVariable(SymbolName("T"))))

PrettyPrinter(Expressions.StringLiteral("hello world"))

PrettyPrinter(Expressions.Apply(Expressions.Variable(SymbolName("max")), List(Expressions.DoubleLiteral(11.3), Expressions.BinaryOp(BinaryOperators.Add, Expressions.Variable(SymbolName("x")), Expressions.ArrayAccess(SymbolName("sinTable"), Expressions.IntLiteral(5))))))

PrettyPrinter(SingleStatements.VariableDeclaration(SymbolName("x"), Expressions.StringLiteral("something")))

val code = PrettyPrinter(
  SingleStatements.If(Expressions.UnaryOp(UnaryOperators.Not, Expressions.BoolLiteral(false)),
    Statements.Sequence(
      SingleStatements.Run(Expressions.StringLiteral("echo"), List(Expressions.StringLiteral("hello"), Expressions.StringLiteral("world"))),
      Statements.Single(SingleStatements.Return(Expressions.IntLiteral(-1)))),
    Statements.NoOp
))

Parser.BarlangLexer.parse(Parser.BarlangLexer.tokens, code).get
