package io.github.vigoo.bash.language

import io.github.vigoo.PrettyPrinterTests
import io.github.vigoo.bash.language.BashConditions._
import io.github.vigoo.bash.language.BashExpressions._
import io.github.vigoo.bash.language.BashPrettyPrint._
import io.github.vigoo.bash.language.BashStatements._
import io.github.vigoo.bash.language.BashVariables._
import org.specs2.Specification

class BashPrettyPrinterSpecs extends Specification with PrettyPrinterTests[BashPrettyPrint.BashFx, BashPrettyPrint.type] { def is = s2"""
    The bash condition pretty printer should
      print equality correctly       $prettyPrintConditionEq

    The bash variable pretty printer should
      print simple variables correctly    $prettyPrintVariable

    The bash identifier pretty printer should
      print identifiers correctly         $prettyPrintIdentifier

    The bash expression pretty printer should
      print literals correctly            $prettyPrintExpressionLiteral
      print read variable correctly       $prettyPrintExpressionReadVariable
      print read whole array correctly    $prettyPrintExpressionReadArrayWhole
      print read array correctly          $prettyPrintExpressionReadArray
      print eval correctly                $prettyPrintExpressionEval
      print conditional correctly         $prettyPrintExpressionConditional
      print interpolated string correctly $prettyPrintExpressionInterpolated
      print eval arithmetic correctly     $prettyPrintExpressionEvalArithmetic

    The bash statement pretty printer should
      print assignment correctly          $prettyPrintStatementAssignment
      print command execution correctly   $prettyPrintStatementCommand
      print if-then-else correctly        $prettyPrintStatementIfThenElse
      print sequence correctly            $prettyPrintStatementSequence
      print declare correctly             $prettyPrintStatementDeclare
      print let correctly                 $prettyPrintStatementLet
    """

  override val pp = BashPrettyPrint

  def prettyPrintConditionEq = {
    (Equals(ReadVariable(Variable(BashIdentifier("TEST"))), Literal("something")) should bePrintedAs("""${TEST} == something""")) and
      (Equals(ReadVariable(Variable(BashIdentifier("TEST"))), Literal("something longer")) should bePrintedAs("""${TEST} == "something longer""""))
  }

  def prettyPrintVariable = {
    (Variable(BashIdentifier("TEST_VARIABLE")) should bePrintedAs("TEST_VARIABLE")) and
      (Variable(BashIdentifier("X")) should bePrintedAs("X"))
  }

  def prettyPrintIdentifier = {
    BashIdentifier("TEST_VARIABLE") should bePrintedAs("TEST_VARIABLE")
  }

  def prettyPrintExpressionLiteral = {
    (Literal("something") should bePrintedAs("something")) and
      (Literal("something with whitespace") should bePrintedAs("\"something with whitespace\""))
  }

  def prettyPrintExpressionReadVariable = {
    (ReadVariable(Variable(BashIdentifier("XYZ"))) should bePrintedAs("${XYZ}")) and
      (ReadVariable(Variable(BashIdentifier("X"))) should bePrintedAs("$X"))
  }

  def prettyPrintExpressionReadArrayWhole = {
    ReadArray(Variable(BashIdentifier("LST")), BashArrayIndices.All) should bePrintedAs("${LST[*]}")
  }

  def prettyPrintExpressionReadArray = {
    (ReadArray(Variable(BashIdentifier("LST")), BashArrayIndices.Index(Literal("5"))) should bePrintedAs("${LST[5]}")) and
      (ReadArray(Variable(BashIdentifier("LST")), BashArrayIndices.Index(ReadVariable(Variable(BashIdentifier("IDX"))))) should bePrintedAs("${LST[${IDX}]}"))
  }

  def prettyPrintExpressionEval = {
    (Eval(Command(Literal("test"), List.empty)) should bePrintedAs("$(test)")) and
      (Eval(Command(Literal("test"), List(Literal("x"), Literal("something else"), Literal("y")))) should bePrintedAs("$(test x \"something else\" y)"))
  }

  def prettyPrintExpressionConditional = {
    Conditional(Equals(ReadVariable(Variable(BashIdentifier("TEST"))), Literal("something"))) should bePrintedAs("[[ ${TEST} == something ]]")
  }

  def prettyPrintExpressionInterpolated = {
    (Interpolated(List(Literal("something"))) should bePrintedAs("\"something\"")) and
      (Interpolated(List(Literal("something with whitespace"))) should bePrintedAs("\"something with whitespace\"")) and
      (Interpolated(List(Literal("something "), Literal("with whitespace"))) should bePrintedAs("\"something with whitespace\"")) and
      (Interpolated(List(ReadVariable(Variable(BashIdentifier("TEST"))))) should bePrintedAs("\"${TEST}\"")) and
      (Interpolated(List(Literal("some"), ReadVariable(Variable(BashIdentifier("TEST"))))) should bePrintedAs("\"some${TEST}\"")) and
      (Interpolated(List(Literal("something which is a "), ReadVariable(Variable(BashIdentifier("TEST"))))) should bePrintedAs("\"something which is a ${TEST}\""))
  }

  def prettyPrintExpressionEvalArithmetic =
    EvalArithmetic(
      BashArithmeticExpressions.Div(
        BashArithmeticExpressions.Add(BashArithmeticExpressions.Variable(Variable(BashIdentifier("X"))), BashArithmeticExpressions.Number(1)),
        BashArithmeticExpressions.Number(2))) should bePrintedAs("$(( ($X + 1) / 2 ))")

  def prettyPrintStatementAssignment = {
    (Assign(BashIdentifier("X"), Literal("test")) should bePrintedAs("X=test")) and
      (Assign(BashIdentifier("Y"), ReadVariable(Variable(BashIdentifier("X")))) should bePrintedAs("Y=$X"))
  }

  def prettyPrintStatementCommand = {
    (Command(Literal("echo"), List(Literal("Hello world"))) should bePrintedAs("echo \"Hello world\"")) and
      (Command(ReadVariable(Variable(BashIdentifier("AWS"))), List(Literal("describe-instance"), Literal("i-test"))) should bePrintedAs("${AWS} describe-instance i-test")) and
      (Command(Literal("bc"), List(Literal("-l")), hereString = Some(Literal("5+5"))) should bePrintedAs("bc -l <<< 5+5"))
  }

  def prettyPrintStatementIfThenElse = {
    val statement =
      IfThenElse(
        conditional = Conditional(Equals(ReadVariable(Variable(BashIdentifier("TEST"))), Literal("something"))),
        onTrue = Command(Literal("echo"), List(Literal("TEST is something"))),
        onFalse = Command(Literal("echo"), List(Interpolated(List(Literal("TEST is not something but "), ReadVariable(Variable(BashIdentifier("TEST"))))))))

    statement should bePrintedAs("if [[ ${TEST} == something ]]\nthen\n    echo \"TEST is something\"\nelse\n    echo \"TEST is not something but ${TEST}\"\nfi")
  }

  def prettyPrintStatementDeclare = {
    (Declare(Set(BashDeclareOptions.Array), BashIdentifier("LST"), None) should bePrintedAs("declare -a LST")) and
      (Declare(Set(BashDeclareOptions.Array, BashDeclareOptions.ReadOnly), BashIdentifier("LST"), None) should bePrintedAs("declare -a -r LST")) and
      (Declare(Set(BashDeclareOptions.Array), BashIdentifier("LST"), Some(ReadVariable(Variable(BashIdentifier("TMP"))))) should bePrintedAs("declare -a LST=${TMP}"))
  }

  def prettyPrintStatementLet = {
    Let(List(BashArithmeticExpressions.AssignRem(
      Variable(BashIdentifier("TEST")),
      BashArithmeticExpressions.Exponentiation(
        BashArithmeticExpressions.Number(2), BashArithmeticExpressions.Number(3)
      )))) should bePrintedAs("let \"TEST %= (2 ** 3)\"")
  }

  def prettyPrintStatementSequence = {
    val statement =
      Sequence(List(
        Command(Literal("echo"), List(Literal("Hello world"))),
        Command(ReadVariable(Variable(BashIdentifier("AWS"))), List(Literal("describe-instance"), Literal("i-test"))),
        Command(Literal("bc"), List(Literal("-l")), hereString = Some(Literal("5+5")))
      ))

    statement should bePrintedAs(
      """echo "Hello world"
        |${AWS} describe-instance i-test
        |bc -l <<< 5+5""".stripMargin)
  }
}
