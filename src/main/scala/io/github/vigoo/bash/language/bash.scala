package io.github.vigoo.bash.language


case class BashIdentifier(name: String)


sealed trait BashStatement
object BashStatements {
  case object Nop extends BashStatement
  case class Assign(target: BashIdentifier, expression: BashExpression) extends BashStatement
  case class Command(name: BashExpression, params: List[BashExpression]) extends BashStatement
}


sealed trait BashExpression
object BashExpressions {
  case class Literal(lit: String) extends BashExpression
  case class ReadVariable(variable: BashVariable) extends BashExpression
  case class Eval(statement: BashStatement) extends BashExpression
}


sealed trait BashVariable
object BashVariables {
  case class Variable(name: BashIdentifier) extends BashVariable
}


