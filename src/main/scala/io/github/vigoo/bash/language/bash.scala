package io.github.vigoo.bash.language


case class BashIdentifier(name: String)


sealed trait BashStatement
object BashStatements {
  case object Nop extends BashStatement
  case class Assign(target: BashIdentifier, expression: BashExpression) extends BashStatement
  case class Command(name: BashExpression, params: List[BashExpression], hereString: Option[BashExpression] = None) extends BashStatement
  case class IfThenElse(conditional: BashExpression, onTrue: BashStatement, onFalse: BashStatement) extends BashStatement
}


sealed trait BashExpression
object BashExpressions {
  case class Literal(lit: String) extends BashExpression
  case class ReadVariable(variable: BashVariable) extends BashExpression
  case class Eval(statement: BashStatement) extends BashExpression
  case class Conditional(condition: BashCondition) extends BashExpression
  case class Interpolated(parts: List[BashExpression]) extends BashExpression
}


sealed trait BashVariable
object BashVariables {
  case class Variable(name: BashIdentifier) extends BashVariable
}


sealed trait BashCondition
object BashConditions {
  case class Equals(a: BashExpression, b: BashExpression) extends BashCondition
}