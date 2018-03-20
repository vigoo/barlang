package io.github.vigoo.bash.language


case class BashIdentifier(name: String)


sealed trait BashStatement {
  def flatten: List[BashStatement] = List(this)
  def normalize: BashStatement = this
}
object BashStatements {
  case object Nop extends BashStatement
  case class Assign(target: BashIdentifier, expression: BashExpression) extends BashStatement
  case class Command(name: BashExpression, params: List[BashExpression], hereString: Option[BashExpression] = None) extends BashStatement
  case class IfThenElse(conditional: BashExpression, onTrue: BashStatement, onFalse: BashStatement) extends BashStatement
  case class Declare(options: Set[BashDeclareOption], name: BashIdentifier, initialValue: Option[BashExpression]) extends BashStatement
  case class Sequence(statements: List[BashStatement]) extends BashStatement {
    override def flatten: List[BashStatement] =
      statements.flatMap(_.flatten)

    override def normalize: BashStatement =
      flatten match {
        case Nil => Nop
        case List(statement) => statement
        case ss: List[BashStatement] => Sequence(ss)
      }
  }
}


sealed trait BashExpression
object BashExpressions {
  case class Literal(lit: String) extends BashExpression
  case class ReadVariable(variable: BashVariable) extends BashExpression
  case class ReadArray(variable: BashVariable, index: BashArrayIndex) extends BashExpression
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

sealed trait BashArrayIndex
object BashArrayIndices {
  case class Index(index: BashExpression) extends BashArrayIndex
  case object All extends BashArrayIndex
}

sealed trait BashDeclareOption
case object BashDeclareOptions {
  case object Array extends BashDeclareOption
  case object ReadOnly extends BashDeclareOption
}
