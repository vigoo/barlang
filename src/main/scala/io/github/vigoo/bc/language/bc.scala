package io.github.vigoo.bc.language

import io.github.vigoo.bash.language.BashIdentifier


sealed trait BcExpression

object BcExpressions {
  final case class Number(value: Double) extends BcExpression
  final case class BashVariable(identifier: BashIdentifier) extends BcExpression
  final case class FunctionCall(name: String, parameter: BcExpression) extends BcExpression
  final case class Add(x: BcExpression, y: BcExpression) extends BcExpression
  final case class Sub(x: BcExpression, y: BcExpression) extends BcExpression
  final case class Mul(x: BcExpression, y: BcExpression) extends BcExpression
  final case class Div(x: BcExpression, y: BcExpression) extends BcExpression
}
