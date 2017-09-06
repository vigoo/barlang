package io.github.vigoo.bash.language

sealed trait BashStatement
object BashStatements {
  case object Nop extends BashStatement
}
