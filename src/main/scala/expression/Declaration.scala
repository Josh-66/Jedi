package expression

import context.Environment
import value.{Boole, Notification, Value}

case class Declaration(identifier: Identifier,expression: Expression) extends SpecialForm{
  override def execute(env: Environment): Value = {
    env.addOne(identifier,expression.execute(env))
    Notification.OK
  }
}
