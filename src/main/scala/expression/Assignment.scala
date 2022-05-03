package expression
import context.Environment
import value.{Notification, Value, Variable}
import context.TypeException
case class Assignment(id : Identifier, value: Expression) extends SpecialForm{
  override def execute(env: Environment): Value = {
    env(id) match{
      case v:Variable => v.value=value.execute(env)
      case _ => throw TypeException("Assignment can only be done to Variables")
    }
    Notification.DONE
  }



}
