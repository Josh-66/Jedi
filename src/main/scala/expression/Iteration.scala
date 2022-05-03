package expression
import context.Environment
import value.{Boole, Notification, Value}

case class Iteration(cond: Expression, body:Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    while(cond.execute(env) == Boole.TRUE){
      body.execute(env)
    }
    if (cond.execute(env)!=Boole.FALSE)
      throw context.TypeException("Condition in iteration is not a Boole")
    Notification.DONE
  }
}
