package expression
import context.*
import value.{Boole, Notification, Value}
case class Conditional(condition: Expression,consequence:Expression,alternate:Expression = null) extends SpecialForm{
  override def execute(env: Environment): Value = {
    condition.execute(env) match {
      case b: Boole if b==Boole.TRUE => consequence.execute(env)
      case b: Boole if alternate != null => alternate.execute(env)
      case b: Boole if alternate == null => Notification.UNSPECIFIED
      case _ => throw TypeException("Condition of conditional not Boole")
    }
  }
}
