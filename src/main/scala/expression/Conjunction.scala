package expression
import context.Environment
import value.Value
import value.Boole
import context.TypeException

case class Conjunction(operands:List[Expression]) extends SpecialForm{
  override def execute(env: Environment): Value = {
    var res = true
    var remaining = operands
    while (res && !remaining.isEmpty) {
      remaining.head.execute(env) match {
        case b: Boole if b == Boole.TRUE => {}
        case b: Boole if b == Boole.FALSE => res = false
        case _ => throw TypeException("Non-Boole in conjunction")
      }
      remaining=remaining.tail
    }
    Boole(res)
  }
}
