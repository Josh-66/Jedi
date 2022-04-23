package expression

import context.Environment
import context.TypeException
import value.{Boole, Value}

case class Disjunction(operands:List[Expression])  extends SpecialForm{
  override def execute(env: Environment): Value = {
    var res = false
    var remaining = operands
    while (!res && !remaining.isEmpty) {
      remaining.head.execute(env) match {
        case b: Boole if b == Boole.FALSE => {}
        case b: Boole if b == Boole.TRUE => res = true
        case _ => throw TypeException("Non-Boole in disjunction")
      }
      remaining=remaining.tail
    }
    Boole(res)
  }

}
